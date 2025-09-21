-- Step-Scheme (WIP)
-- ===========
--   This is one-step scheme evaluator, which prints the evaluation
--   of a scheme expression after every step. If the expression cannot
--   be evaluated further i.e. it has reached its Normal Form the
--   evaluation stops at it's normal form even if the resultant expression
--   doesn't make sense.
-- TODO: Add regression tests.
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-do-bind    #-}
module Language.Scheme.StepScheme
  (runRepl)
  where

import Data.Either (rights)
import Text.ParserCombinators.Parsec hiding (spaces)

import System.IO
import Prelude

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"


readOrThrow :: Parser a -> String -> Either LispError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> Left $ Parser err
    Right val -> return val


readExpr :: String -> Either LispError LispVal
readExpr = readOrThrow parseExpr


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return (case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom)


parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ (Number . read) x


parseList :: Parser LispVal
parseList = do
  x <- sepBy parseExpr spaces
  return $ List x


parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseAtom -- used try for backtracking
         <|> try parseString
         <|> try parseNumber
         <|> try parseQuoted
         <|> do char '('
                x <- try parseList
                char ')'
                return x

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


instance Show LispVal where show = showVal

-- | This is One Step Evaluation.
-- TODO: Only If-expressions are evaluation using one step.
--       Next, convert the function definitions and applications to one-step.
-- Example evaluation:
-- ==================
--   (if (if #t #f #t) 2 3)  -->  (if #f 2 3)
--   (if (if #f 'pie 'waffle) 2 3)  -->  (if (quote waffle) 2 3)   <- (Reached its Normal Form)
eval :: LispVal -> Either LispError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id') = return (Atom id')
eval (List [Atom "quote", val]) = return $ List [Atom "quote", val]
eval (List [Atom "if", pred', conseq, alt]) = do
    case pred' of
      Bool True -> return conseq
      Bool False -> return alt
      pred'' -> do
        pred''' <- eval pred''
        return (List [Atom "if", pred''', conseq, alt])
-- TODO: These look horrible, refactor all primitive operations into separate
--       functions
eval (List (Atom "+":args)) =
  if not (all isNumber args) then
    return $ List (Atom "+" : rights (map eval args))
  else
    return $ sumNumbers args
  where
    sumNumbers :: [LispVal] -> LispVal
    sumNumbers nums = Number $ sum [ x | (Number x) <- nums]
eval (List (Atom "*":args)) = do
  if not (all isNumber args) then
    return $ List (Atom "+" : rights (map eval args))
  else
    return $ productNumbers args
  where
    productNumbers :: [LispVal] -> LispVal
    productNumbers nums = Number $ product [ x | (Number x) <- nums]

eval badForm = Left $ Default $ "evaluation error: " <> show badForm

data LispError = Parser ParseError
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = err

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr =
  case readExpr expr >>= eval of 
    Right v -> return $ show v
    Left _  -> return expr


-- | Evaluate scheme expression until it reaches its normal form.
-- Example:
-- =======
-- >>> (if (if #t #f #f) 10 20)  -->  (if #f 10 20)  -->  20
evalAndPrint :: String -> IO ()
evalAndPrint = evalLoop
  where
    evalLoop :: String -> IO ()
    evalLoop expr' = do
      res <- evalString expr'
      putStrLn $ "> " <> res
      nextRes <- evalString res
      if res == nextRes
        then return ()
        else do
          evalLoop res


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
  result <- prompt
  if pred' result
    then return ()
    else action result >> until_ pred' prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
