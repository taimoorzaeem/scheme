-- Step-Scheme (WIP)
-- ===========
--   This is one-step scheme evaluator, which prints the evaluation
--   of a scheme expression after every step. If the expression cannot
--   be evaluated further i.e. it has reached its Normal Form the
--   evaluation stops at it's normal form even if the resultant expression
--   doesn't make sense.
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-do-bind    #-}
module Main where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe (isJust, isNothing)
import Data.IORef
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = getArgs >>= \case
  []   -> runRepl
  args -> runOne args


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"


readOrThrow :: Parser a -> String -> Either LispError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> Left $ Parser err
    Right val -> return val


readExpr :: String -> Either LispError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> Either LispError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> Either LispError LispVal)
             | Func { params :: [String],
                      vararg :: Maybe String,
                      body :: [LispVal],
                      closure :: Env }
             | IOFunc ([LispVal] -> ExceptT LispError IO LispVal)
             | Port Handle


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


parseDottedList :: Parser LispVal
parseDottedList = do
  head' <- endBy parseExpr spaces
  tail' <- char '.' >> spaces >> parseExpr
  return $ DottedList head' tail'


parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ "." ++ showVal tail' ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = args, vararg = varargs }) =
          "(lambda (" ++ unwords (map show args) ++
                  (case varargs of
                      Nothing -> ""
                      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


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
eval :: Env -> LispVal -> ExceptT LispError IO LispVal
eval _   val@(String _) = return val
eval _   val@(Number _) = return val
eval _   val@(Bool _) = return val
eval env (Atom id') = getVar env id'
eval _   (List [Atom "quote", val]) = return $ List [Atom "quote", val]
eval env (List [Atom "if", pred', conseq, alt]) = do
    case pred' of
      Bool True -> return conseq
      Bool False -> return alt
      pred'' -> do
        pred''' <- eval env pred''
        return (List [Atom "if", pred''', conseq, alt])
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= fmap last <$> mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm

-- | Apply args to function. This is Applicative Order Evaluation.
apply :: LispVal -> [LispVal] -> ExceptT LispError IO LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwE $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>=
        bindVarArgs varargs >>= evalBody
      where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply _ _ = throwE $ Default youGotMeError


primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


boolBinop :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
boolBinop unpacker op args = if length args /= 2
                             then Left $ NumArgs 2 args
                             else do
                                left <- unpacker $ head args -- index 0
                                right <- unpacker $ head $ tail args -- index 1
                                return $ Bool $ left `op` right


numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> Either LispError LispVal
numBoolBinop = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> Either LispError LispVal
strBoolBinop = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> Either LispError LispVal
boolBoolBinop = boolBinop unpackBool


unpackStr :: LispVal -> Either LispError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = Left $ TypeMismatch "string" notString


unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


car :: [LispVal] -> Either LispError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> Either LispError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> Either LispError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> Either LispError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
  return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where
      eqvPair (x1, x2) = case eqv [x1, x2] of
                          Right (Bool val) -> val
                          _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


-- From ExistentialQuantification language extension
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Either LispError a)


unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` const (return False)


equal :: [LispVal] -> Either LispError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- or <$> (mapM $ unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]

    if primitiveEquals then
      return $ Bool primitiveEquals
    else do
      eqvEquals <- eqv [arg1, arg2]

      case eqvEquals of
        Bool x -> Right $ Bool x
        _      -> Left $ Default youGotMeError

equal badArgList = throwError $ NumArgs 2 badArgList



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                      " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = err

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- TODO: Partial Function? This is horrible.
extractValue :: Either LispError a -> a
extractValue (Right val) = val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)


-- | Evaluate scheme expression until it reaches its normal form.
-- Example:
-- =======
-- >>> (if (if #t #f #f) 10 20)  -->  (if #f 10 20)  -->  20
-- >>> (if (if #f
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalLoop env expr
  where
    evalLoop :: Env -> String -> IO ()
    evalLoop env expr = do
      res <- evalString env expr
      putStrLn $ "> " <> res
      nextRes <- evalString env res
      if res == nextRes
        then return ()
        else do
          evalLoop env res


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
  result <- prompt
  if pred' result
    then return ()
    else action result >> until_ pred' prompt action


runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc' IOFunc) ioPrimitives ++ map (makeFunc' PrimitiveFunc) primitives)
  where
    makeFunc' constructor (var, func) = (var, constructor func)


liftThrows :: Either LispError a -> ExceptT LispError IO a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: ExceptT LispError IO String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var


getVar :: Env -> String -> ExceptT LispError IO LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)


setVar :: Env -> String -> LispVal -> ExceptT LispError IO LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value


defineVar :: Env -> String -> LispVal -> ExceptT LispError IO LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv >>= newIORef
    where
      extendEnv env = fmap (++ env) (mapM addBinding bindings)

      addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)


makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal


ioPrimitives :: [(String, [LispVal] -> ExceptT LispError IO LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]


applyProc :: [LispVal] -> ExceptT LispError IO LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc _ = throwE $ Default youGotMeError


makePort :: IOMode -> [LispVal] -> ExceptT LispError IO LispVal
makePort mode [String filename] = Port <$> liftIO (openFile filename mode)
makePort _ _ = throwE $ Default youGotMeError


closePort :: [LispVal] -> ExceptT LispError IO LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False


readProc :: [LispVal] -> ExceptT LispError IO LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc _ = throwE $ Default youGotMeError


writeProc :: [LispVal] -> ExceptT LispError IO LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc _ = throwE $ Default youGotMeError


readContents :: [LispVal] -> ExceptT LispError IO LispVal
readContents [String filename] = String <$> liftIO (readFile filename)
readContents _ = throwE $ Default youGotMeError


load :: String -> ExceptT LispError IO [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList


readAll :: [LispVal] -> ExceptT LispError IO LispVal
readAll [String filename] = List <$> load filename
readAll _ = throwE $ Default youGotMeError


youGotMeError :: String
youGotMeError = "Whoops! You caught me not handling every case. This means that you are good at program testing."
