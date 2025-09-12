# step-scheme

A one step scheme evaluator. Try evaluating a scheme expression, and this should print the scheme expression at each step of evaluation up to its **normal form**. I stole the code from [Write Yourself a Scheme in 48 Hours](https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf) and reduced it to a basic version.

This is WIP. Currently it works something like:

```scheme
(if (if (if #t #f #t) #t #f) 2 3)
(if (if #f #t #f) 2 3)
(if #f 2 3)
3 ;; Normal Form
```

The goals is to take it beyond the just the `if` expressions to functions as well as other forms like `cond`, `lambda` etc.

**To build:**

```sh
$ cabal v2-build
```

**Run:**

```sh
$ rlwrap cabal v2-run    # "rlwrap" for easier retrying, otherwise not needed
```
