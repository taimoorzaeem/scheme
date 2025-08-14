# Scheme

A toy scheme interpreter written entirely in haskell. The code used in this is taken from the book [Write Yourself a Scheme in 48 Hours](https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf). 

Although the book does a really great job in writing an up and running scheme interpreter, the book contains many bad haskell practices like use of partial functions and incomplete patterns etc. I was shocked when I saw `[x] ++ xs`. To see all the bad practices, compile with `-Wall` and/or use `hlint` and you'll get what I mean.

**To build:**

```sh
$ ghc Scheme.hs
```

**Run:**

```sh
$ ./Scheme
```
