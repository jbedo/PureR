PureR is an R backend for PureScript. It is a forked version of
[PureNix](https://github.com/purenix-org/purenix). Use it after generating
corefn with `purs`.

# Example

Consider
``` purescript
module Main where

greeting :: String
greeting = "hello purescript!"

foreign import rgreet :: String
```

R lacks a module system and encourages a workflow where source files are present
in the current working directory. To implement a module system, each transcoded
purescript module is placed in an independent file (`default.R`) which returns a
list of exported content. The above example can be loaded in R with

``` R
Main <- source("./output/Main/default.R", chdir=T)$value
```

The `chdir=T` is important here as other modules are loaded relative to the
location of the source file. The greeting is then available as a list element
`Main$greeting`. Foreign function interfaces are similar, they are lists with elements
matching the foreign function name. A suitable implementation of the above is thus

``` R
list(rgreet = "hello from R!")
```

# Links

- [purer-prelude](https://github.com/jbedo/purer-prelude): A port of [purescript-prelude](https://github.com/purescript/purescript-prelude)
- [PureNix](https://github.com/purenix-org/purenix): A Nix backend for
  purescript; the base and inspiration for this.
