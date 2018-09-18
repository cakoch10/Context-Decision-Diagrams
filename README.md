# Context-Decision-Diagrams
This repo contains code for an Ocaml implementation of context decision diagrams.

Example usage in utop:

```
#require "core"
#use "cdd.ml"
module D = struct let dimension = 2 end
module A = ArrayVector(D)
module C = Make(A)
#install_printer C.format
#install_printer A.format
let blank = A.init 2 (fun i -> C.Unknown)
let l1 = C.leaf blank
let l2 = C.leaf blank
let tree = C.ite 1 l1 l2;;
```

