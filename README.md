# ocp-ppx-import
A ppx to simplify the usage of module aliases wrappers in OCaml

This ppx adds an extension point for structures (not signatures) to import multiple submodules from a given module. It is especially useful when using module aliases to simulate namespaces.

The syntax is simple:
```ocaml
[%%import MyModule.(Mod1, Mod2, Mod3 => Mod3Alias)]
```
This simple piece of code will generate the following code:
```ocaml
module Mod1 = MyModule.Mod1
module Mod2 = MyModule.Mod2
module Mod3Alias = MyModule.Mod3
```

It can be used in combination with [ocp-nsgen](https://github.com/OCamlPro-Couderc/ocp-nsgen) to use external libraries as namespaces. 

## TODO:
* Allow to import all modules
* Allow imports in signature; drawback: every alias generated is exported.
* Installation
* More?
