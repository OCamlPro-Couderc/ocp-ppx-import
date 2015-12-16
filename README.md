# ocp-ppx-namespace
A ppx to simplify the usage of module aliases wrappers in OCaml, and allowing a
form of `selective open` and `selective include`.

This ppx adds an extension point for structures (not signatures) to import multiple submodules from a given module. It is especially useful when using module aliases to simulate namespaces.

The syntax is simple:
```ocaml
(* myfile.ml *)
[%%import MyModule.(Mod1, Mod2, Mod3 => Mod3Alias)]
```
This simple piece of code will generate the following code:
```ocaml
module Namespace-Myfile-Mymodule-1 = struct
  module Mod1 = MyModule.Mod1
  module Mod2 = MyModule.Mod2
  module Mod3Alias = MyModule.Mod3
end
open Namespace-Myfile-MyModule-1
```

Of course, it is not a valid module identifier, to avoid
clashes with other modules of the user. The ```1``` at the end of the module
name is a unique stamp, to allow reimporting from an already used module.

It can be used in combination with
[ocp-nsgen](https://github.com/OCamlPro-Couderc/ocp-nsgen) to use external
libraries as namespaces.

The following syntax is also available to import a unique module:
```ocaml
[%%import MyModule.M1]
```

## Selective include

Using the directive ```include``` instead of ```import``` generates:
```ocaml
(* myfile.ml *)
[%%include MyModule.Mod1]
```

```ocaml
module Namespace-Myfile-Mymodule-1 = struct
  module Mod1 = MyModule.Mod1
end
include Namespace-Myfile-MyModule-1
```

## Value import

Values can also be imported from a module:
```ocaml
[%%import MyModule.(my_function, my_value => v)]
```

Of courses, modules and values can be imported in the same import directive.

## Types and module types import

Using the attribute ```[@type]``` on each
imported element (or globally) results in importing a type with the qualified
name instead of a value (and reciprocally with moduel types instead of module).

However, it will need to read the signature of the module to generate the
correct manifest and kind for type declarations, i.e.:
```ocaml
(* myfile.ml *)
[%%import Pervasives.(option [@type])]
```
must be translated as:
```ocaml
module Namespace-Pervasives-Myfile-1 = struct
  type 'a option = 'a Pervasives.option = Some 'a | None
end
open Namespace-Pervasives-Myfile-1
```

Otherwise, if it simply abbreviated (without writing the complete definition),
the constructors Some and None won't be in the scope of the current module. The
`cmi` files are necessary.

## Type extensions and exceptions

Not implemented yet.

## Classes and class types

Not implemented yet.

## Namespace declaration

The namespace declaration is also usable:
```ocaml
[%%namespace MyModule]
```
which is simply an ```open MyModule```. It simply avoids dependencies with
ocamldep and module aliases: it won't
generate a dependency to MyModule in this specific case (useful when MyModule
has been compiled with ```-no-alias-deps``` and is a wrapper to simulate a namespace).

## TODO:
* Allow imports in signature, which is not that relevant: the generated
  signature will contain this specific generated module which is
  unusable. Moreover, types can be prefixed with the correct module.
* Installation
