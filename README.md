# ocp-ppx-namespace
A ppx to simplify the usage of module aliases wrappers in OCaml

This ppx adds an extension point for structures (not signatures) to import multiple submodules from a given module. It is especially useful when using module aliases to simulate namespaces.

The syntax is simple:
```ocaml
[%%import MyModule.(Mod1, Mod2, Mod3 => Mod3Alias)]
```
This simple piece of code will generate the following code:
```ocaml
module *namespace*-MyModule = struct
  module Mod1 = MyModule.Mod1
  module Mod2 = MyModule.Mod2
  module Mod3Alias = MyModule.Mod3
end
open *namespace*-MyModule
```

Of course, it is not a valid module identifier, which is expected to avoid
clashes with other modules.

It can be used in combination with
[ocp-nsgen](https://github.com/OCamlPro-Couderc/ocp-nsgen) to use external
libraries as namespaces.

The following syntax is also available to import a unique module:
```ocaml
[%%import MyModule.M1]
```

## Value import

Values can also be imported from a module:
```ocaml
[%%import MyModule.(my_function, my_value => v)]
```

Of courses, modules and values can be imported in the same import directive.

## Types and module types import

Not implemented yet.

Idea: using the exact same syntax, adding the attribute ```[@type]``` on each
imported element (or globally) results in importing a type with the qualified
name instead of a value (and reciprocally with moduel types instead of module).

However, it will need to read the signature of the module to generate the
correct manifest and kind for type declarations, i.e.:
```ocaml
[%%import Pervasives.(option [@type])]
```
must be translated as:
```
module *namespace*-Pervasives = struct
  type 'a option = 'a Pervasives.option = Some 'a | None
end
open *namespace*-Pervasives 
```

Otherwise, if it simply abbreviated (without writing the complete definition),
the constructors Some and None won't be in the scope of the current module.
[ppx_import](https://github.com/whitequark/ppx_import) does the job actually.

## Classes and class types

Not supported yet.

## Namespace declaration

The namespace declaration is also usable:
```ocaml
[%%namespace MyModule]
```
which is simply an opening. The idea is using it with ocamldep: it won't
generate a dependency to MyModule in this specific case (useful when MyModule
has been compiled with ```-no-alias-deps```).

## TODO:
* Allow to import all modules
* Allow imports in signature, which is not that relevant: the generated
  signature will contain this specific generated module which is
  unusable. Moreover, types can be prefixed with the correct module.
* Installation
* A way to import values which is simple, and possibly types and classes (more
  complicated), and possibly module types.
