api-diff only accepts arguments of the same nature, that is it either
diff a .cmi file with another .cmi file or a directory with a directory

  $ mkdir test
  $ touch test.cmi
  $ api-diff test test.cmi
  api-diff: Inconsistent arguments. Either --main-module, --unwrapped-library or two single $(b,.cmi) files should be provided.
  [123]

When diffing all libraries, the --main-module argument is mandatory

  $ mkdir test2
  $ api-diff test test2
  api-diff: Inconsistent arguments. Either --main-module, --unwrapped-library or two single $(b,.cmi) files should be provided.
  [123]

When passing --main-module while diffing single .cmi files, the user will be warn
that it is ignored

  $ touch test2.cmi
  $ api-diff --main-module main test.cmi test2.cmi
  api-diff: Inconsistent arguments. Either --main-module, --unwrapped-library or two single $(b,.cmi) files should be provided.
  api-diff: Cmi_format.Error(_)
  [123]
