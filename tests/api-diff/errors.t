api-diff only accepts arguments of the same nature, that is it either
diff a .cmi file with another .cmi file or a directory with a directory

  $ mkdir test
  $ touch test.cmi
  $ api-diff test test.cmi
  api-watcher: Arguments must either both be directories or both single .cmi files.
  [123]

When diffing all libraries, the --main-module argument is mandatory

  $ mkdir test2
  $ api-diff test test2
  api-watcher: --main-module must be provided when diffing entire libraries.
  [123]
