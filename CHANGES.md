## unreleased

### Added

- Add detection of type declarations changes (#92, @azzsal)
- Add detection of module_type declarations changes (#93, @NchamJosephMuam)
- Add detection of classes addition and removal (#90, @marcndo)
- Add detection of addition and removal of class type declarations (#103, @azzsal)
- Add initial support for unwrapped libraries (#107, @Siddhi-agg, @azzsal)
- Add detection of modified class declarations and class types (#106, @azzsal)
- Add word-level display of textual diffs in `api-diff`, enabled with the
  `--word-diff` flag (#131, #136, @azzsal)
- Add `--plain` flag to `api-diff` to use text markers for inline highlighting.
  Can be used when the output doesn't support colors (#136, @azzsal)
- Add fine-grained diff of tuple types (#139, @azzsal)
- Add fine-grained diff of arrow types (#140, @azzsal)
- Add fine-grained diff of type constructors (#148, @azzsal)

### Changed

  - Improve diff representation of modified record types (#109, @azzsal)
  - Improve diff representation of modified variant types (#111, @azzsal)
  - Improve the diff representation of type declarations with more fine grained diffing of
    type kind, type privacy and type manifest (#120, @azzsal)
  - Improve the diff representation of type declarations to have type parameters diff (#113,@azzsal)
  - Improve the textual diff representation output to have highlighting of exact
    changes in a line (#126,@azzsal)
  - Improve handling of type equalities across the reference and current
    versions of the interface. (#134, @azzsal)
  - Initialize the typing enviorment with the standard library

### Deprecated

### Fixed

- Ignore hidden signature items (#102, @NchamJosephMuam)
- Remove duplicate items in class and class types (#105, @azzsal)
- Fixed loading of modules whose signature is given by a path to a module type:
  `module X : Y` (#128, @panglesd)
- Fixed initialization of the typing enviorment (#134, @azzsal)
- Fix a bug the was causing the tool to stack overflow when dealing with
  some instances of parametrized types (#134, @azzsal)
- Fix a bug where the loader couldn't find module types defined in the same
    compilation unit. (#151, @NathanReb, @azzsal)
- Fix expansion of type constructors (#153, @azzsal)

### Removed

### Security

## 0.1.1

### Fixed

- Remove dependency on unreleased `diffutils` package
  (#88, #95, @azzsal)

## 0.1.0

### Added

- First prototype of `api-diff` tool (@Siddhi-agg, @NathanReb)
- First prototype of `api-watch` library (@Siddhi-agg, @NathanReb)
