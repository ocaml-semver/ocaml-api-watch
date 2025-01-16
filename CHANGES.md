## unreleased

### Added

- Add detection of type declarations changes (#92, @azzsal)
- Add detection of module_type declarations changes (#93, @NchamJosephMuam)
- Add detection of classes addition and removal (#90, @marcndo)
- Add detection of addition and removal of class type declarations (#103, @azzsal)
- Add initial support for unwrapped libraries (#107, @Siddhi-agg, @azzsal)

### Changed

  - Improve diff representation of modified record types (#109, @azzsal)

### Deprecated

### Fixed

- Ignore hidden signature items (#102, @NchamJosephMuam)
- Remove duplicate items in class and class types (#105, @azzsal)

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
