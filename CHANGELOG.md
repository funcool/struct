# Changelog #

## Version 2.0.0 ##

Date: 2019-08-28

This is **BREAKING CHANGES** release:

- The error reporting is changed; now is a map instead of simple
  string, and it includes the current value, the validator type and
  the validator functions used to generate this error.
- The error message translation is generalized; now the messages can
  be functions that will called when error is reported with current
  validator state as single argument.
- The validator structure map changed a little bit.

Other relevant changes:

- New `defs` macro for "compile" user defined spec (huge performance
  improvement over use spec directly).
- Removed `cuerdas` dependency (just for convenience of having 0 deps).


## Version 1.4.0 ##

Date: 2019-06-06

- Minor fix on handling neested data structures.
- Migrate to cli tools.

## Version 1.3.0 ##

Date: 2018-06-02

- Fix message formatting.


## Version 1.2.0 ##

Date: 2017-01-11

- Allow `number-str` and `integer-str` receive already coerced values.
- Minor code cleaning.
- Update dependencies.

## Version 1.1.0 ##

Date: 2017-08-16

- Add count validators.
- Update cuerdas to 2.0.3


## Version 1.0.0 ##

Date: 2016-06-24

- Add support for neested data structures.
- Add fast skip already validated and failed paths (performance improvement).
- BREAKING CHANGE: the errors are now simple strings. No additional list
  wrapping is done anymore. Because the design of the library is just fail
  fast and only one error is allowed.


## Version 0.1.0 ##

Date: 2016-04-19

Initial version.
