# Changelog

## 0.3.1 (September 29th, 2020)

- Added support for `base16-bytestring-1.0`.

## 0.3.0 (June 9, 2016)

### New Features

- The `Base16` and `Base64` newtypes are now provided for managing safe conversions between binary data and Base16 and Base64 textual encodings of that data.

## 0.2.0 (May 25, 2016)

### Breaking Changes

- The `ConvertText` typeclass has been split into two simpler functions, `convertText` and `decodeConvertText`. This means the vision of a unified function for converting between *any* two textual datatypes is no longer implemented, but the original attempt had too many problems to really be worth the cost. Specifically, instances like `FromText (Maybe Foo)` would cause problems due to the overlapping instances, which have now been removed.

## 0.1.0 (May 24, 2016)

- Initial release
