# Changelog

## 0.2.0 (May 25, 2016)

### Breaking Changes

- The `ConvertText` typeclass has been split into two simpler functions, `convertText` and `decodeConvertText`. This means the vision of a unified function for converting between *any* two textual datatypes is no longer implemented, but the original attempt had too many problems to really be worth the cost. Specifically, instances like `FromText (Maybe Foo)` would cause problems due to the overlapping instances, which have now been removed.

## 0.1.0 (May 24, 2016)

- Initial release
