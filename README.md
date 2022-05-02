# text-conversions [![Build Status](https://img.shields.io/github/workflow/status/cjdev/text-conversions/build/master)](https://github.com/cjdev/text-conversions/actions/workflows/build.yml) [![Hackage](https://img.shields.io/badge/hackage-0.3.1.1-5e5184)][hackage]

This is a small library to ease the pain when converting between the many different string types in Haskell. Unlike some other libraries that attempt to solve the same problem, text-conversions is:

  - **Safe.** This library treats binary data (aka `ByteString`) like binary data, and it does not assume a particular encoding, nor does it ever throw exceptions when failing to decode. It does, however, provide failable conversions between binary data and textual data.

  - **Extensible.** It’s easy to add or derive your own instances of the typeclasses to use your own types through the same interface.

Here’s an example of using text-conversions to convert between textual types:

```haskell
> convertText ("hello" :: String) :: Text
"hello"
```

And here’s an example of converting from UTF-8 encoded binary data to a textual format:

```haskell
> decodeConvertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
Just "hello"
> decodeConvertText (UTF8 ("\xc3\x28" :: ByteString)) :: Maybe Text
Nothing
```

[For more details, see the documentation on Hackage.][hackage]

[hackage]: https://hackage.haskell.org/package/text-conversions
