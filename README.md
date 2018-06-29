# text-conversions [![Build Status](https://travis-ci.org/cjdev/text-conversions.svg?branch=master)](https://travis-ci.org/cjdev/text-conversions)

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

[string-conversions](https://hackage.haskell.org/package/string-conversions) is very similar in size and purpose.  Which one you should use may be a matter of taste.
