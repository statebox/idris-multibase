
# Idris-Multibase

This is an implementation of [Multibase](https://github.com/multiformats/multibase) in Idris.

This can be used to encode and decode strings in different bases, such as  `1`, `binary`, `octal`, `decimal`, `hexadecimal`, `32bitshex`, `58bits`, and `64bits`.

## Usage

The API exposes two functions:

```idris
encode : BaseSymbol n -> String -> String
```

This function takes a target base and an arbitrary string as input, and converts the string using
the given base.

```idris
decode : String -> Either (MultibaseError Char) String
```

This function decodes a string that has been encoded with the multibase format. It will return `MultibaseError`
if the format is not respected.

See [Tests/Multibase.idr](https://github.com/statebox/idris-multibase/blob/master/src/Tests/Multibase.idr) for examples.

# Installation

1) Clone the project and `cd` into the project dir.

2) Compile it:
   ```
   idris --install idris-multibase.ipkg
   ```

4) Import `Data.Multibase` and start using `encode` and `decode`.
