
# Idris-Multibase

This is an implementation of [Multibase](https://github.com/multiformats/multibase) in idris.

The API allows to encode and decode strings in different bases. As such it exposes two functions

## API

```idris
encode : BaseSymbol n -> String -> String
```

This function takes a target base as its first argument and an arbitrary string and convert the string using
the given base.

```idris
decode : String -> Either (MultibaseError Char) String
```

This function decodes a string that has been encoded with the multibase format. It might return `MultibaseError`
if the format is not respected.

# Installation

Clone the project

Run 

```
idris --install idris-multibase.ipkg
```

import `Data.Multibase` and start using `encode` and `decode`.



