# bitset-word8

[![Build Status](https://travis-ci.org/nshimaza/bitset-word8.svg?branch=master)](https://travis-ci.org/nshimaza/bitset-word8)

Space efficient set of `Word8` and some pre-canned sets useful for parsing HTTP related `ByteString`.
This packaged is intended to provide O(1) membership test on any subset of ASCII and Latin-1 character set
in order to write efficient HTTP related parser.

### Creating your own set

You can create your own set by `fromList`.

```haskell
myCharSet :: BitSetWord8
myCharSet = fromList [ 'Y', 'y', 'N', 'n' ]
```

You can create pre-evaluated set using Template Haskell.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Lift, lift)

myPreEvaluatedCharSet :: BitSetWord8
myPreEvaluatedCharSet = $(lift myCharSet)
```
