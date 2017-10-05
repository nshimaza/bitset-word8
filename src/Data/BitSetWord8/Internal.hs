{-|
Module      : Network.BitSetWord8.Internal
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Space efficient set of 'Word8' and some pre-canned sets useful for parsing HTTP related 'ByteString'.
This file contains additional useful character sets but they aren't evaluated at compile time.
-}

{-# LANGUAGE DeriveLift #-}

module Data.BitSetWord8.Internal where

import           Prelude                    hiding (zipWith)

import           Data.Bits                  (setBit, shiftR, testBit)
import           Data.ByteString            (ByteString, index, pack, zipWith)
import           Data.Char                  (chr, ord)
import           Data.List                  (foldl', splitAt)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as Set (Set, fromList, member)
import           Data.Word                  (Word8)
import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax (Lift)


-- | Bitwise set of Word8.  Space efficient backend and O(1) membership test.
newtype BitSetWord8 = BitSetWord8 ByteString deriving (Eq, Lift, Show)

-- | DIGIT of RFC5324 in 'Char' list.
rfc5234Digit' :: [Char]
rfc5234Digit' = ['0'..'9']

-- | UPALPHA of RFC2616 in 'Char' list.
--   Note that RFC2616 has been obsoleted by RFC7230 and RFC7230 doesn't define UPALPHA.
rfc2616UpAlpha' :: [Char]
rfc2616UpAlpha' = [ 'A'..'Z' ]

-- | LOALPHA of RFC2616 in 'Char' list.
--   Note that RFC2616 has been obsoleted by RFC7230 and RFC7230 doesn't define LOALPHA.
rfc2616LoAlpha' :: [Char]
rfc2616LoAlpha' = [ 'a'..'z' ]

-- | ALPHA of RFC5324 in 'Char' list.
rfc5234Alpha' :: [Char]
rfc5234Alpha' = rfc2616UpAlpha' <> rfc2616LoAlpha'

-- | HEXDIG of RFC5324 in 'Char' list.
rfc5234HexDig' :: [Char]
rfc5234HexDig' = rfc5234Digit' <> ['A'..'F']

-- | VCHAR of RFC5324 in 'Char' list.
rfc5234VChar' :: [Char]
rfc5234VChar' = [ '!'..'~']

-- | WSP (aka white space) of RFC5324 in 'Char' list.
rfc5324Wsp' :: [Char]
rfc5324Wsp' = [ '\t', ' ' ]

-- | sub-delim of RFC3986 in 'Char' list.
rfc3986SubDelims' :: [Char]
rfc3986SubDelims' = [ '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=' ]

-- | gen-delim of RFC3986 in 'Char' list.
rfc3986GenDelims' :: [Char]
rfc3986GenDelims' = [ ':', '/', '?', '#', '[', ']', '@']

-- | reserved of RFC3986 in 'Char' list.
rfc3986Reserved' :: [Char]
rfc3986Reserved' = rfc3986GenDelims' <> rfc3986SubDelims'

-- | unreserved of RFC3986 in 'Char' list.
rfc3986Unreserved' :: [Char]
rfc3986Unreserved' = rfc5234Alpha' <> rfc5234Digit' <> [ '-', '.', '_', '~' ]

-- | pct-encoded of RFC3986 in 'Char' list.
rfc3986PctEncodedChar' :: [Char]
rfc3986PctEncodedChar' = ['%'] <> rfc5234HexDig'

-- | pchar of RFC3986 in 'Char' list.
rfc3986PChar' :: [Char]
rfc3986PChar' = rfc3986Unreserved' <> rfc3986PctEncodedChar' <> rfc3986SubDelims' <> [':', '@']

-- | URI-Reference of RFC3986 in 'Char' list.
rfc3986UriReference' :: [Char]
rfc3986UriReference' = rfc3986Reserved' <> rfc3986Unreserved' <> ['%']

-- | tchar of RFC7230 in 'Char' list.
rfc7230TChar' :: [Char]
rfc7230TChar' = [ '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~' ]
    <> rfc5234Digit' <> rfc5234Alpha'

-- | obs-text of RFC7230 in 'Char' list.
rfc7230ObsText' :: [Char]
rfc7230ObsText' = [ chr 0x80 .. chr 0xff]

-- | qdtext of RFC7230 in 'Char' list.
rfc7230QDText' :: [Char]
rfc7230QDText' = rfc5324Wsp' <> [ '!' ] <> [ '#' .. '[' ] <> [ ']' .. '~'] <> rfc7230ObsText'

-- | quoted-pair of RFC7230 in 'Char' list.
rfc7230QuotedPair' :: [Char]
rfc7230QuotedPair' = rfc5324Wsp' <> rfc5234VChar' <> rfc7230ObsText'

-- | O(1).  Return 'True' if given 'Word8' is a member of given 'BitSetWord8'.
member :: BitSetWord8 -> Word8 -> Bool
member (BitSetWord8 bs) w = testBit (index bs (fromIntegral (w `div` 8))) (fromIntegral (w `mod` 8))

-- | Convert given list of 'Char' into 'Set' of 'Word8'.  Any 'Char' having code point greater than 0xff is ignored.
toWord8Set :: [Char] -> Set.Set Word8
toWord8Set = Set.fromList . map fromIntegral . filter (<= fromIntegral (maxBound :: Word8)) . map ord

-- | Convert 'Set' of Word8 to full filled list of boolean existence flag.
toBoolList :: Set.Set Word8 -> [Bool]
toBoolList wSet = map (\w -> Set.member w wSet) [0..0xff]

-- | Pack 8 of boolean list into single 'Word8' bitwise set.
toWord8 :: [Bool] -> Word8
toWord8 = foldl' (\a e -> let aL = shiftR a 1 in if e == True then setBit aL 7 else aL) 0

-- | Convert full filled boolean list into 32 packed 'Word8' list.
toWord8List :: [Bool] -> [Word8]
toWord8List [] = []
toWord8List bs = let (bs8, rest) = splitAt 8 bs in toWord8 bs8 : toWord8List rest

-- | Convert given List of 'Char' into packed bitwise set of Word8.
--   Any 'Char' having code point greater than 0xff is ignored.
fromList :: [Char] -> BitSetWord8
fromList = BitSetWord8 . pack . toWord8List . toBoolList . toWord8Set

