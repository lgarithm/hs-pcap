{-# LANGUAGE ScopedTypeVariables #-}

module Misc.Binary where

import           Data.Word         (Word8)
import           Text.Parsec.Error (ParseError)

type Decode a = [Word8] -> Either ParseError a
type JustDecode a = [Word8] -> Maybe a

class FromBytes a where
  decode :: Decode a

justDecode :: forall a . (FromBytes a) => JustDecode a
justDecode bs =
  case decode bs of
    Right result -> Just (result :: a)
    _            -> Nothing

mustDecode :: forall a . (FromBytes a) => JustDecode a
mustDecode bs =
  case decode bs of
    Right result -> Just (result :: a)
