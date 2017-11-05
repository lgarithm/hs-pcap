module Misc.Plain(plain) where

newtype Plain = Plain String

instance Show Plain where show (Plain s) = s

plain = Plain
