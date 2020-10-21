module Misc.Sure where

class Sure m where sure :: m a -> a
instance Sure Maybe where sure (Just x) = x
instance Sure (Either e) where sure (Right x) = x
