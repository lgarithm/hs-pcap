{-# LANGUAGE StandaloneDeriving #-}

module Net.Arp.Pprint where

import           Net.Arp.Format  (ArpFrame (ArpFrame))
import           Net.Link.Pprint ()

deriving instance Show ArpFrame
