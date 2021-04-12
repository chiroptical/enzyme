{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Types

newtype Newtype = Newtype Int

buildEnzymeStringInstance ''Newtype

newtypeDatatypeInfo :: Q DatatypeInfo
newtypeDatatypeInfo = reifyDatatype ''Newtype
