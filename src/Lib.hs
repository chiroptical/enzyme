{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Types

newtype Newtype = Newtype Int

genEnzymeStringInstance ''Newtype

newtypeDatatypeInfo :: Q DatatypeInfo
newtypeDatatypeInfo = reifyDatatype ''Newtype

data ARecord = ARecord
  { aField0 :: Int,
    aField1 :: Maybe String
  }

newtype BRecord = BRecord {unBRecord :: CRecord}

newtype CRecord = CRecord {unCRecord :: Int}

data SumOfProducts
  = A ARecord
  | B BRecord

genEnzymeDatatypeInfo ''SumOfProducts
genEnzymeInfo ''SumOfProducts
genEnzymeTree ''SumOfProducts
