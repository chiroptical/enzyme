{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Language.Haskell.TH
import Lib
import Types

main :: IO ()
main = do
  -- Printing the @DatatypeInfo@ for a type
  print $(stringE . show =<< newtypeDatatypeInfo)

  -- Use the @ToEnzymeStringInstance@ to print the name of the dataconstructor
  print (toEnzymeStringInstance $ Proxy @Newtype)
