{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Language.Haskell.TH
import Lib
import Types

$(genEnzymeInfo ''Int)

main :: IO ()
main = do
  -- -- Printing the @DatatypeInfo@ for a type
  -- print $(stringE . show =<< newtypeDatatypeInfo)

  -- -- Use the @ToEnzymeStringInstance@ to print the name of the dataconstructor
  -- print (toEnzymeStringInstance $ Proxy @Newtype)

  -- -- Use the @ToEnzymeInfo@ to print the info of the dataconstructor
  -- print (toEnzymeDatatypeInfo $ Proxy @SumOfProducts)

  -- -- Use the @ToEnzymeInfo@ to print the info of the dataconstructor
  -- print (toEnzymeInfo $ Proxy @SumOfProducts)

  -- Use the @ToEnzymeRecurseToPrimitives@ to print the info of the dataconstructor
  putStrLn (toEnzymeRecurseToPrimitives $ Proxy @SumOfProducts)
