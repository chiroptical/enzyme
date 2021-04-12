{-# LANGUAGE DataKinds #-}

module Types where

import Data.Proxy (Proxy (..))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

class ToEnzymeStringInstance a where
  toEnzymeStringInstance :: Proxy a -> String

buildEnzymeStringInstance :: Name -> Q [Dec]
buildEnzymeStringInstance name = do
  datatypeInfo <- reifyDatatype name
  pure
    <$> instanceD
      -- Constraints, e.g. (Ord a, Eq a)...
      (pure [])
      -- RHS of =>, in this case `ToEnzymeStringInstance name`
      ( pure $
          AppT
            ( ConT
                (mkName "ToEnzymeStringInstance")
            )
            -- Be careful with `VarT` here, which defines a variable and not a constructor
            ( ConT
                -- By default, `name` will be something like `Lib.Constructor`.
                -- `nameBase` will strip off the module location
                (mkName . nameBase $ name)
            )
      )
      [ pure $
          FunD
            (mkName "toEnzymeStringInstance")
            [ Clause
                -- Function inputs, in this case it is ignored
                [WildP]
                -- Function body
                (NormalB . LitE . StringL $ nameBase name)
                -- Extra declarations
                []
            ]
      ]
