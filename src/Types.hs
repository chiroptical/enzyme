{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Types where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Tree
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

class ToEnzymeStringInstance a where
  toEnzymeStringInstance :: Proxy a -> String

genEnzymeStringInstance :: Name -> Q [Dec]
genEnzymeStringInstance name = do
  buildToStringInstance
    (InstanceName "ToEnzymeStringInstance")
    name
    (nameBase name)

class ToEnzymeDatatypeInfo a where
  toEnzymeDatatypeInfo :: Proxy a -> String

genEnzymeDatatypeInfo :: Name -> Q [Dec]
genEnzymeDatatypeInfo name = do
  datatypeInfo <- reifyDatatype name
  buildToStringInstance
    (InstanceName "ToEnzymeDatatypeInfo")
    name
    (show datatypeInfo)

class ToEnzymeInfo a where
  toEnzymeInfo :: Proxy a -> String

-- | This will only work for @Int@, but gets to @PrimTyConI@
-- @
--    TyConI (DataD _ _ _ _ [NormalC name' _] _) <- reify name
--    DataConI _ (AppT (AppT ArrowT (ConT name'')) _) _ <- reify name'
--    info <- reify name''
-- @
genEnzymeInfo :: Name -> Q [Dec]
genEnzymeInfo name = do
  info <- reify name
  buildToStringInstance
    (InstanceName "ToEnzymeInfo")
    name
    (show info)

member :: Eq a => a -> Tree a -> Bool
member x (Node y ys) = x == y || or (member x <$> ys)

insertAt :: Eq a => a -> a -> Tree a -> Tree a
insertAt x e (Node y ys') =
  case (ys', x == y) of
    ([], False) -> Node y []
    ([], True) -> Node y [Node e []]
    (ys, False) -> Node y (insertAt x e <$> ys)
    (ys, True) -> Node y (ys ++ [Node e []])

insert :: a -> Tree a -> Tree a
insert x (Node y []) = Node y [Node x []]
insert x (Node y ys) = Node y (insert x <$> ys)

topRoot :: Eq a => a -> Tree a -> Tree a
topRoot x tree@(Node y ys) =
  if x == y
    then tree
    else case selectForest x ys of
      [] -> tree
      (top : _) -> top

selectForest :: Eq a => a -> [Tree a] -> [Tree a]
selectForest _ [] = []
selectForest x (tree@(Node y ys) : zs) =
  if x == y
    then tree : selectForest x zs
    else selectForest x zs

class ToEnzymeRecurseToPrimitives a where
  toEnzymeRecurseToPrimitives :: Proxy a -> String

-- | The @[Name]@ is used to avoid infinite loops
type EnzymeM = ExceptT String Q

liftE = lift

genEnzymeTree :: Name -> Q [Dec]
genEnzymeTree name =
  runExceptT
    ( do
        info <- liftE $ reify name
        buildInfoTree (Node name []) name info
    )
    >>= \case
      Left err -> fail err
      Right tree -> buildToStringInstance (InstanceName "ToEnzymeRecurseToPrimitives") name (drawTree $ show <$> tree)

buildInfoTree :: Tree Name -> Name -> Info -> EnzymeM (Tree Name)
buildInfoTree tree name = \case
  DataConI _ ty _ -> buildTypeTree tree ty
  TyConI dec -> buildDecTree tree dec
  -- Not handled
  PrimTyConI {} -> pure tree
  ClassI {} -> pure tree
  ClassOpI {} -> pure tree
  FamilyI {} -> pure tree
  PatSynI {} -> pure tree
  VarI {} -> pure tree
  TyVarI {} -> pure tree

buildDecTree :: Tree Name -> Dec -> EnzymeM (Tree Name)
buildDecTree tree@(Node root forest) = \case
  DataD _ name _ _ cons _ -> do
    consForest <- concat <$> mapM (buildConForest name) cons
    pure . Node root $ forest ++ consForest
  NewtypeD _ name _ _ cons _ -> do
    consForest <- buildConForest name cons
    pure . Node root $ forest ++ consForest
  -- Not handled
  FunD {} -> pure tree
  ValD {} -> pure tree
  TySynD {} -> pure tree
  ClassD {} -> pure tree
  InstanceD {} -> pure tree
  SigD {} -> pure tree
  KiSigD {} -> pure tree
  ForeignD {} -> pure tree
  InfixD {} -> pure tree
  PragmaD {} -> pure tree
  DataFamilyD {} -> pure tree
  DataInstD {} -> pure tree
  NewtypeInstD {} -> pure tree
  TySynInstD {} -> pure tree
  OpenTypeFamilyD {} -> pure tree
  ClosedTypeFamilyD {} -> pure tree
  RoleAnnotD {} -> pure tree
  StandaloneDerivD {} -> pure tree
  DefaultSigD {} -> pure tree
  PatSynD {} -> pure tree
  PatSynSigD {} -> pure tree
  ImplicitParamBindD {} -> pure tree

buildBtyForest :: Name -> (Bang, Type) -> EnzymeM [Tree Name]
buildBtyForest name = \case
  (_, ty) -> buildTypeForest (Node name []) ty

buildVBtyForest :: (Name, Bang, Type) -> EnzymeM [Tree Name]
buildVBtyForest = \case
  (name, _, ty) -> buildTypeForest (Node name []) ty

buildConForest :: Name -> Con -> EnzymeM [Tree Name]
buildConForest name = \case
  NormalC name btys -> do
    forest <- concat <$> mapM (buildBtyForest name) btys
    pure [Node name forest]

  -- Not handled
  RecC name vbtys -> do
    forest <- concat <$> mapM buildVBtyForest vbtys
    pure [Node name forest]
  InfixC {} -> pure []
  ForallC {} -> pure []
  GadtC {} -> pure []
  RecGadtC {} -> pure []

third :: (a, b, c) -> c
third (_, _, c) = c

buildTypeForest :: Tree Name -> Type -> EnzymeM [Tree Name]
buildTypeForest tree@(Node root forest) = \case
  AppT x y -> do
    forestX <- buildTypeForest tree x
    forestY <- buildTypeForest tree y
    pure $ forestX <> forestY
  ConT name -> do
    if member name tree
      then pure []
      else do
        info <- liftE $ reify name
        tree <- buildInfoTree (Node name []) name info
        pure [tree]
  -- Not handled
  ForallT {} -> pure []
  ForallVisT {} -> pure []
  AppKindT {} -> pure []
  SigT {} -> pure []
  VarT {} -> pure []
  PromotedT {} -> pure []
  InfixT {} -> pure []
  UInfixT {} -> pure []
  ParensT {} -> pure []
  TupleT {} -> pure []
  UnboxedTupleT {} -> pure []
  UnboxedSumT {} -> pure []
  ArrowT {} -> pure []
  -- MulArrowT {} -> pure []
  EqualityT {} -> pure []
  ListT {} -> pure []
  PromotedTupleT {} -> pure []
  PromotedNilT {} -> pure []
  PromotedConsT {} -> pure []
  StarT {} -> pure []
  ConstraintT {} -> pure []
  LitT {} -> pure []
  WildCardT {} -> pure []
  ImplicitParamT {} -> pure []

buildTypeTree :: Tree Name -> Type -> EnzymeM (Tree Name)
buildTypeTree tree@(Node root forest) = \case
  AppT x y -> do
    forestX <- buildTypeForest (Node root []) x
    forestY <- buildTypeForest (Node root []) y
    pure . Node root $ forest ++ forestX ++ forestY
  ConT name -> do
    if member name (topRoot root tree)
      then pure tree
      else do
        info <- liftE $ reify name
        buildInfoTree tree name info
  -- Not handled
  ForallT {} -> pure tree
  ForallVisT {} -> pure tree
  AppKindT {} -> pure tree
  SigT {} -> pure tree
  VarT {} -> pure tree
  PromotedT {} -> pure tree
  InfixT {} -> pure tree
  UInfixT {} -> pure tree
  ParensT {} -> pure tree
  TupleT {} -> pure tree
  UnboxedTupleT {} -> pure tree
  UnboxedSumT {} -> pure tree
  ArrowT {} -> pure tree
  -- MulArrowT {} -> pure tree
  EqualityT {} -> pure tree
  ListT {} -> pure tree
  PromotedTupleT {} -> pure tree
  PromotedNilT {} -> pure tree
  PromotedConsT {} -> pure tree
  StarT {} -> pure tree
  ConstraintT {} -> pure tree
  LitT {} -> pure tree
  WildCardT {} -> pure tree
  ImplicitParamT {} -> pure tree

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (x : xs) = toLower x : xs

newtype InstanceName = InstanceName String

buildToStringInstance :: InstanceName -> Name -> String -> Q [Dec]
buildToStringInstance (InstanceName instanceStr) name contents = do
  let instanceName = mkName $ capitalize instanceStr
      funcName = mkName $ unCapitalize instanceStr
  pure
    <$> instanceD
      -- Constraints, e.g. (Ord a, Eq a)...
      (pure [])
      -- RHS of =>, in this case `ToEnzymeStringInstance name`
      ( pure $
          AppT
            ( ConT
                instanceName
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
            funcName
            [ Clause
                -- Function inputs, in this case it is ignored
                [WildP]
                -- Function body
                (NormalB . LitE . StringL $ contents)
                -- Extra declarations
                []
            ]
      ]
