# enzyme

## Design Decisions

- Template Haskell over Generics
- Haskell should be the source of truth for types

## DatatypeInfo Notes

```haskell
data DatatypeInfo =
  DatatypeInfo
    { datatypeName :: Name                -- Type constructor
    , datatypeVars :: [TyVarBndrUnit]     -- Type parameters
    , datatypeInstTypes :: [Type]         -- Argument types
    , datatypeVariant :: DatatypeVariant  -- Extra information
    , datatypeCons :: [ConstructorInfo]   -- Normalize constructor information
    }
```

- You can generate a `DatatypeInfo` with `reifyDatatypeInfo {Name} :: Q`

### Some Examples

#### Basic Sum

```haskell
data Sum = A | B | C
```

```
{ datatypeName = Main.Sum
, datatypeVariant = Datatype
, datatypeCons =
  [ ConstructorInfo { constructorName = Main.A, constructorVariant = NormalConstructor }
  , ConstructorInfo { constructorName = Main.B, constructorVariant = NormalConstructor }
  , ConstructorInfo { constructorName = Main.C, constructorVariant = NormalConstructor }
  ]
}
```

- `Datatype :: DatatypeVariant`, could be `Datatype`, `Newtype`, `DataInstance`, `NewtypeInstance`
- `NormalConstructor :: ConstructorVariant`, could be `NormalConstructor`, `InfixConstructor`, `RecordConstructor [Name]`

#### Basic Newtype

```haskell
data Newtype = Newtype Int
```

```
{ datatypeName = Main.Newtype
, datatypeVariant = Newtype
, datatypeCons =
  [ ConstructorInfo
    { constructorName = Main.Newtype
    , constructorVariant = NormalConstructor
    , constructorFields = [ConT GHC.Types.Int]
    }
  ]
}
```

- `constructorFields :: [Type]`
- `ConT :: Name -> Type`, therefore `GHC.Types.Int :: Name`

#### Int

```
data Int
```

```
{ datatypeName = GHC.Types.Int
, datatypeVariant = Datatype
, datatypeCons =
  [ ConstructorInfo
    { constructorName = GHC.Types.I#
    , constructorVariant = NormalConstructor
    , constructorFields = [ConT GHC.Prim.Int#]
    }
  ]
}
```

- Moat uses `ToMoatType` to determine that `GHC.Types.Int` is `MoatType.Int`

#### One Occupant

```haskell
data A = A
```

```
{ datatypeName = Main.A
, datatypeVariant = Datatype
, datatypeCons =
  [ ConstructorInfo
    { constructorName = Main.A
    , constructorVariant = NormalConstructor
    , constructorFields = []
    }
  ]
}
```

## Questions

- What is a `InfixConstructor`?
