# enzyme

## Design Decisions

- Template Haskell over Generics
- Haskell should be the source of truth for types

## Getting Constructor Information

```haskell
newtype Newtype = Newtype Int
```

```
DatatypeInfo
  { datatypeContext = []
  , datatypeName = Lib.Newtype
  , datatypeVars = []
  , datatypeInstTypes = []
  , datatypeVariant = Newtype
  , datatypeCons = 
    [ ConstructorInfo
      { constructorName = Lib.Newtype
      , constructorVars = []
      , constructorContext = []
      , constructorFields = [ConT GHC.Types.Int]
      , constructorStrictness = [FieldStrictness {fieldUnpackedness = UnspecifiedUnpackedness, fieldStrictness = UnspecifiedStrictness}]
      , constructorVariant = NormalConstructor
      }
    ]
  }
```
