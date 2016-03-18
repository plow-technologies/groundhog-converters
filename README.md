# groundhog-converters
Groundhog allows a programmer to represent a haskell datatype by an equivalent type that is more suited for Database work.
groundhog-converters provides a selection of common converters that have proved useful.


## Usage
``` haskell
-- | Sample DataType 'Group' proviides a Map between an Integer and  a 'Person'
-- However, the person is embedded in the Datatype relative to the SQL database

data Group = Group {
     _people :: Map Integer Person
         }
 deriving (Eq)


-- | A wrapped representation of a Person
data Person = Person { _unPerson :: String}
  deriving (Eq)

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary

-- | An Isomorphism between the representation that is pleasent to use in haskell
-- and the one that makes sense to store i.e. 'PersistEntity' 
personMapConverter :: Converter (Map Integer Person) [(Int64,String)]
personMapConverter = mapConverter `composeConverter` fmapConverter (bicomposeConverter integerConverter personConverter)

-- | This converter is embedded in 'personMapConverter'
personConverter :: Converter Person String
personConverter = (_unPerson,Person)


-- | A declaration for group.
mkPersist defaultCodegenConfig [groundhog|
- entity: Group
  constructors:
  - name: Group
    fields:
      - name: _people
        dbName: people
        exprName: MappedIdToPerson
        converter: personMapConverter
- primitive: Person
  converter: personConverter


|]
```


## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

