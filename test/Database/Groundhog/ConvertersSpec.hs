{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes #-}



{- |
Module      : Database.Groundhog.ConverterSpec
Description : Local Tests and an example for the Groundhog Converters library
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


Some of the functionality of this test suite is made available for use in applications through the package.
tasty-groundhog-converters Which is my favorite name in all of haskell.

| -}


module Database.Groundhog.ConvertersSpec (tests) where
  --------------------------------------------------
  -- Imports TO TEST SUITE
  --------------------------------------------------
import Test.Tasty.HUnit
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Database.Groundhog.Converters
import Data.Int (Int64)
import   Data.Map.Strict (Map)
import qualified  Data.Map.Strict  as Map
import Database.Groundhog.TH
import Test.Tasty
import Test.Tasty.QuickCheck
    


-- | Test round trip property of a converter,
-- >>> roundTripConverter "should convert a RecordBiMap to a StorableList" (==) myRecordConverter
-- You will need to create an Arbitrary instance for the incoming item
roundTripConverter :: Arbitrary a => TestName -> (a -> a -> Bool) -> Converter a b -> TestTree
roundTripConverter testName toBool converterToTest = testProperty testName  runRoundTripTest 
    where
      (f,g) = converterToTest
      runRoundTripTest = do
         val <- arbitrary
         (return . toBool val . g . f) val

-- | goldenSqlConverter takes advantage of the file nature of SQLlite to read in your data
-- using groundhog.  Even if you store your data in a different database
-- if the serialization of a converter works in SQLite it will probably work in
-- your given DB.  Obviously that isn't perfect but this makes writing a quick test easy
-- The 'a' you give the function will only be active for the first insert.
-- The rest of the time, the conversion and insertion are self contained.


goldenSqlConverter :: (PersistEntity b) =>  TestName ->
                       FilePath -> a -> 
                       (b -> b -> Bool) -> Converter a b  ->
                       TestTree
goldenSqlConverter testName fp someA  bToBool converter  = testCase testName (runSqlTest >>= assertBool "SQLite insertion and conversion should match original")
    where
       (toB,toA) = converter
       runSqlTest = withSqliteConn fp $ runDbConn $ do
         runMigration $ migrate (toB someA)
         bs <- selectAll
         case bs of
           [] -> do
              _ <- insert (toB someA)
              bs' <- selectAll
              return (and $ (\(_,b) -> (bToBool b . toB.toA) b) <$> bs' )
           _ -> return (and $ (\(_,b) -> (bToBool b . toB.toA ) b) <$> bs) 


  --------------------------------------------------


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


-- | build a golden test (a single test designed to make sure a representation stays constant over time).
-- The aGroup provided is only used the first time the test is used.  The converter at the top level here
-- is just (id, id) and (==) is used because there is an Eq instance on Group.
exampleGoldenSqlConverter :: TestTree
exampleGoldenSqlConverter = goldenSqlConverter "Test The test GoldenSqlConverter" "TestGolden" aGroup (==) (id,id) 
  where
    aGroup = Group somePeople
    somePeople = (Map.fromList . zip [1 ..] . fmap Person ) ["Margret"]

-- | There are no database hits on a round trip test
-- Converter makes the claim that a Converter is an Isomorphism between the two DataTypes.
-- Round trip tests should verify this.
exampleRoundTripTest :: TestTree
exampleRoundTripTest = roundTripConverter "roundtrip personMapConverter" (==) personMapConverter 

-- | call the example test



tests :: TestTree
tests = allTests
 where
    allTests = testGroup "all example groundhog converter tests" [ exampleGoldenSqlConverter
                                                                 , exampleRoundTripTest]
