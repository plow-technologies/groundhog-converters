{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}


{- |
Module      : Database.Groundhog.Converters
Description : Common Converters for groundhog.
Copyright   : Plow Technologies LLC
License     : MIT License
Maintainer  : Scott Murphy


This is a library for creating composable converters between datatypes for the purpose
of inserting into a groundhog database.

The idea is to get from a representation a, which does not have a type class 'PersistEntity a'
to one 'b', which has that type class defined.

This allows lightweight types libraries to coexist with nice database representations.

| -}


module Database.Groundhog.Converters ( makeConverter
                                     , flipConverter
                                     , composeConverter
                                     , fmapConverter
                                     , bicomposeConverter
                                     , firstConverter
                                     , secondConverter
                                     , jsonConverter
                                     , integerConverter
                                     , mapConverter
                                     , bimapConverter
                                     , intMapConverter
                                     , Converter
                                     ) where
import           Control.Arrow             (first, second, (***))
import Data.Aeson (ToJSON,FromJSON,encode,eitherDecode)
import Data.Int (Int64)
import   Data.Map.Strict (Map)
import  Data.IntMap  (IntMap)
import qualified Data.IntMap  as IntMap
import qualified  Data.Map.Strict  as Map
import qualified Data.ByteString.Lazy as BL
import           Data.Bimap                (Bimap)
import qualified Data.Bimap                as Bimap

-- | The type of a converter from a newtype or opaque type to a DB-serializable type
type Converter a b = (a -> b, b -> a)

-- | Make a converter
--
-- There are preconditions on the input functions:
--
-- > makeConverter f g
--
-- prop> f (g x) = x
--
-- prop> g (f x) = x
makeConverter :: (a -> b) -- ^ Convert developer-facing type to database-storable type
              -> (b -> a) -- ^ Convert database-storable type to developer-facing type
              -> Converter a b
makeConverter ab ba = (ab, ba)

-- | Reverse the direction of a converter
flipConverter :: Converter a b -> Converter b a
flipConverter (ba, ab) = (ab, ba)

-- | Compose two converters
composeConverter :: Converter a b -> Converter b c -> Converter a c
composeConverter (ab, ba) (bc, cb) = (bc . ab, ba . cb)



-- | Map a converter over a functor
fmapConverter :: Functor f => Converter a b -> Converter (f a) (f b)
fmapConverter (ba, ab) = (fmap ba, fmap ab)


-- | compose a First and Second Converter 
bicomposeConverter  :: Converter a b -> Converter c d -> Converter (a,c) (b,d)
bicomposeConverter (ba,ab) (dc,cd) = (ba *** dc, ab *** cd)

-- | Convert only the first element of a pair
firstConverter :: Converter a b -> Converter (a, c) (b, c)
firstConverter (ba, ab) = (first ba, first ab)

-- | Convert only the second element of a pair
secondConverter :: Converter a b -> Converter (c, a) (c, b)
secondConverter (ba, ab) = (second ba, second ab)

-- | Convert via to and from JSON
jsonConverter :: (ToJSON a, FromJSON a) => Converter a BL.ByteString
jsonConverter = makeConverter encode (either error id . eitherDecode)

-- | Convert an 'Integer' (which doesn't have a 'PersistField' instance) to an 'Int64' (which does)
integerConverter :: Converter Integer Int64
integerConverter = makeConverter fromInteger fromIntegral

-- | Convert a 'Map' to a list of key-value pairs
mapConverter :: (Ord k) => Converter (Map k v) [(k, v)]
mapConverter = makeConverter Map.toList Map.fromList

-- | Convert a 'Bimap' to a list of key-value pairs
bimapConverter :: (Ord a, Ord b) => Converter (Bimap a b) [(a, b)]
bimapConverter = makeConverter Bimap.toList Bimap.fromList

-- | Convert an 'IntMap' to a list of 'Int'-value pairs
intMapConverter :: Converter (IntMap a) [(Int, a)]
intMapConverter = makeConverter IntMap.toList IntMap.fromList

