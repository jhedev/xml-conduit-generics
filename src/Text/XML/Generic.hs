{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Text.XML.Generic where

import           Data.Word
import           Data.Text (Text, pack)
import           Data.Char (toLower)
import qualified Data.Map.Lazy as LMap 
import           GHC.Generics
import           Text.XML
import           Text.Hamlet.XML

-- Typeclass ToXml

class ToXml a where
    toXml :: a -> [Node]

    default toXml :: (Generic a, GToXml (Rep a)) => a -> [Node]
    toXml a = gToXml (from a)

-- Instances

instance ToXml Int where
    toXml i = [xml|#{pack $ show i}|]

instance ToXml Integer where
    toXml i = [xml|#{pack $ show i}|]

instance ToXml Double where
    toXml d = [xml|#{pack $ show d}|]

instance ToXml Float where
    toXml f = [xml|#{pack $ show f}|]

instance ToXml Word8 where
    toXml w = [xml|#{pack $ show w}|]

instance ToXml Word16 where
    toXml w = [xml|#{pack $ show w}|]

instance ToXml Word32 where
    toXml w = [xml|#{pack $ show w}|]

instance ToXml Word64 where
    toXml w = [xml|#{pack $ show w}|]

instance ToXml String where
    toXml str = [xml|#{pack $ str}|]

instance ToXml a => ToXml [a] where
    toXml xs = foldl (\x y -> x ++ (toXml y)) [] xs

-- Generics

-- Generic Type Class
class GToXml f where
    gToXml :: f a -> [Node]

-- 
instance GToXml U1 where
    gToXml U1 = []
 
instance (GToXml a, GToXml b) => GToXml (a :*: b) where
    gToXml (a :*: b) = gToXml a  ++ gToXml b

instance (GToXml a, GToXml b) => GToXml (a :+: b) where
    gToXml (L1 z) = gToXml z
    gToXml (R1 z) = gToXml z

-- Use constructor of a datatype as element name
instance (GToXml a, Constructor c) => GToXml (M1 C c a) where
    gToXml m1  =  NodeElement (Element (Name (pack $ map toLower $ conName m1) Nothing Nothing) LMap.empty (gToXml (unM1 m1))) : []

instance (GToXml a) => GToXml (M1 D c a) where
    gToXml (M1 z) = gToXml z

instance (GToXml a, Selector c) => GToXml (M1 S c a) where
    gToXml m1 | selName m1 == "" = gToXml (unM1 m1) 
              | otherwise        = NodeElement (Element (Name (pack $ map toLower $ selName m1) Nothing Nothing) LMap.empty (gToXml ( unM1 m1))) : []

instance (ToXml a) => GToXml (K1 i a) where
    gToXml (K1 z) = toXml z
