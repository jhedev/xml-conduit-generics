{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import           Prelude hiding (writeFile)
import           Data.Map (empty)
import           Text.XML
import           Text.XML.Generic
import           GHC.Generics

data SimpleTree = Node Int SimpleTree SimpleTree | Leaf deriving (Show, Generic)

instance ToXml SimpleTree 

main :: IO()
main = do
        writeFile def { rsPretty = True } "tree.xml" $ Document (Prologue [] Nothing []) root []
            where
                tree = Node 10 (Node 5 Leaf Leaf) (Node 13 (Node 11 Leaf Leaf) (Node 15 Leaf Leaf))
                root = Element "tree" empty $ toXml tree
