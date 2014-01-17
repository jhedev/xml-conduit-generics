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

data Hobby = Hobby String deriving (Show, Generic)

data User = User { 
                   firstName :: String,
                   lastName :: String,
                   age :: Int,
                   hobbies :: [Hobby]
                  }
                  deriving (Show, Generic)

instance ToXml Hobby
instance ToXml User

main :: IO()
main = do
        writeFile def { rsPretty = True } "users.xml" $ Document (Prologue [] Nothing []) root []
            where
                john = User "John" "Doe" 44 [Hobby "jokes", Hobby "laughing"]
                jane = User "Jane" "Doe" 38 []
                users = (toXml) john ++ (toXml jane)
                root = Element "users" empty users

