xml-conduit-generics
====================

Small library based on xml-conduit to create xml representations for 
your own data types. It's just thrown together and still missing a few
default instances. 

#Example:
If we have a datatype like the following

```
data User = User { 
                   firstName :: String,
                   lastName :: String,
                   age :: Int,
                   hobbies :: [String]
                  }
                  deriving (Show, Generic)
```
With ```deriving Generic``` we let GHC derive the generic representations.
If we add

```
instance ToXml User
```
the compiler will create this instance for us. So let's create two users and
write the xml to a file

```
main :: IO()
main = do
        writeFile def { rsPretty = True } "users.xml" $ Document (Prologue [] Nothing []) root []
            where
                john = User "John" "Doe" 44 ["jokes", "laughing"]
                jane = User "Jane" "Doe" 38 []
                users = (toXml) john ++ (toXml jane)
                root = Element "users" empty users
```
This will create a users.xml file with the following content:

```
<?xml version="1.0" encoding="UTF-8"?>
<users>
    <user>
        <firstname>
            John
        </firstname>
        <lastname>
            Doe
        </lastname>
        <age>
            44
        </age>
        <hobbies>
            jokeslaughing
        </hobbies>
    </user>
    <user>
        <firstname>
            Jane
        </firstname>
        <lastname>
            Doe
        </lastname>
        <age>
            38
        </age>
        <hobbies/>
    </user>
</users>
```
You see there is a problem with the content of the hobbies tags. This is because the representation of 
a list is just the concatenation of the representation of the elements and a string is represented by its 
value. So this will just concat all strings in the list. To avoid this behaviour we create a simple
wrapper for our hobbies:

```
data Hobby = Hobby String deriving (Show, Generic)

instance ToXml Hobby
```
And change the user data type as follows:

```
data User = User { 
                   firstName :: String,
                   lastName :: String,
                   age :: Int,
                   hobbies :: [Hobby]
                  }
                  deriving (Show, Generic)
```

After updating john and jane to:

```
                john = User "John" "Doe" 44 [Hobby "jokes", Hobby "laughing"]
                jane = User "Jane" "Doe" 38 []
```

Now our xml file will look like this:

```
<?xml version="1.0" encoding="UTF-8"?>
<users>
    <user>
        <firstname>
            John
        </firstname>
        <lastname>
            Doe
        </lastname>
        <age>
            44
        </age>
        <hobbies>
            <hobby>
                jokes
            </hobby>
            <hobby>
                laughing
            </hobby>
        </hobbies>
    </user>
    <user>
        <firstname>
            Jane
        </firstname>
        <lastname>
            Doe
        </lastname>
        <age>
            38
        </age>
        <hobbies/>
    </user>
</users>
```