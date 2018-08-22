{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module Address (loadAddressBook, 
                saveAddressBook, 
                showAddressBook, 
                addFriend, 
                getFriend, 
                deleteFriend,
                addrLen,
                empty,
                isEmpty,
                Friend) where

import Prelude hiding (readFile)
import System.IO.Strict(readFile)
import System.IO(writeFile)
import System.Directory(doesFileExist)
import Data.List.Split(splitOn)

import SharedDataTypes (IP, Port, Name)


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Data Types              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- 
    REPRESENTATION CONVENTION: 
        - Represents an entry in an address book, contains 
           a name, the persons IP-address and the port that the person uses
    REPRESENTATION INVARIANT:  
        - True
 -}
data Friend = Friend {
      getName :: Name
    , getIP   :: IP
    , getPort :: Port
} deriving (Show)

{- 
    REPRESENTATION CONVENTION: 
        - Represents an address book
    REPRESENTATION INVARIANT:  
        - Every Friend has a unique name
 -}
type AddrBook = [Friend]




{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Interface Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- loadAddressBook
   PURPOSE:      Load the addresses from the address book file
   PRE:          True
   POST:         The formated contents of addressBook.txt
   SIDE EFFECTS: Check if addressBook.txt exists, if it does: open the file and retrieve its content, 
                    else create the file
-}
loadAddressBook :: IO AddrBook
loadAddressBook = do
    fileExist <- doesFileExist "addressBook.txt"
    if not fileExist
        then do 
            saveAddressBook empty
            return empty
        else do 
            addressString <- readFile "addressBook.txt"
            return $ toAddrBook addressString 

{- saveAddressBook addr
   PURPOSE:     Save addresses to the address book file
   PRE:         True
   POST:        True
   SIDE EFFECTS Overwrites the content of addressBook.txt with the unformated contents of addr
-}
saveAddressBook :: AddrBook -> IO ()
saveAddressBook addr = writeFile "addressBook.txt" (fromAddrBook addr) 

{- showAddressBook addr
   PURPOSE:      Display all entries in an address book
   PRE:          True
   POST:         True
   SIDE EFFECTS: Print to the terminal either a standard message if addr is empty, 
                    else the layout-formated contents of addr
   EXAMPLES:     showAddressBook (address book containing entries for Jonas, Adam & Lotta) ->
                     prints out:
                     "1. Adam, 129.143.24.12, 3001
                      2. Jonas, 149.143.24.12, 3001
                      3. Lotta, 113.143.24.12, 299"
                 showAddressBook empty -> prints out: "No entries in you address book"
-}
showAddressBook :: AddrBook -> IO ()
showAddressBook addr
    | isEmpty addr = putStrLn "No entries in you address book"
    | otherwise    = putStrLn $ bookLayout addr

{- addFriend addr name ip port
   PURPOSE:  Add a new entry to an address book
   PRE:      True
   POST:     if name == getName f for any f in addr 
                then a new AddrBook containing all entries from addr with 
                     IP and Port of f changed to ip and port
                else a new AddrBook containing all entries from addr with a new entry added
                     containing name ip & port
   EXAMPLES: addFriend (empty) "Ada Lovelace" "1.1.1.1" "9810" = 
                AddrBook with one entry containing "Ada Lovelace" "1.1.1.1" "9810"
-}
addFriend :: AddrBook -> Name -> IP -> Port -> AddrBook
addFriend [] name ip port = (Friend name ip port) : []   
addFriend (f:addr) name ip port
    | getName f == name = (Friend name ip port) : addr
    | otherwise         = f: addFriend addr name ip port

{- getFriend addr n
   PURPOSE:  Get a specific entry with index (n-1) from an address book
   PRE:      n > 0 && n <= addrLen addr
   POST:     If index (n-1) is valid then Just the IP and port of the friend at index n-1,
                else Nothing
   EXAMPLES: getFriend (addFriend empty "Hello" "1.1.1.1" "1") 1 == Just ("1.1.1.1", "1")
             getFriend empty 1 == Nothing
-}
getFriend :: AddrBook -> Int ->  Maybe (IP, Port)
getFriend addr n
    | n <= 0 || n > addrLen addr = Nothing
    | otherwise                  = Just (ip,port)
    where
        friend = addr !! (n-1)
        ip = getIP friend
        port = getPort friend

{- deleteFriend addr n
   PURPOSE:  Delete a specific entry with index (n-1) from an address book
   PRE:      n > 0 && n <= addrLen addr
   POST:     If (n-1) is a valid index then a new AddrBook with all entries from addr but 
              with friend at index n-1 removed, else addr  
   EXAMPLES: deleteFriend (addFriend empty "Hello" "1.1.1.1" "1") 1 == empty
             deleteFriend (addFriend empty "Hello" "1.1.1.1" "1") 2 == address book with one element
-}
deleteFriend :: AddrBook -> Int ->  AddrBook
deleteFriend []     _ = []
deleteFriend (x:xs) 1 = xs
deleteFriend (x:xs) n 
    | n <= 0    = x:xs
    | otherwise = x : deleteFriend xs (n-1)


{- addrLen addr
   PURPOSE:  Get the number of contacts in an AddressBook
   PRE:      True
   POST:     The number of contacts in addr
   EXAMPLES: addrLen (addFriend empty "Hello" "1.1.1.1" "1") == 1
-}
addrLen :: AddrBook -> Int
addrLen = length

{- empty
   PURPOSE:  Create an empty address book
   PRE:      True
   POST:     An empty address book
-}
empty :: AddrBook
empty = []

{- isEmpty addr
   PURPOSE:  Check if an address book is empty
   PRE:      True
   POST:     True if addr is empty, False otherwise
   EXAMPLES: isEmpty (addFriend empty "Hello" "1.1.1.1" "1") = False
             isEmpty empty = True
-}
isEmpty :: AddrBook -> Bool
isEmpty [] = True
isEmpty _  = False


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Helper Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- toAddrBook str
   PURPOSE:      Convert a valid string into an address book.
   PRE:          str is in the form "name, ip, port\n" repeated 0 or more times &&
                    validName name && validIP ip && validPort port
   POST:         An AddrBook representing the data in str
   EXAMPLES:     toAddrBook "Hi, 1.1.2.3, 123\n" == AddrBook with one element containing 
                    name "Hi", IP "1.1.2.3" and port "123"
-}
toAddrBook :: String -> AddrBook
toAddrBook str = map toFriend (lines str)
    where
        toFriend :: String -> Friend
        toFriend s = Friend name ip port
            where
                [name, ip, port] = (splitOn ", " s)    


{- fromAddrBook addr
   PURPOSE:     Convert an address book to its string representation
   PRE:         True
   POST:        A String representing the data in addr
   EXAMPLES:    fromAddrBook (addFriend empty "Hello" "1.1.1.1" "1") == "Hello, 1.1.1.1, 1\n"
                fromAddrBook empty == ""
-}
fromAddrBook :: AddrBook -> String 
fromAddrBook addr = unlines (map fromFriend addr)
    where
        fromFriend :: Friend -> String
        fromFriend (Friend name ip port) = name ++ ", " ++ ip ++ ", " ++ port  

{- bookLayout addr
   PURPOSE:      Convert an address book into a fancy string representation, with numbered entries
   PRE:          True
   POST:         A string with name it and port from each entry in addr being represented as "n. name, IP: ip, Port: port\n"
                    where n = index of said entry + 1
   EXAMPLES:     bookLayout (addFriend empty "Hello" "1.1.1.1" "1") == "1. Hello,  IP: 1.1.1.1,  Port: 1\n"
-}
bookLayout :: AddrBook -> String
bookLayout addr  = unlines (printOrder 1 addr)
    where
        printOrder :: Int -> AddrBook -> [String]
        printOrder _ [] = []

        printOrder n ((Friend name ip port):addr)  = (show n ++ ". " ++ name ++ ",  IP: " ++ ip ++ ",  Port: " ++ port):
                                                        printOrder (n+1) addr
