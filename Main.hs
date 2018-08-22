{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
import SharedDataTypes
import qualified Settings
import qualified SessionSettings
import qualified Netmod
import qualified Address
import qualified Graphics

import Text.Read (readMaybe)
import Data.Char (isDigit)
import System.Console.ANSI (setTitle)

import Control.Monad (when)
import Control.Exception (catch, IOException(..))
import System.IO
import System.Exit (exitWith, ExitCode(..))

{- main
   PURPOSE:      Start the chat program
   PRE:          True
   POST:         True
   SIDE EFFECTS: Change buffering of stdout, set window title, execute further IO functions
-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    setTitle "Chat Program"
    mainMenu

{- mainMenu
   PURPOSE:      Show the menu to the user and execute the user's choice
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a choice, execute further IO functions
-}
mainMenu :: IO ()
mainMenu = do
    Graphics.newPage
    putStrLn "Welcome to the Chat program!"
    putStrLn "What would you like to do?"
    putStrLn "1. Host a chat\n2. Connect to a host\n3. Change settings\n4. Edit your address book\n5. Exit program\n"
    putStr "Enter choice 1-5: "
    reply <- getLine
    case reply of
        "1" -> runHost
        "2" -> runClient
        "3" -> settingsMenu
        "4" -> addressMenu
        "5" -> do exitWith ExitSuccess
        _   -> do Graphics.errorMsgPopup False "#01" >> mainMenu
            
{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Chat Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- runHost
   PURPOSE:      Initialize the program as host of a session
   PRE:          True
   POST:         True
   SIDE EFFECTS: Load data from "settings.txt", prompt the user for data, execute the program as host
-}
runHost :: IO ()
runHost = do
    Graphics.newPage
    userSettings <- Settings.loadSettings
    userName <- getNameDef $ Settings.userName userSettings
    userPort <- getPortDef $ Settings.userPort userSettings
    let newSettings = Settings.setUserName userName $ Settings.setUserPort userPort userSettings
    Graphics.clearAll
    catch (Netmod.host $ SessionSettings.createSessionSettings newSettings) handleError

{- runClient
   PURPOSE:      Initialize the program as client of a session
   PRE:          True
   POST:         True
   SIDE EFFECTS: Load data from "settings.txt", prompt the user for data, execute the program as client
-}
runClient :: IO ()
runClient = do
    Graphics.newPage
    userSettings <- Settings.loadSettings
    userName <- getNameDef $ Settings.userName userSettings
    (ip, port) <- getClientSettings
    let sessionSet = SessionSettings.setSessionPort port $ SessionSettings.setSessionIP ip $ SessionSettings.createSessionSettings $ Settings.setUserName userName userSettings
    Graphics.clearAll
    catch (Netmod.client sessionSet) handleError

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                         Chat Helper Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- getClientSettings
   PURPOSE:      Get the ip and port that will be used to connect in the program 
   PRE:          True
   POST:         A validated IP and Port
   SIDE EFFECTS: Prompt the user for a choice, execute further IO functions
-}
getClientSettings :: IO (IP,Port)
getClientSettings = do
    Graphics.newPage
    putStrLn "What would you like to do?"
    putStrLn "1. Connect to a friend\n2. Connect directly via IP and Port"
    putStr "\nEnter choice 1-2: "
    reply <- getLine
    case reply of
        "1" -> friendConnect
        "2" -> directConnect
        _   -> Graphics.errorMsgPopup False "#01" >> getClientSettings

{- directConnect
   PURPOSE:      Get IP and Port to the host that the user wants to connect to
   PRE:          True
   POST:         A validated IP and Port
   SIDE EFFECTS: Prompt the user for an IP and Port, give choice of adding as new contact
-}
directConnect :: IO (IP, Port)
directConnect = do
    Graphics.newPage
    ip   <- getIP   $ "Enter the IP of the host: "
    port <- getPort $ "Enter the IP of the host: " ++ ip ++ "\nEnter the Port of the host: "
    putStr "Would you like to add as a contact? (y/n): "
    reply <- getLine
    case reply of
        "y" -> addContact ip port
        _   -> return ()
    return (ip, port) 

{- friendConnect
   PURPOSE:      Get IP and Port to the host from the friend that the user chooses
   PRE:          True
   POST:         A valid IP and Port
   SIDE EFFECTS: Prompt the user for a choice of contact, read from "addressBook.txt"
-}
friendConnect :: IO (IP, Port)
friendConnect = do
    Graphics.newPage
    putStrLn "Your address book:"
    loadedAddr <- Address.loadAddressBook
    Address.showAddressBook loadedAddr
    putStr "Enter the number of the friend that you want to connect to: "
    reply <- getLine
    if not (all isDigit reply)
        then Graphics.errorMsgPopup False "#03" >> friendConnect
        else case Address.getFriend loadedAddr (read reply) of
                Just result -> return result
                Nothing     -> Graphics.errorMsgPopup False "#02" >> friendConnect


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                   General Helper Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- handleError e
   PURPOSE:      Handle an exception
   PRE:          True
   POST:         True
   SIDE EFFECTS: Print out the error e, prompt the user to restart the program
   
Credit: https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Exception.html#v:catch
-}
handleError :: IOException -> IO ()
handleError e = do 
    let err = show (e :: IOException)
    hPutStr stderr ("An error was detected namely, " ++ err ++ "\n")
    putStr "Press [Enter] to restart"
    getLine
    mainMenu

{- getPortDef defaultPort
   PURPOSE:      Get a port from the user
   PRE:          True
   POST:         If the user enters nothing then defaultPort,
                  else the port that the user enters
   SIDE EFFECTS: Prompt the user for a port or to use a standard value
-}
getPortDef :: Port -> IO Port
getPortDef defaultPort = do
    putStr $ "Enter port number (default is " ++ defaultPort ++ "): "
    reply <- getLine
    case reply of
        ""   -> return defaultPort
        port -> if validPort port
                    then return port
                    else Graphics.errorMsgPopup False "#11" >> Graphics.newPage >> getPortDef defaultPort

{- getNameDef defaultName
   PURPOSE:      Get a name from the user
   PRE:          True
   POST:         If the user enters nothing then defaultName,
                  else the name that the user enters
   SIDE EFFECTS: Prompt the user for a name or to use a standard value
-}
getNameDef :: Name -> IO Name
getNameDef defaultName = do
    putStr $ "Enter your name (default is " ++ defaultName ++ "): "
    reply <- getLine
    case reply of
        ""   -> return defaultName
        name -> if validName name
                    then return name
                    else Graphics.errorMsgPopup False "#13" >> Graphics.newPage >> getNameDef defaultName

{- getIP s
   PURPOSE:      Get an IP address from the user
   PRE:          True
   POST:         An IP address from the user
   SIDE EFFECTS: Prompt the user with s to get an IP-address
-}
getIP :: String -> IO IP
getIP s = do
    Graphics.newPage
    putStr s
    reply <- getLine
    if reply == "localhost" || reply == "0.0.0.0"
        then return "localhost"
        else if validIP reply 
                then return reply
                else Graphics.errorMsgPopup False "#12" >> getIP s


{- getPort s
   PURPOSE:      Get a port number from the user
   PRE:          True
   POST:         A Port from the user
   SIDE EFFECTS: Prompt the user with s to get a port number
-}
getPort :: String -> IO Port
getPort s = do
    Graphics.newPage
    putStr s
    reply <- getLine
    if validPort reply 
        then return reply
        else Graphics.errorMsgPopup False "#11" >> getPort s
                
{- getName s
   PURPOSE:      Get a name from the user
   PRE:          True
   POST:         A name from the user
   SIDE EFFECTS: Prompt the user with s to get a name
-}
getName :: String -> IO Name
getName s = do
    Graphics.newPage
    putStr s
    reply <- getLine
    if validName reply 
        then return reply
        else Graphics.errorMsgPopup False "#13" >> getName s

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Settings Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- settingsMenu
   PURPOSE:    	 Show the settings menu to the user and execute the user's choice
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a choice, execute further IO functions
-}
settingsMenu :: IO ()
settingsMenu = do
    Graphics.newPage
    putStrLn "Settings menu"
    putStrLn "What would you like to do?"
    putStrLn "1. Show current settings\n2. Change settings\n3. Restore to defaults\n4. Back to main menu"
    putStr "\nEnter choice 1-4: "
    reply <- getLine
    case reply of
        "1" -> showCurrentSettings >> settingsMenu
        "2" -> changeSettings >> settingsMenu
        "3" -> Graphics.newPage >> Settings.restoreDefaults >>  putStr "\nDefaults restored. Press [Enter] to continue" >> getLine >> settingsMenu
        "4" -> mainMenu
        _   -> Graphics.errorMsgPopup False "#01" >> settingsMenu


{- changeSettings
   PURPOSE:      Show the change settings menu to the user and execute the user's choice
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a choice, execute further IO functions
-}
changeSettings :: IO ()
changeSettings = do
    Graphics.newPage
    putStrLn "Change settings"
    putStrLn "Which settings would you like to change?"
    putStrLn "1. Change user name\n2. Change port\n3. Back to settings menu"
    putStr "\nEnter choice 1-3: "
    reply <- getLine
    case reply of
        "1" -> changeUserName 
        "2" -> changeUserPort
        "3" -> return ()
        _   -> Graphics.errorMsgPopup False "#01" >> changeSettings


{- showCurrentSettings
   PURPOSE:      Show the settings from settings.txt
   PRE:          True
   POST:         True
   SIDE EFFECTS: Read from settings.txt and write out its formatted contents to the console
-}
showCurrentSettings :: IO ()
showCurrentSettings = do
    Graphics.newPage
    putStrLn "Your current settings are: \n"
    s <- Settings.loadSettings
    Settings.showSettings s
    putStr "\nPress [Enter] to continue" 
    getLine
    return ()

{- changeUserName
   PURPOSE:      Change the user's name in settings.txt
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a name, update the data in settings.txt with said name
-}
changeUserName :: IO ()
changeUserName = do
    Graphics.newPage
    s <- Settings.loadSettings
    newName <- getName $ "Enter your new user name (current: " ++ Settings.userName s ++ "): "
    Settings.saveSettings $ Settings.setUserName newName s
    putStr "\nThe settings have been saved! Press [Enter] to continue"
    getLine
    return ()
 
{- changeUserPort
   PURPOSE:      Change the user's port in settings.txt
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a port, update the data in settings.txt with said port
-}
changeUserPort :: IO ()
changeUserPort = do
    Graphics.newPage
    s <- Settings.loadSettings
    newPort <- getPort $ "Enter your new port number (current: " ++ Settings.userPort s ++ "): "
    Settings.saveSettings $ Settings.setUserPort newPort s
    putStr "\nThe settings have been saved! Press [Enter] to continue"
    getLine
    return ()

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Address Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- addressMenu
   PURPOSE:    	 Show the address menu to the user and execute the user's choice
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a choice, execute further IO functions
-}
addressMenu :: IO ()
addressMenu = do
    Graphics.newPage
    putStrLn "Address menu"
    putStrLn "What would you like to do?"
    putStrLn "1. Show address book\n2. Add friend\n3. Delete friend\n4. Back to main menu"
    putStr "\nEnter choice 1-4: "
    reply <- getLine
    case reply of
        "1" -> showCurrentAddressBook >> addressMenu
        "2" -> addNewFriend >> addressMenu
        "3" -> deleteFriend >> addressMenu
        "4" -> mainMenu
        _   -> Graphics.errorMsgPopup False "#01" >> addressMenu

{- showCurrentAddressBook
   PURPOSE:      Show the address book from addressBook.txt
   PRE:          True
   POST:         True
   SIDE EFFECTS: Read from addressBook.txt and write out its formatted contents to the console
-}
showCurrentAddressBook :: IO ()
showCurrentAddressBook = do
    Graphics.newPage
    putStrLn "Your address book contains: \n"
    a <- Address.loadAddressBook
    Address.showAddressBook a
    putStr "\nPress [Enter] to continue" 
    getLine
    return ()
    
    
{- addNewFriend
   PURPOSE:      Add a new friend to the address book
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for data, add an entry in addressBook.txt using said data
-}
addNewFriend :: IO ()
addNewFriend = do
    Graphics.newPage
    ip   <- getIP   $ "Enter the information for the new friend:\nIP address: "
    port <- getPort $ "Enter the information for the new friend:\nIP address: " ++ ip ++ "\nPort number: "
    name <- getName $ "Enter the information for the new friend:\nIP address: " ++ ip ++ "\nPort number: " ++ port ++ "\nName: "
    loadedAddr <- Address.loadAddressBook
    Address.saveAddressBook $ Address.addFriend loadedAddr name ip port
    putStr "\nFriend added. Press [Enter] to continue" 
    getLine
    return ()
    
    
{- deleteFriend
   PURPOSE:      Delete a friend in the address book
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a number, remove an entry in addressBook.txt
-}  
deleteFriend :: IO ()
deleteFriend = do
    Graphics.newPage
    putStrLn "Your address book contains: \n0. Go back"
    loadedAddr <- Address.loadAddressBook
    Address.showAddressBook loadedAddr
    putStr "\nEnter the number to the friend that you want to delete: "
    reply <- getLine
    case (readMaybe reply :: Maybe Int) of
        Nothing -> Graphics.errorMsgPopup False "#03" >> deleteFriend
        Just 0  -> return ()
        Just n  -> if Address.addrLen loadedAddr < n 
                    then Graphics.errorMsgPopup False "#02" >> deleteFriend
                    else do
                        Address.saveAddressBook (Address.deleteFriend loadedAddr n)
                        putStr "\nThe friend has been deleted! Press [Enter] to continue"
                        getLine
                        return ()

{- addContact ip port
   PURPOSE:      Add a contact to the address book
   PRE:          True
   POST:         True
   SIDE EFFECTS: Prompt the user for a name, add an entry in addressBook.txt containing ip & port
-}
addContact :: IP -> Port -> IO ()
addContact ip port = do
    Graphics.newPage
    name <- getName $  "Write the name of the new contact: "
    addr <- Address.loadAddressBook
    let newAddr = Address.addFriend addr name ip port 
    Address.saveAddressBook newAddr
