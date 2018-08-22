{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module Settings (loadSettings, 
                 saveSettings, 
                 showSettings, 
                 restoreDefaults,
                 userPort,
                 userName,
                 setUserPort,
                 setUserName,
                 defaultSettings,
                 Settings) where

import System.IO(writeFile, readFile)
import Data.List.Split(splitOn)
import System.Directory(doesFileExist)

-- Testing
import Test.HUnit
import Test.QuickCheck

import SharedDataTypes (Port, Name)


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                      Data Types              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- 
    REPRESENTATION CONVENTION: 
        - Represents the settings that the user can set
        - userPort is the port that will be used when running the program
        - userName is the name that will be used as prompt in the chat
    REPRESENTATION INVARIANT:  
        -  True
 -}
data Settings = Settings {
      userPort :: Port
    , userName :: Name
} deriving (Show, Eq)


{- defaultSettings
   PURPOSE:  Create the default settings
   PRE:      True
   POST:     Settings with predefined values
-}
defaultSettings :: Settings
defaultSettings = Settings "3001" "User"



{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                               Interface Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- loadSettings
   PURPOSE:     Load the settings for the program
   PRE:         True
   POST:        The formatted settings from "settings.txt"
   SIDE EFFECTS Opens and read the file "setting.txt"
-}
loadSettings :: IO Settings
loadSettings = do
    fileExist <- doesFileExist "settings.txt"
    if not fileExist
        then do 
            restoreDefaults
            return defaultSettings
        else do 
            rawSettings <- readFile "settings.txt"
            return $ toSettings rawSettings      

{- saveSettings s
   PURPOSE:      Save settings for the program
   PRE:          True
   POST:         True
   SIDE EFFECTS: Overwrite the content of "settings.txt" with formatted contents of s
-}
saveSettings :: Settings -> IO ()
saveSettings s = writeFile "settings.txt" (fromSettings s)

{- showSettings s
   PURPOSE:      Display settings
   PRE:          True
   POST:         True
   SIDE EFFECTS: Write the formatted settings to stdOut
-}
showSettings :: Settings -> IO()
showSettings s = putStrLn $ fromSettings s

{- restoreDefaults
   PURPOSE:      Set the stored settings to the defaults
   PRE:          True
   POST:         True
   SIDE EFFECTS: Overwrite the content of "settings.txt" with defaultSettings
-}
restoreDefaults :: IO ()
restoreDefaults = saveSettings defaultSettings



{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                Functions for changing settings              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- setUserPort port s
   PURPOSE:  Change the user port in settings
   PRE:      True
   POST:     New settings with all values the same as in s except userPort, which is now port.
   EXAMPLES: setUserPort (Settings data with Port set to "3001" and Name to "Adam") "1357" ==
                (Settings data with Port set to "1357" and Name to "Adam")
-}
setUserPort :: Port -> Settings -> Settings
setUserPort port s = s {userPort = port}

{- setUserName name s
   PURPOSE:  Change the user name in settings
   PRE:      True
   POST:     New settings with all values the same as in s except userName, which is now name.
   EXAMPLES: setUserPort (Settings data with Port set to "3001" and Name to "Adam") "Jonas" ==
                (Settings data with Port set to "3001" and Name to "Jonas")
-}
setUserName :: Name -> Settings -> Settings
setUserName name s = s {userName = name}



{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                   Help Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- toSettings str
   PURPOSE:  Process a string into a Settings data type
   PRE:      str contains "Port: port" && "Name: name" on individual lines && validPort port && validName name
   POST:     All settings in str put into a Settings structure
   EXAMPLES: toSettings "Port: 3001\nName: Adam" = 
                Settings data structure with Port set to "3001" and Name to "Adam"
-}
toSettings :: String -> Settings
toSettings str = toSettingsAux defaultSettings $ map (splitOn ": ") $ lines str
    where
        toSettingsAux :: Settings -> [[String]] -> Settings
        toSettingsAux s [] = s
        toSettingsAux s (["Port", port]:strs) = toSettingsAux (setUserPort port s) strs
        toSettingsAux s (["Name", name]:strs) = toSettingsAux (setUserName name s) strs

{- fromSettings s
   PURPOSE:  Turn a settings data structure into a string
   PRE:      True
   POST:     The settings from s turned into a string
   EXAMPLES: fromSettings (Settings data structure with Port set to "3001" and Name to "Adam") =
                "Port: 3001\nName: Adam"
-}
fromSettings :: Settings -> String
fromSettings (Settings port name) = "Port: " ++ port ++ "\nName: " ++ name




{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Test cases              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
toSettingsTest1 = TestCase (assertEqual "toSettings: Port: 3001\nName: Adam" (Settings {userPort = "3001", userName = "Adam"}) (toSettings "Port: 3001\nName: Adam"))
  
fromSettingsTest1 = TestCase (assertEqual "fromSettings: userPort = \"3001\", userName = \"Adam\"" ("Port: 3001\nName: Adam") (fromSettings ((Settings {userPort = "3001", userName = "Adam"}) :: Settings )) )
  
--Credit: adopted code from piazza thread: "Testing in groups"
toSettingsTests = TestList [toSettingsTest1]
testToSetting = runTestTT toSettingsTests

fromSettingsTests = TestList [fromSettingsTest1]
testFromSetting = runTestTT fromSettingsTests

testAll = runTestTT (TestList [toSettingsTests, fromSettingsTests])

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 QuickCheck              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- qTest num
   PURPOSE:  Use QuickCheck to prove that toSettings and fromSettings are each others complement
   PRE:  num > 0
   POST: Number of tests will be defined by num
   EXAMPLES: qTest 1000 =
                +++ OK, passed 1000 tests.
-}   
qTest :: Int -> IO ()
qTest num = quickCheckWith stdArgs { maxSuccess = num } prop_settings   

prop_settings :: SafeName -> SafeNumber -> Bool
prop_settings (SafeName name) (SafeNumber number) = fromSettings (Settings {userPort = number, userName = name}) == (fromSettings (toSettings ("Port: " ++ number ++ "\nName: " ++ name)))

-- The following below were inspired from  http://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings by Ganesh Sittampalam

{- REPRESENTATION CONVENTION: Represents a safe name
   REPRESENTATION INVARIANT:  Only strings of a-z, A-Z are allowed
-}
data SafeName = SafeName { unwrapSafeName :: String }
    deriving Show

instance Arbitrary SafeNumber where
    arbitrary = SafeNumber <$> genSafeNumbers    

{- REPRESENTATION CONVENTION: Represents a safe number
   REPRESENTATION INVARIANT:  Only strings of '1'-'9' are allowed
-}    
data SafeNumber = SafeNumber { unwrapSafeNumber :: String }
    deriving Show

instance Arbitrary SafeName where
    arbitrary = SafeName <$> genSafeName

genSafeChar :: Gen Char
genSafeChar = elements (['a'..'z'] ++ ['A'..'Z'])

genSafeName :: Gen String
genSafeName = listOf genSafeChar    
 
genSafeNumber :: Gen Char
genSafeNumber = elements ['0'..'9']

genSafeNumbers :: Gen String
genSafeNumbers = listOf genSafeNumber
