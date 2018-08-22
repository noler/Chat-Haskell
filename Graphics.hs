{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module Graphics (clearAll, errorMsgPopup, newPage, ErrorCode) where

import System.Console.ANSI


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Data Type              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- 
    REPRESENTATION CONVENTION: 
        - Represents an error in the running of the program
        - Each error code represents an unique error
    REPRESENTATION INVARIANT:  
        - the error begins with '#' and is followed by two digits ('0'-'9').
    EXAMPLES:
        - "#01"
        - "#21"
    ERROR CODES:
        - #01   - Invalid choice

        - #02   - Index out of bounds
        - #03   - Not a Number (NaN)

        - #11   - Invalid Port
        - #12   - Invalid IP
        - #13   - Invalid Name
 -}
type ErrorCode = String


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- clearAll
   PURPOSE:      Force the console to fully clear the screen
   PRE:          True
   POST:         True
   SIDE EFFECTS: The console is cleared
-}
clearAll :: IO ()
clearAll = mapM_ (\n -> setCursorPosition n 0 >> clearLine) [0..25] >> setCursorPosition 0 0


{- errorMsgPopup scroll ec
   PURPOSE:      Print out an error message popup
   PRE:          True
   POST:         True
   SIDE EFFECTS: Print out an error message containing ec, wait for input from keyboard,
                    is scroll then scroll up one pixel
-}
errorMsgPopup :: Bool -> ErrorCode -> IO ()
errorMsgPopup scroll ec = do
    let backColor = Yellow
    let backSetting = Vivid
    let forColor = Red
    let forSetting = Dull
    
    setCursorPosition 9 14
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                                                      "
    setCursorPosition 10 14
    putStr $ " Invalid input (Code: "++ec++"), press [Enter] to continue "
    setCursorPosition 11 14
    putStr "                                                      "
    setSGR []
    hideCursor
    if scroll then scrollPageDown 1 else return ()
    getLine
    return ()

{- newPage
   PURPOSE:      Generate a new clear page for the menu
   PRE:          True
   POST:         True
   SIDE EFFECTS: Clear the console and print out the logo
  
ASCII art generated at: http://patorjk.com/software/taag/ font: Big by Glenn Chappell
-}  
newPage :: IO ()
newPage = do
    showCursor
    clearAll 
    
    let backColor = Cyan
    let backSetting = Dull
    let forColor = White
    let forSetting = Vivid
    
    putStr "\n    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "     _____ _           _     _____                                     \n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "    / ____| |         | |   |  __ \\                                    \n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "   | |    | |__   __ _| |_  | |__) | __ ___   __ _ _ __ __ _ _ __ ___  \n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "   | |    | '_ \\ / _` | __| |  ___/ '__/ _ \\ / _` | '__/ _` | '_ ` _ \\ \n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "   | |____| | | | (_| | |_  | |   | | | (_) | (_| | | | (_| | | | | | |\n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "    \\_____|_| |_|\\__,_|\\__| |_|   |_|  \\___/ \\__, |_|  \\__,_|_| |_| |_|\n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                                              __/ |                    \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                              _____ ___  ____|___/                     \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                             |  __ \\__ \\|  __ \\                        \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                     ______  | |__) | ) | |__) |_____                  \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                    |______| |  ___/ / /|  ___/______|                 \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                             | |    / /_| |                            \n"
    setSGR []
    putStr "    "    
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                             |_|   |____|_|                            \n"
    setSGR []
    putStr "    "
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground forSetting forColor, SetColor Background backSetting backColor]
    putStr "                                                                       \n"
    setSGR []
    putStr "\n"
