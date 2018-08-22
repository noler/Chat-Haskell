{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module SessionSettings (SessionSettings, createSessionSettings, userSettings, 
                        sessionIP, sessionSocket, sessionName, sessionPort, 
                        setSessionIP, setSessionPort, setSessionName, setSessionSocket) where

import SharedDataTypes(IP, Port, Name)
import Settings(Settings)

import Network.Socket(Socket)


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Data Type              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- 
    REPRESENTATION CONVENTION: 
        - This data type will contain all the information needed during a chat session.
        - The userSettings are the settings that are saved in the settings.txt file.
        - The sessionIP is the IP of the host that the client connects to, the host will have sessionIP "0.0.0.0".
        - The sessionPort is the port of the host that the client connects to, just like the sessionIP the host will
            have a standard value of "0" because the Port and IP of the client will not be used in a session.
        - The sessionName is the name of the person that one is chatting with. It is sent in the beginning of the session.
        - The sessionSocket is the socket that the host or client is using during the session.
    REPRESENTATION INVARIANT:  
        - sessionSocket may be undefined
 -}
data SessionSettings = SessionSettings {
    userSettings :: Settings,
    sessionIP    :: IP,
    sessionPort  :: Port,
    sessionName  :: Name,
    sessionSocket :: Socket
} deriving (Show)

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Interface Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- createSessionSettings s
   PURPOSE:  Create the session settings for a new session
   PRE:      True
   POST:     New SessionSettings where userSettings is s and the other values are default
   EXAMPLES: createSessionSettings (Settings data with Port set to "3001" and Name to "Adam") ==
                SessionSettings where userSettings is (Settings data with Port set to "3001" and Name to "Adam")
                                      sessionIP    is "0.0.0.0"
                                      sessionPort  is "0"
                                      sessionName  is "Replier"
-}
createSessionSettings :: Settings -> SessionSettings
createSessionSettings s = SessionSettings s "0.0.0.0" "0" "Replier" undefined


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                Functions for changing settings              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- setSessionIP ip s
   PURPOSE:  Change the session ip in a session setting
   PRE:      True
   POST:     New session settings with all values the same as s except sessionIP, which is now ip.
   EXAMPLES: setSessionIP "192.168.1.1" (SessionSettings data with sessionIP set to "111.111.122.121", 
                    sessionPort set to "3001" and sessionName to "Jonas") ==
                (SessionSettings data with sessionIP set to "192.168.1.1", 
                    sessionPort set to "3001" and sessionName to "Jonas")
-}
setSessionIP :: IP -> SessionSettings -> SessionSettings
setSessionIP ip s = s {sessionIP = ip}

{- setSessionPort port s
   PURPOSE:  Change the session port in a session setting
   PRE:      True
   POST:     New session settings with all values the same as s except sessionPort, which is now port.
   EXAMPLES: setSessionPort "1378" (SessionSettings data with sessionIP set to "111.111.122.121", 
                    sessionPort set to "3001" and sessionName to "Jonas") ==
                (SessionSettings data with sessionIP set to "111.111.122.121", 
                    sessionPort set to "1378" and sessionName to "Jonas")
-}
setSessionPort :: Port -> SessionSettings -> SessionSettings
setSessionPort port s = s {sessionPort = port}

{- setSessionName name s
   PURPOSE:  Change the session name in a session setting
   PRE:      True
   POST:     New session settings with all values the same as s except sessionName, which is now name.
   EXAMPLES: setSessionName "Adam" (SessionSettings data with sessionIP set to "111.111.122.121", 
                    sessionPort set to "3001" and sessionName to "Jonas") ==
                (SessionSettings data with sessionIP set to "111.111.122.121", 
                    sessionPort set to "3001" and sessionName to "Adam")
-}
setSessionName :: Name -> SessionSettings -> SessionSettings
setSessionName name s = s {sessionName = name}

{- setSessionSocket sock s
   PURPOSE:  Change the session socket in the session settings
   PRE:      True
   POST:     New session settings with all values the same as s except sessionSocket, which is now sock.
-}
setSessionSocket :: Socket -> SessionSettings -> SessionSettings
setSessionSocket sock s = s {sessionSocket = sock}
