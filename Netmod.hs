{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module Netmod (client, host) where

import Control.Concurrent(forkIO, myThreadId, killThread, ThreadId)
import Network.Socket
import System.Console.ANSI

import SharedDataTypes (IP,Port,Name)
import SessionSettings
import Settings(userName, userPort, setUserName, setUserPort, defaultSettings)


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Interface Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- client settings
   PURPOSE:      Initialize a chat with a host as a client
   PRE:          sessionIP settings and sessionPort settings lead to an open host
   POST:         True
   SIDE-EFFECTS: Write to stdOut, create a socket and connect to an external host with 
                  IP-address = sessionIP settings and port = sessionPort settings, 
                 send data to host, execute further IO functions
-}
client :: SessionSettings -> IO ()
client settings = do
    let ip   = sessionIP settings
        port = sessionPort settings
        name = userName $ userSettings settings

    (sock, sockAddr) <- createSocket (Just ip) port
    putStrLn "Connecting..."
    
    connect sock sockAddr
    
    send sock name
    hostName <- recv sock 1024
    putStrLn $ "Connected to " ++ hostName ++ "@" ++ ip ++ ":" ++ port
    
    let newSettings = setSessionName hostName $ setSessionSocket sock settings

    clientThreadId <- myThreadId
    forkIO (sendMsg newSettings clientThreadId)
    readMsg newSettings clientThreadId
    
{- host settings
   PURPOSE:      Host a chat session
   PRE:          True
   POST:         True
   SIDE-EFFECTS: Write to stdOut, create a socket and await a connection from an external client,
                 send data to client, execute further IO functions
-}
host :: SessionSettings -> IO ()
host settings = do
    let port = userPort $ userSettings settings
        name = userName $ userSettings settings

    (sock, sockAddr) <- createSocket Nothing port
    bind sock sockAddr
    listen sock 0
    putStrLn "Waiting for a friend to connect..."
    (sock, clientAddr) <- accept sock    
    clientName <- recv sock 1024
    send sock name

    putStrLn $ "Connected to " ++ clientName
    
    let newSettings = setSessionName clientName $ setSessionSocket sock settings
    hostThreadId <- myThreadId
    forkIO (sendMsg newSettings hostThreadId)
    readMsg newSettings hostThreadId


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                         Helper Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}

{- createSocket ip port
   PURPOSE:      Open a socket
   PRE:          ip == Nothing || ip && port lead to an open host 
   POST:         If ip == Nothing then a new socket and socket address to the Port port
                  else a new socket and socket address to the IP-address ip and Port port
   SIDE-EFFECTS: Create a socket,  look up the address info of ip and port 
-}
createSocket :: Maybe IP -> Port -> IO (Socket, SockAddr)
createSocket ip port = do
    let settings = case ip of
                Nothing -> (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                _       -> Nothing
    (addr:_) <- getAddrInfo settings ip (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    return (sock, addrAddress addr)

{- readMsg settings threadId
   PURPOSE:      Recieve a message from another user
   PRE:          sessionSocket settings is open && threadId is the id of the main thread
   POST:         True
   SIDE-EFFECTS: Recieve a message from sessionSocket settings, display message to stdOut, 
                  execute further IO functions
-}
readMsg :: SessionSettings -> ThreadId -> IO ()
readMsg settings threadId = do
    let sock = sessionSocket settings
        senderName = sessionName settings
    response <- recv sock 1024
    readCmd response sock threadId
    
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStrLn $ senderName ++ ": " ++ response
    setSGR []
    readMsg settings threadId   

{- sendMsg settings threadId
   PURPOSE:      Send a message to another user
   PRE:          sessionSocket settings is open && threadId is the id of the main thread
   POST:         True
   SIDE-EFFECTS: Get a message from the user, send the message to sessionSocket settings, 
                  execute further IO functions
-}
sendMsg :: SessionSettings -> ThreadId -> IO ()
sendMsg settings threadId = do
    let sock = sessionSocket settings
    msg <- getLine
    send sock msg
    readCmd msg sock threadId
    sendMsg settings threadId

{- readCmd cmd sock threadId
   PURPOSE:  Read and perform a user's command
   PRE:      threadId is the id of the main thread
   POST:     True
   SIDE-EFFECTS: if cmd == "/close" then close sock and kill thread with id threadId
-}
readCmd :: String -> Socket -> ThreadId -> IO ()    
readCmd cmd sock threadId = do
    if cmd == "/close" 
        then do
                shutdown sock ShutdownBoth 
                killThread threadId
        else return ()

{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Tests (not automated)              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
testLocalClient :: IO ()
testLocalClient = testClient "localhost" "3001" "Client"

testLocalHost :: IO ()
testLocalHost = testHost "3001" "Host"

testClient :: IP -> Port -> Name -> IO ()
testClient ip port name = client $ setSessionIP ip $ setSessionName "Host" $ setSessionPort port  $ createSessionSettings (setUserName name (setUserPort "3001" defaultSettings)) 
-- testClient "localhost" "3001" "Adam"

testHost :: Port -> Name -> IO ()
testHost port name = host $ createSessionSettings (setUserName name (setUserPort port defaultSettings))
-- testHost "3001" "Jonas"