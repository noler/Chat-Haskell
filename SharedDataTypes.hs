{- 
  Course:  PKD Spring 2016
  Group:   04
  Authors: Jonas Norlinder, Lotta Åhag, Adam Inersjö
-}
module SharedDataTypes (IP, Port, Name,
                        validIP, validPort, validName) where

import Data.Char(isDigit)
import Data.List.Split(splitOn)
import Test.HUnit


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Data Types              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- 
    REPRESENTATION CONVENTION: 
        - Represents an IPv4-address
    REPRESENTATION INVARIANT:
        - The string is non-empty
        - The string consists only of characters '0'-'9' and '.'
        - Uses dotted decimal notation "a.b.c.d", where a,b,c,d are string representations of
             numbers in the closed range [0,255]
        - Every part (ie. a,b,c,d) has length between 1 and 3. A part longer than 1 can't start with '0'
    EXAMPLES:
        - "192.168.1.1"
        - "0.0.0.0"
        - "255.255.255.255"
    INVALID EXAMPLES:
        - "hello"           not digits and '.', doesn't follow the dotted decimal notation
        - "01.1.1.1"        a part starts with '0' and has length > 1
        - "1.2.3.4.5.6"     too many dot-separated parts
        - "255.255.255.256" part d is not in the range [0,255] (larger than the allowed 32-bit)
 -}
type IP = String

{- 
    REPRESENTATION CONVENTION: 
        - Represents a port number, which consists of an unsigned 16-bit integer
    REPRESENTATION INVARIANT:
        - The string is non-empty
        - The string consists only of the characters '0'-'9'
        - A string longer than 1 can't start with '0'
        - The numerical representation of the string is in the range [0,65535]
    EXAMPLES:
        - "1234"
        - "0"
        - "10101"
    INVALID EXAMPLES:
        - "01"      starts with '0' and is longer than 1
        - "hello"   contains non-numerical characters
        - ""        is empty
        - "65536"   is larger than an unsigned 16-bit integer
 -}
type Port = String

{- 
    REPRESENTATION CONVENTION: 
        - Represents a persons name/user name, may include First and Last name
    REPRESENTATION INVARIANT:  
        . The string is non-empty
        - A name can contain all characters excluding ',', ':' and '\n'
    EXAMPLES:
        - "Hi"
        - "I'm valid.!!?"
    INVALID EXAMPLES:
        - ""                is empty
        - "Am I valid,?"    contains ','
        - "Invalid :("      contains ':'
        - "Just\nWrong,:"   contains ',', ':' and '\n'
 -}
type Name = String


{-%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %                                 Validation Functions              
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-}
{- validIP ip
   PURPOSE:  Check if an IP is valid
   PRE:      True
   POST:     True if ip is valid, False if it isn't
   EXAMPLES: validIP ""             == False
             validIP "192.168.1.1"  == True
             validIP "01.213.13.41" == False
             validIP "1.2.3.4.5"    == False
             validIP "hello"        == False
-}
validIP :: IP -> Bool
validIP ip = splitLen == 4 && all validIpPart splitIP
    where
        splitIP = splitOn "." ip
        splitLen = length splitIP

        validIpPart :: String -> Bool
        validIpPart ""      = False
        validIpPart ['0']   = True
        validIpPart ('0':s) = False
        validIpPart str     = all isDigit str && (read str :: Int) < 256


{- validPort port
   PURPOSE:  Check if a port is valid
   PRE:      True
   POST:     True if port is valid, False if it isn't
   EXAMPLES: validPort "1"     == True
             validPort "hello" == False
             validPort "011"   == False
             validPort "65535" == True
-}
validPort :: Port -> Bool
validPort ""       = False
validPort ['0']    = True
validPort ('0':ps) = False
validPort port     = all isDigit port && (read port :: Int) < 65536


{- validName name
   PURPOSE:  Check if a name is valid
   PRE:      True
   POST:     True if name is valid, False if it isn't
   EXAMPLES: validName "hello"             == True
             validName "Adam Jonas Lotta"  == True
             validName "Adam: Jonas Lotta" == False
             validName ""                  == False
             validName "Hi\nHello"         == False
             validName "Hi, my name is Yu" == False
-}
validName :: Name -> Bool
validName ""   = False
validName name = not $ any (`elem` [',',':','\n']) name

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
-- IP TESTS --
validIPTest1 = TestCase (assertEqual "Valid IP: hello" False (validIP "hello"))
validIPTest2 = TestCase (assertEqual "Valid IP: 192.168.1.100" True (validIP "192.168.1.100"))
validIPTest3 = TestCase (assertEqual "Valid IP: 01.213.13.41" False (validIP "01.213.13.41"))
validIPTest4 = TestCase (assertEqual "Valid IP: 1.2.3.4.5" False (validIP "1.2.3.4.5"))
validIPTest5 = TestCase (assertEqual "Valid IP: 255.255.255.256" False (validIP "255.255.255.256"))

-- PORT TESTS --: 
validPortTest1 = TestCase (assertEqual "Valid Port: hello" False (validPort "hello"))
validPortTest2 = TestCase (assertEqual "Valid Port: 1" True (validPort "1"))
validPortTest3 = TestCase (assertEqual "Valid Port: 01" False (validPort "01"))
validPortTest4 = TestCase (assertEqual "Valid Port: 65536" False (validPort "65536"))

-- NAME TESTS --
validNameTest1 = TestCase (assertEqual "Valid Name: Adam: Jonas Lotta" False (validName "Adam: Jonas Lotta"))
validNameTest2 = TestCase (assertEqual "Valid Name: Adam Jonas Lotta" True (validName "Adam Jonas Lotta"))
validNameTest3 = TestCase (assertEqual "Valid Name: \"\"" False (validName ""))
validNameTest4 = TestCase (assertEqual "Valid Name: hello" False (validName "Hi \n"))
validNameTest5 = TestCase (assertEqual "Valid Name: Hi, my name is Yu" False (validName "Hi, my name is Yu"))

--Credit: adopted code from piazza thread: "Testing in groups"
validIPTests = TestList [validIPTest1, validIPTest2, validIPTest3, validIPTest4, validIPTest5]
testIP = runTestTT validIPTests

validPortTests = TestList [validPortTest1, validPortTest2, validPortTest3, validPortTest4]
testPort = runTestTT validPortTests

validNameTests = TestList [validNameTest1, validNameTest2, validNameTest3, validNameTest4, validNameTest5]
testName = runTestTT validNameTests

testAll = runTestTT (TestList [validNameTests, validPortTests, validIPTests])
