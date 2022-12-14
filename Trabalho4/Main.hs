-- Autor: Yichen Cao        FC58165
--        Gonçalo Fernandes FC58194

-- import System.Random ()

import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import T3 (EstadoJogo (..), move, posicoesDe)
import Test.QuickCheck
import Tests
  ( prop_move_dimensions,
    prop_move_door_without_key,
    prop_move_exists,
    prop_move_keys,
    prop_move_limits,
    prop_move_number_of_doors,
    prop_move_picks_key,
    prop_move_portals,
  )

-- import Debug.Trace (trace)
-- debug :: c -> String -> c
-- debug = flip trace

{-
- reads one arg (optional), and then iteratively performs the following actions:
    - if has args, load associated map
    - if no args, load "default.map"
    - will continue to call parseCmd until termination.
-}
main :: IO ()
main = do
  -- regular run: args = 1, fileExists, not -t
  -- test run: args =1, -t--
  arguments <- getArgs -- text file name included in args
  processArgs arguments

-- parse arguments, checks their validity and return with appropriate response
processArgs :: [String] -> IO ()
processArgs args
  | length args > 1 = putStrLn ("Too many args. \n" ++ helpMenu)
  | file == "-t" = runTest
  | file == "help" = putStrLn helpMenu
  | fileExists <- doesFileExist $ head args = loadFiles file
  | otherwise = putStrLn ("\"" ++ file ++ "\" does not exist." ++ helpMenu) --loadGame $ head args
  where
    file = head args

-- runs quickcheck tests
runTest :: IO ()
runTest = do
  putStrLn "Checks if the dimensions of the game stay the same after moving."
  quickCheck prop_move_dimensions
  putStrLn "Checks if the player stays in bounds after moving."
  quickCheck prop_move_limits
  putStrLn "Checks if the number of keys the player doesnt decrease after moving."
  quickCheck prop_move_keys
  putStrLn "Checks if the player still exists after moving."
  quickCheck prop_move_exists
  putStrLn "Checks if the portals stayed consistent after moving."
  quickCheck prop_move_portals
  putStrLn "Checks if the number of doors after a move doesnt increase."
  quickCheck prop_move_number_of_doors
  putStrLn "Checks if the player stays in place when moving against a locked door."
  quickCheckWith stdArgs {maxDiscardRatio = 100} prop_move_door_without_key
  putStrLn "Checks if the keys array increases when the player picks up a key."
  quickCheckWith stdArgs {maxDiscardRatio = 100} prop_move_picks_key

-- loads a file, assuming it is correctly formatted.
-- Then continue to ask for another command
loadFiles :: String -> IO ()
loadFiles fileName = do
  let gameState =
        if null fileName -- if null, load default, else load whichever
          then loadGame "default.map"
          else loadGame fileName
  loadedGame <- gameState
  print loadedGame
  cmd <- getLine
  parseCmd cmd loadedGame

-- the help menu which is printed when incorrect or when the "help" is called
helpMenu :: String
helpMenu =
  unlines
    [ "--------------------[Help Menu]--------------------",
      "    ./Main help      -- Shows this command menu.",
      "    ./Main [file]    -- Runs game with custom map.",
      "    ./Main           -- Runs game with default map.",
      "    ./Main -t        -- Runs tests for the game.",
      "---------------------------------------------------"
    ]

{-
- reads a command and a game state, and then parse command correspondingly.
    - valid commands: move <String>, save <FilePath>, load <FilePath>, exit
- runs recursively until "exit" command is called.
-}
parseCmd :: String -> EstadoJogo -> IO ()
parseCmd cmd game
  | cmd == "exit" = return () --putStrLn ("kill!")
  | "move " `isPrefixOf` cmd = loop $ do return $ move game args --`debug` ("comand:" ++ cmd)
  | "save " `isPrefixOf` cmd = loop $ do saveGame game args -- save the game
  | "load " `isPrefixOf` cmd = loop $ do
    --loadGame args
    fileExists <- doesFileExist args
    if fileExists
      then do loadGame args
      else do
        putStrLn ("\"" ++ args ++ "\" does not exist. Try again")
        return game
  | otherwise = loop $ do putStrLn "Invalid command. Please try again: "; return game
  where
    args = drop 5 cmd -- instructions are always 4 letters + " "
    loop instruction = do
      newGame <- instruction -- specified based on instruction. instruction is an IO a
      print newGame -- has to update game
      newCmd <- getLine
      parseCmd newCmd newGame

{-
- takes a file name and returns a game state.
- Assumes file always exists when using this function
-}
loadGame :: FilePath -> IO EstadoJogo
loadGame x = do
  content <- readFile x
  let fileData = lines content
  let game =
        Game
          { maze = mz,
            playerPos = read $ head fileData :: (Int, Int),
            keys = fileData !! 1,
            portals = posicoesDe mz '@'
          }
        where
          mz = drop 2 fileData
  return game --returns new loaded game

-- takes the file name and a game state, saves current game state into the file.
saveGame :: EstadoJogo -> FilePath -> IO EstadoJogo
saveGame game@(Game maze playerPos keys _) saveTo =
  do
    writeFile saveTo $ unlines [show playerPos, keys] ++ unlines maze -- save to file
    return game -- returns the same game