import Test.QuickCheck
import System.Random ()
import System.Environment (getArgs)
import T3 (EstadoJogo(..), posicoesDe, inicializa, jogador, chaves, terminado, move)
import Data.List (isPrefixOf,stripPrefix)

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

{-
- reads one arg (optional), and then iteratively performs the following actions:
    - if has args, load associated map
    - if no args, load "default.map"
    - will continue to call parseCmd until termination.
-}
main :: IO ()
main =  do
        args <- getArgs -- text file name included in args
        let gameState = if null args -- if null, load default, else load whichever
            then loadGame "default.map"
            else loadGame $ head args
        loadedGame <- gameState
        print loadedGame
        cmd <- getLine
        parseCmd cmd loadedGame

{-
- reads a command and a game state, and then parse command correspondingly.
    - valid commands: move <String>, save <FilePath>, load <FilePath>, exit
- runs recursively until "exit" command is called.
-}
parseCmd :: String -> EstadoJogo -> IO()
parseCmd cmd game
        | cmd == "exit" = return () --putStrLn ("kill!")
        -- the rest 
        | "move " `isPrefixOf` cmd = loop $ do return $ move game args `debug` ("comand:" ++ cmd)
        | "save " `isPrefixOf` cmd = loop $ do saveGame game args -- save the game
        | otherwise = loop $ do {putStrLn "Invalid command. Please try again: "; return game}
        where args = drop 5 cmd -- instructions are always 4 letters + " "
              loop instruction = do newGame <- instruction -- specified based on instruction. instruction is an IO a
                                    print newGame -- has to be new game
                                    appendFile "history.txt" (show newGame)
                                    --putStr "gimme a instruction:"
                                    newCmd <- getLine
                                    parseCmd newCmd newGame

{-
- takes the file name and returns a game state.
-}
loadGame :: FilePath -> IO EstadoJogo
loadGame x = do
        content <- readFile x -- auto includes does not exist error report. we good
        let fileData = lines content
        let game = Game {maze = mz,
                         playerPos = read $ head fileData ::(Int,Int),
                         keys = fileData !! 1,
                         portals = posicoesDe mz '@'}
                        where mz = drop 2 fileData
        return game --returns new loaded game

{-
- takes the file name and a game state, saves current game state into the file.
-}
saveGame :: EstadoJogo -> FilePath -> IO EstadoJogo
saveGame game@(Game maze playerPos keys _) saveTo = 
        do writeFile saveTo $ unlines [show playerPos,keys] ++ unlines maze -- save to file
           return game -- returns the same game

{-
savein.map: load from this file
input.txt: list of commands
--
output: history, only prints map
check: expected output
--
saveout.map: saved map file (playerPos, keys, maze)
savecheck.txt: expected saved map

quick ref to debug commands:
./Main 04_savein.map < 04_input.txt > 04_output.txt
diff 04_output.txt 04_check.txt
diff 04_saveout.map 04_savecheck.map
-}

{-
LOOP:
- AWAITS STRING INSTRUCTION
1. instruction parsed as valid move
    - use -> move :: EstadoJogo -> String -> EstadoJogo
    to move all the moves and store it in EstadoJogo
    - stdout prints EstadoJogo
2.  instruction parsed as "load <fileName>" 
    - stdout prints <fileName> EstadoJogo
3.  instruction parsed as "save <fileName>" 
    - saves EstadoJogo to <fileName>
    - stdout prints <fileName> EstadoJogo
4.  instruction parsed as "exit"
    - breaks loop
-}


{-      LETS SEE
- https://stackoverflow.com/questions/61480575/installing-quickcheck
- what does "expose" means, in the sense of "expose module in one's Cabal file"?
-> see screenshot: it seems like in ghci, i could attempt to import packages,
    but sometimes it fails and it will ask me to ":set -package libName".
    i assume this is what "expose" means, but what is the permanent way of doing it?
    bc right now it only performs this action in ghci environment

- difference between Cabal and Stack? what's the recommended version? 
- how to see the various packages that is actually on my computer?
    - ghc-pkg list -v
    - cabal list --installed
- if i were to find a package in hackage i do actually want to use, what's the best way
    to download it? stack? cabal? whuthaudfhak
    - how do i know which packages are in fact install-able using cabal even. 
    - because sometimes i need to just download the files
    - how do i download packages with straight files
    

- https://www.reddit.com/r/haskell/comments/ivesrj/i_am_trying_to_install_quickcheck_using_cabal_but/
    - mentions "repl"

- https://cabal.readthedocs.io/en/3.8/cabal-commands.html 
    - commands document
-}