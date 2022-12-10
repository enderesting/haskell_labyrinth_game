import Test.QuickCheck
import System.Random ()
import System.Environment (getArgs)
import T3 (EstadoJogo(..), posicoesDe, inicializa, jogador, chaves, terminado, move)
import Data.List (isPrefixOf)

{-
main do block that
- reads one arg (optional)  (use Maybe?)
    - if has args, load associated map
    - if no args, load "default.map"
-}

main :: IO ()
main =  do
        args <- getArgs -- text file name included in args
        if null args -- if null, load default, else load whichever
            then (do gameState <- loadGame "default.map"
                     print gameState 
                     putStrLn "default invoked")
            else (do gameState <- loadGame $ head args
                     print gameState
                     putStrLn ("loaded:" ++ head args))
        putStrLn "gimme a instruction:"
        cmd <- getLine
        parseCmd cmd
        -- return()

parseCmd :: String -> IO()
parseCmd cmd 
        | cmd == "exit" = putStrLn ("kill!")
        | "move" `isPrefixOf` cmd = loop $ putStrLn ("Moving to" ++ fileName) -- TO DO
        | "load" `isPrefixOf` cmd = loop $ putStrLn ("Loading to "  ++ fileName) -- TO DO
        | "save" `isPrefixOf` cmd = loop $ putStrLn ("Saving to "  ++ fileName) -- TO DO
        | otherwise = error "fucked up my guy"
        where fileName = drop 5 cmd -- instructions are always 4 letters + " "
              loop instruction = do instruction -- specified based on instruction. instruction is an IO a
                                    putStrLn "gimme a instruction:"
                                    newCmd <- getLine
                                    parseCmd newCmd


-- type FilePath = String
-- reads a String, and returns the IO EstadoJogo
loadGame :: FilePath -> IO EstadoJogo
loadGame x = do
        content <- readFile x -- auto includes does not exist error report. we good
        let fileData = lines content
        let game = Game {maze = mz,
                         playerPos = read $ head fileData ::(Int,Int),
                         keys = [],
                         portals = posicoesDe mz '@'}
                        where mz = drop 2 fileData
        return game

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