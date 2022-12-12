
-----------------[guide to test files]----------------------

savein.map: load from this file
input.txt: list of commands
--
output: history, only prints map
check: expected output
--
saveout.map: saved map file (playerPos, keys, maze)
savecheck.txt: expected saved map

---------------[test commands quick ref]--------------------
be sure you have all the "check" files, "input" files and Main.hs compiled
compiling:
> ghc --make Main.hs
run and comparison:
> ./Main 03_savein.map < 03_input.txt > 03_output.txt
> diff 03_output.txt 03_check.txt
> diff 03_saveout.map 03_savecheck.map

----------[notes, troubleshooting, questions]---------------

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
