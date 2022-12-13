-- Autor: Yichen Cao        FC58165
--        Gonçalo Fernandes FC58194

module T3
(
    EstadoJogo(..), posicoesDe, inicializa, jogador, chaves, terminado, move, placeAt,direction
)where

----------------------- 

import Data.Char (toLower,isLower,isUpper,toUpper,isAsciiLower,isAsciiUpper)
import Data.List (elemIndex, intercalate,sort)

----------------------- 
--nep's find char's pos code :) it's cleaner than mine
posicoesDe :: Maze -> Char -> [Coords]
posicoesDe xs c = filter condition [(x, y) | x <- [1 .. n], y <- [1 .. m]]
  where
    n = length xs -1
    m = length (head xs) -1
    condition = \(x, y) -> findAt xs (x,y) == c

--finds the char associated with a coords on a maze
findAt :: Maze -> Coords -> Char
findAt maze (x,y) =  mazeLine !! y
                    where mazeLine = maze !! x

-----------------------

data EstadoJogo = Game {maze :: Maze, playerPos :: Coords,
                        keys :: String, portals :: [Coords]}
data KeyState = Obtained | Missing deriving (Show, Eq)  
type Maze = [String]
type Coords = (Int,Int)
type DoorCoords = Coords

--A1: given valid lab, initiate gameState (no key, player @ S)
inicializa :: Maze -> EstadoJogo
inicializa xs = Game {maze = xs, playerPos = head $ posicoesDe xs 'S',
                      keys = [], portals = posicoesDe xs '@'}

--Aux1: NEWCHAR is placed at an position
placeAt :: Maze -> Coords -> Char -> Maze
placeAt maze (a,b) c = foldr (\xs acc ->
                        if xs `elemIndex` maze == Just a 
                            then replaceOld xs:acc
                            else xs:acc) [] maze
                        where replaceOld xs = let (x,_:ys) = splitAt b xs in x ++ c : ys


--A2: find the player position
jogador :: EstadoJogo -> Coords
jogador (Game _ playerPos _ _ ) = playerPos

--A3: find the keys acquired 
chaves :: EstadoJogo -> String
chaves (Game _ _ keys _) = keys

--A4: is the game finished? (player at end)
terminado :: EstadoJogo -> Bool
terminado (Game maze playerPos _ _) = head (posicoesDe maze 'F') == playerPos

--A5: class show
instance Show EstadoJogo where
    show game@(Game maze playerPos keys _) = unlines newMaze ++ "chaves: " ++ keys
        where newMaze = placeAt maze playerPos 'P'
        -- P is the only one that's put into the maze here. everything else is static
        -- they are changed AS move happens

--Aux2: find the vector of the movement based on direction
direction :: Char -> Coords
direction c
        | c == 'd' = (1,0)
        | c == 'u' = (-1,0)
        | c == 'l' = (0,-1)
        | c == 'r' = (0,1)
        | otherwise = error "Oops! Not an available direction."

--Aux3: finds the position after a move
findDest :: Char -> Coords -> Coords
findDest c (a,b)= (fst dir + a, snd dir + b)
                    where dir = direction c

--B1: move the player and changing its state following steps given
move :: EstadoJogo -> String -> EstadoJogo
move game@(Game maze playerPos keys portals) = foldl movePerStep game

--Aux4: move one step at a time
movePerStep :: EstadoJogo -> Char -> EstadoJogo
movePerStep game@(Game maze playerPos keys portals) dir
        | symbol `elem` [' ','F','S'] = Game maze nextPos keys portals --based move
        | isAsciiLower symbol = Game cleanMaze nextPos newKeys portals -- find key, pick it up
        | isAsciiUpper symbol && toLower symbol `elem` keys
            = Game cleanMaze nextPos keys portals -- find door has key, open door
        | symbol == '@' = Game maze teleportedPos keys portals --teleport you to the other portal
        | otherwise = game --no move, nothing changed
        where symbol = findAt maze nextPos
              nextPos = findDest dir playerPos
              teleportedPos = head $ filter (/= nextPos) portals
              cleanMaze = placeAt maze nextPos ' '
              newKeys = sort $ symbol : keys