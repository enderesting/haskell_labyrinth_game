-- Autor: Yichen Cao        FC58165
--        Gonçalo Fernandes FC58194

module Tests
  ( prop_move_door_without_key,
    prop_move_picks_key,
    prop_move_exists,
    prop_move_portals,
    prop_move_number_of_doors,
    prop_move_keys,
    prop_move_limits,
    prop_move_dimensions,
  )
where

import T3 (EstadoJogo (..), inicializa, move, posicoesDe)
import Test.QuickCheck (Arbitrary (arbitrary), Property, choose, elements, suchThat)
-- import T1 (labirintosGen,labirintos5,contaCaracteres) -- deprecated
import Test.QuickCheck.Gen (generate, vectorOf)
import Test.QuickCheck.Property ((==>))

-- import Test.QuickCheck.Test (quickCheck)

newtype Movimentos = Movimentos String

instance Show Movimentos where
  show (Movimentos dir) = dir

instance Arbitrary Movimentos where
  arbitrary = do
    size <- choose (1, 5)
    mov <- vectorOf size $ elements "udlr"
    return $ Movimentos mov

instance Arbitrary EstadoJogo where
  arbitrary = do
    col <- choose (4, 7)
    row <- choose (4, 7)
    -- maze <- elements labirintos5
    let rndGut =
          suchThat
            (vectorOf row (vectorOf col items))
            ( \x ->
                (countAppearance '@' x == 0 || countAppearance '@' x == 2)
                  && countAppearance 'S' x == 1
                  && countAppearance 'F' x == 1
                  && countAppearance 'a' x >= 0
                  && countAppearance 'b' x >= 0
                  && countAppearance 'c' x >= 0
                  && countAppearance 'A' x >= 0
                  && countAppearance 'B' x >= 0
                  && countAppearance 'C' x >= 0
            )
    maze <- rndGut
    let borderify = [replicate (col + 2) '*'] ++ ["*" ++ mazeLine ++ "*" | mazeLine <- maze] ++ [replicate (col + 2) '*']
    return $ inicializa borderify
    where
      items = elements "          ****SF@ABCabc"

-- Counts how many times a character appears in a Maze
countAppearance :: Char -> [String] -> Int
countAppearance c str = length $ filter (== c) (unlines str)

-- Counts how many times a character appears in a line
countAppearanceLine :: Char -> String -> Int
countAppearanceLine c str = length $ filter (== c) str

-- Checks if the dimensions of the game stay the same after moving
prop_move_dimensions :: EstadoJogo -> Movimentos -> Bool
prop_move_dimensions game@(Game maze playerPos keys portals) (Movimentos dir) = length maze == length moved_maze && length (head maze) == length (head moved_maze)
  where
    moved_maze = T3.maze $ move game dir

-- Checks if the player stays in bounds after moving
prop_move_limits :: EstadoJogo -> Movimentos -> Bool
prop_move_limits game@(Game maze playerPos keys portals) (Movimentos dir) = moved_player `notElem` filter condition [(x, y) | x <- [0 .. n], y <- [0 .. m]]
  where
    moved = move game dir
    moved_player = T3.playerPos moved
    n = length (T3.maze moved) -1
    m = length (head (T3.maze moved)) -1
    condition = \(x, y) -> x `elem` [0, n] || y `elem` [0, m]

-- Checks if the number of keys the player doesnt decrease after moving
prop_move_keys :: EstadoJogo -> Movimentos -> Bool
prop_move_keys game@(Game maze playerPos keys portals) (Movimentos dir) = length keys <= length moved_keys
  where
    moved_keys = T3.keys $ move game dir

-- Checks if the number of doors after a move doesnt increase
prop_move_number_of_doors :: EstadoJogo -> Movimentos -> Bool
prop_move_number_of_doors game@(Game maze playerPos keys portals) (Movimentos dir) = length doors >= length moved_doors
  where
    moved_maze = T3.maze $ move game dir
    doors = doorPositions maze
    moved_doors = doorPositions moved_maze

doorPositions :: [String] -> [(Int, Int)]
doorPositions xs = concatMap (posicoesDe xs) "ABC"

keyPositions :: [String] -> [(Int, Int)]
keyPositions xs = concatMap (posicoesDe xs) "abc"

-- Checks if the portals stayed consistent after moving
prop_move_portals :: EstadoJogo -> Movimentos -> Bool
prop_move_portals game@(Game maze playerPos keys portals) (Movimentos dir) = portals == moved_portals
  where
    moved_portals = T3.portals (move game dir)

-- Checks if the player still exists after moving
prop_move_exists :: EstadoJogo -> Movimentos -> Bool
prop_move_exists game@(Game maze playerPos keys portals) (Movimentos dir) = not $ null moved_player
  where
    moved_player = T3.playerPos (move game dir)

-- Checks if the keys array increases when the player picks up a key
prop_move_picks_key :: EstadoJogo -> Movimentos -> Property
prop_move_picks_key game@(Game maze (x, y) keys portals) (Movimentos dir) = (positions && directions) ==> length keys <= length moved_keys
  where
    key_coords = keyPositions maze
    moved_keys = T3.keys (move game dir)
    positions = (x - 1, y) `elem` key_coords
    directions = countAppearanceLine 'u' dir >= countAppearanceLine 'd' dir && all (\x -> x == 'u' || x == 'd') dir

-- Checks if the player stays in place when moving against a locked door
prop_move_door_without_key :: EstadoJogo -> Movimentos -> Property
prop_move_door_without_key game@(Game maze playerPos keys portals) (Movimentos dir) = no_keys ==> playerPos `notElem` moved_door_coords
  where
    moved = move game dir
    moved_player = T3.playerPos moved
    moved_door_coords = doorPositions $ T3.maze moved
    no_keys = countAppearance 'a' maze == 0 && countAppearance 'b' maze == 0 && countAppearance 'c' maze == 0
