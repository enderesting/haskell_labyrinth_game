module Tests
  ( prop_move_dimensions,
    prop_move_limits,
    prop_move_number_of_doors,
  )
where

import T3 (EstadoJogo (..), inicializa, move, placeAt, posicoesDe)
import Test.QuickCheck (Arbitrary (arbitrary), Property, choose, elements)
import Test.QuickCheck.Gen (vectorOf)
import Test.QuickCheck.Property ((==>))
import Test.QuickCheck.Test (quickCheck)

-- NOT SURE HOW TO USE IT CUS AM DUM DUM
newtype Movimentos = Movimentos String

instance Show Movimentos where
  show (Movimentos dir) = dir

instance Arbitrary Movimentos where
  arbitrary = do
    size <- choose (4, 10)
    mov <- vectorOf size $ elements "udlr"
    return $ Movimentos mov

instance Arbitrary EstadoJogo where
  arbitrary = do
    columns <- choose (4, 10)
    rows <- choose (4, 10)
    interior <- elements (replicate 16 " " ++ ["A", "B", "C", "*"])
    line <- elements ["*" ++ concat (replicate rows interior) ++ "*"]
    maze <- elements [replicate columns "*" ++ replicate (rows -2) line ++ replicate columns "*"]

    return $ inicializa maze

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
    condition = \(x, y) -> x == 0 || y == 0 || (x /= 0 && y == m) || (y /= 0 && x == n)

-- Checks if the number of keys the player has doesnt decrease after moving
prop_move_keys :: EstadoJogo -> Movimentos -> Bool
prop_move_keys game@(Game maze playerPos keys portals) (Movimentos dir) = length keys >= length moved_keys
  where
    moved_keys = T3.keys $ move game dir

-- Checks if the number of doors after a move doesnt increase
prop_move_number_of_doors :: EstadoJogo -> Movimentos -> Bool
prop_move_number_of_doors game@(Game maze playerPos keys portals) (Movimentos dir) = length doors <= length moved_doors
  where
    moved_maze = T3.maze $ move game dir
    doors = doorPositions maze
    moved_doors = doorPositions moved_maze

doorPositions :: [String] -> [(Int, Int)]
doorPositions xs = concat [posicoesDe xs x | x <- ['A', 'B', 'C']]

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
prop_move_picks_key game@(Game maze (x, y) keys portals) (Movimentos dir) = (fst key_coords == x + 1 && snd key_coords == y && head dir == 'd') ==> length keys <= length moved_keys
  where
    key_coords = head $ posicoesDe maze 'a'
    moved_keys = T3.keys (move game dir)

-- Checks if the player stays in place when moving against a locked door
prop_move_door_without_key :: EstadoJogo -> Movimentos -> Property
prop_move_door_without_key game@(Game maze playerPos keys portals) (Movimentos dir) = (null keys && fst door_coords == fst playerPos + 1 && snd door_coords == snd playerPos && head dir == 'd') ==> playerPos == moved_player
  where
    door_coords = head $ posicoesDe maze 'A'
    moved_player = T3.playerPos (move game dir)
