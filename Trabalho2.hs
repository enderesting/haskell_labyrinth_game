-- Autor: Yichen Cao FC58165
--import Debug.Trace

lab1 :: [String]
lab1 = ["*****","*S*F*","* * *","*   *","*****"]
lab2 :: [String]
lab2 = ["*****","*   *","*   *","* SF*","*****"]
lab3 :: [String]
lab3 = ["*****","*S***","*****","***F*","*****"]
--debug = flip trace
-- receives a valid maze and return the initial position (aka position of 'S')
-- (i,j) > line, column from top left, starting with zero
posicaoInicial :: [String] -> (Int,Int)
posicaoInicial = findCharPos 'S'

-- returns -1 if its not there
-- only works if symbol c is unique in the whole string
findCharY :: Char -> String -> Int
findCharY c mazeLine = snd $ foldl (\(acc,y) x -> if x == c then (acc+1,acc) else (acc+1,y)) (0,-1) mazeLine

-- returns -1 if its not there
-- only works if symbol c is unique in the maze
findCharX :: Char -> [String] -> Int
findCharX c maze = fst $ foldl (\(x,acc) xs -> if c `elem` xs then (acc,acc+1) else (x,acc+1)) (-1,0) maze

-- returns the x,y coordinates of char c in a maze
-- only works if symbol c is unique
findCharPos :: Char -> [String] -> (Int,Int)
findCharPos c maze = (x, y)
                    where x = findCharX c maze
                          y = findCharY c $maze!!x


-- receives a coord and return a list of coords [up,left,right,bottom]
-- thats NOT occupied by a wall && a valid coord on the maze
vizinhos :: [String] -> (Int,Int) -> [(Int,Int)]
vizinhos maze (x,y) = foldr (\x acc -> if validPos maze x && findAt maze x /= '*' then x:acc else acc) [] pos
                        where pos = [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]

-- check if a coord is a valid coord on the maze
validPos :: [String] -> (Int,Int) -> Bool
validPos maze (x,y) = x > 0 && y > 0 && x < length maze && y < length (head maze)

-- finds the char associated with a coords on a maze
findAt :: [String] -> (Int,Int) -> Char
findAt maze (x,y) =  mazeLine !! y
                    where mazeLine = maze !! x

-- searchPath from S to F
-- list out all paths starting from S. ending condition: no more vizinha OR is F
procuraCaminho :: [String] -> Bool
procuraCaminho maze = searchPath maze start (vizinhos maze start) []
                    where start = posicaoInicial maze

searchPath :: [String] -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> Bool
searchPath maze coords [] _ = findCharPos 'F' maze == coords || False --`debug` "all used up"
searchPath maze coords freeCoords usedCoords = (findCharPos 'F' maze == coords) ||
                                                searchPath maze newCoords (newFree++tail freeCoords) newUsed --`debug` ("newCoord:" ++ show newCoords ++ "  newFree:" ++ show (newFree++tail freeCoords) ++ "  newUsed:" ++ show newUsed)
                                                where newUsed = coords : usedCoords
                                                      newFree = foldr (\x acc -> if x `elem` newUsed then acc else x:acc) [] (vizinhos maze newCoords)
                                                      newCoords = head freeCoords