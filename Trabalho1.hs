--Autor: Yichen Cao FC58165

--counts one char's frequency from a [string]
contaCaracteres :: Char -> [String] -> Int
contaCaracteres c maze = sum[contaCharFromString c str| str <- maze]

--counts one char's frequency from a string
contaCharFromString :: Char -> String -> Int
contaCharFromString c xs = sum[if x == c then 1 else 0| x <- xs]

--lists all possible lines of a maze, occurrence of F and S <= 1
possibleLines :: [String]-- could have 1 or 0 F and S ?
possibleLines = [ "*" ++ xs ++ "*" |let dict = ["*"," ","S","F"], a<-dict, b<-dict, c<-dict, let xs = a ++ b ++ c, contaCharFromString 'S' xs <= 1, contaCharFromString 'F' xs <= 1]

--generates all possible combination of maze
mazeGuts :: [[String]] --YEAH! get spooky
mazeGuts = [xs |let pl = possibleLines, l1 <- pl, l2 <- pl, l3 <- pl, let xs = [l1] ++ [l2] ++ [l3], contaCaracteres 'S' xs == 1, contaCaracteres 'F' xs == 1]

--generates the borders for every combination of maze
labirintos5 :: [[String]]
labirintos5 = [["*****"] ++ line ++ ["*****"] | line <- mazeGuts]
