--Board representation of ConnectFour
module Board where
  --Creates a mxn board, m columns and n rows. List of lists
  mkBoard :: Int -> Int -> [[Int]]
  mkBoard m n = [createRow n | x <- [1..m]]
  createRow :: Int -> [Int]
  createRow n = [0 | n <- [1..n]]

  mkPlayer :: Int
  mkPlayer = 1
  mkOpponent :: Int
  mkOpponent = 2

  myBoard :: [[Int]]
  myBoard = [[0,0,0,0,0,1], [0,0,0,1,11,12], [0,14,15,16,1,18], [0,20,21,22,23,1], [25,26,27,28,29,30], [31,32,33,34,35,36], [37,38,39,40,41,42]]

  row :: [[Int]] -> Int -> [Int]
  row [] _ = []
  row (h:t) i = h !! (i - 1) : row t i

  rows :: [[Int]] -> [[Int]]
  rows bd = [row bd i | i <- [1 .. height bd]]
    where height bd = length (head bd)

  --Returns the mapped character depending on number n
  playerToChar :: Int -> Char
  playerToChar n
    | n == 1 = 'O'
    | n == 2 = 'X'
    | otherwise = '.'

  --Transform a row from ints to characters
  row2str :: [Int] -> [Char]
  row2str r = foldl (++) "" (map (\p -> (playerToChar p : "" )) r)

  --Transforms all rows from ints to characters
  rows2str :: [[Int]] -> [Char]
  rows2str [] = ""
  rows2str (h:t) = row2str h ++ "\n" ++ rows2str t

  --boardToStr:: (Int -> Char) -> [[t]] -> [Char]
  boardToStr :: (Int -> Char) -> [[Int]] -> [Char]
  boardToStr playerToChar bd = rows2str (rows bd)
    where
      rows2str [] = ""
      rows2str (h:t) = row2str h ++ "\n" ++ rows2str t
        where
          row2str r = foldl (++) "" (map (\p -> (playerToChar p : " ")) r)


  --Drops the given player into the desired slot. 0-based
  dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
  dropInSlot bd i p = dropInSlotHelper bd i p 0
  dropInSlotHelper :: [[Int]] -> Int -> Int -> Int -> [[Int]]
  dropInSlotHelper (h:t) i p curr
    | curr == i = (drops h p):t
    | otherwise = h: (dropInSlotHelper (t) i p (curr + 1))

  --Drops respective slot p into a column
  drops :: [Int] -> Int -> [Int]
  drops (h:t) p = reverse (dropHelper (reverse(h:t)) p)
  dropHelper :: [Int] -> Int -> [Int]
  dropHelper (h:t) p
    | h == 0 = p:t
    | otherwise = (h):dropHelper (t) p

  --Return whether or not the selected slot is available
  isSlotOpen :: [[Int]] -> Int -> Bool
  isSlotOpen bd i
    | i < 0 = False
    | i >= (numSlot bd) = False
    | otherwise = (bd!!i!!0) == 0

  --It gives the numbers of columns of a board bd
  numSlot :: [[Int]] -> Int
  numSlot bd = length(bd)

  --It gives the number of rows of a board bd
  numRows :: [[Int]] -> Int
  numRows bd = length (head bd)
  --It return whether or not the board bd is full
  isFull :: [[Int]] -> Bool
  isFull bd = length (filter (\x->x == 0) (concat bd)) == 0

  --Checks if a given state of the board bd will be won by player p
  isWonBy :: [[Int]] -> Int -> Bool
  isWonBy bd p = winHorizontally bd p 1 || winVertically bd p 0 || winDiagonallyLeft bd p || winDiagonallyRight bd p

  --Methods used for different checks

  --It checks True/False if there are four consecutive numbers in a list that are the same
  --as the provided "p"
  fourEquals :: [Int] -> Int -> Bool
  fourEquals (h:t) p
    | length (h:t) < 4 = False
    | checkSame (take 4 (h:t)) p == True = True
    | otherwise = fourEquals t p

  --Checks if all the numbers of a given list are the same "p"
  checkSame :: [Int] -> Int -> Bool
  checkSame (h:t) p = if p /= h then False else checkSame t p
  checkSame [] p = True

  --Checks of Connect Four

  --Checks if a user has won by connecting four pieces horizontally _
  winHorizontally :: [[Int]] -> Int -> Int -> Bool
  winHorizontally bd p i
    | i > length (head bd) = False
    | fourEquals (row bd i) p == True = True
    | otherwise = winHorizontally bd p (i+1)

  --Checks if a user has won by connecting four pieces vertically |
  winVertically :: [[Int]] -> Int -> Int -> Bool
  winVertically bd p i
    | i >= length(bd) = False
    | fourEquals (bd!!i) p == True = True
    | otherwise = winVertically bd p (i+1)

  --Checks if a user has won by connecting four pieces diagonally from bottom to right /
  winDiagonallyRight :: [[Int]] -> Int -> Bool
  winDiagonallyRight bd p = helperLists [take4DR bd i j | i <- [0..((numSlot bd)-4)], j <- [0..((numRows bd)-4)]] p 0
  --Used in winDiagonallyRight and winDiagonallyLeft
  helperLists :: [[Int]] -> Int -> Int -> Bool
  helperLists (h:t) p i
    | i >= length(h:t) = False
    | fourEquals ((h:t)!!i) p == True = True
    | otherwise = helperLists (h:t) p (i+1)

  --This is to select t
  take4DR :: [[Int]] -> Int -> Int -> [Int]
  take4DR bd i j = [bd !! (i + x) !! ((numRows bd) - j - 1 - x) | x <- [0..3]]

  --Checks if a user has won by connecting four pieces diagonally from bottom to right \
  winDiagonallyLeft :: [[Int]] -> Int -> Bool
  winDiagonallyLeft bd p = helperLists [take4LR bd i j | i <- [3..((numSlot bd)-1)], j <- [0..((numRows bd)-4)]] p 0
  take4LR :: [[Int]] -> Int -> Int -> [Int]
  take4LR bd i j = [bd !! (i - x) !! ((numRows bd) - j - 1 - x) | x <- [0..3]]
