--UI part of the code
module MainGame where
import Board
import System.IO
import System.Random

--Returns the mapped character depending on number n
p2c :: Int -> Char
p2c n
  | n == 1 = 'O'
  | n == 2 = 'X'
  | otherwise = '.'

readSlot :: [[Int]] -> Int -> IO(Int)
readSlot bd p = do
  putStr ("Select a slot (1-" ++ show width ++ ") or -1 to quit: ")
  --Line or number, check if number between the range
  line <- getLine
  let realNum = reads line :: [(Int, String)]
  if length realNum == 0
    then readSlotHelper bd p
  else do
    let (x,_) = head realNum
    if x > 0 && x < (width+1) && isSlotOpen bd (x-1)
      then return x
      else do
        if x == (-1)
          then return (-1)
          else readSlotHelper bd p
 where
  width = length bd

readSlotHelper :: [[Int]] -> Int -> IO(Int)
readSlotHelper bd p = do
  putStrLn ("Invalid Input for selecting slot!")
  readSlot bd p

-- 1.Main function that calls the mainGame function of ConnectFour
main :: IO()
main = do
  putStrLn("Welcome to ConnectFour done with Haskell!!")
  mainGame
  putStrLn("Thank you for playing ConnectFour done in Haskell!!")

-- 2.It displays whether the user has won, tie, loose or exited the game
mainGame :: IO()
mainGame = do
  strategyOption <- getStrategy
  let bd = mkBoard 7 6
  gameStatus <- mainGameHelper bd mkPlayer strategyOption
  if gameStatus == -1 then putStrLn("Exiting from game, -1 entered")
  else do
    if gameStatus == 0 then putStrLn("There was a tie, better luck next time to both!")
    else do
      if gameStatus == mkPlayer then putStrLn("Player " ++ (show mkPlayer) ++ " Won!")
      else do
        if gameStatus == mkOpponent
          then putStrLn("Player " ++ (show mkOpponent) ++ " Won!")
          else putStrLn("Status Unknown of game")

--3.It goes with the loop of the game
mainGameHelper :: [[Int]] -> Int -> Int -> IO(Int)
mainGameHelper bd player strategyOption = do
  strategyChosen strategyOption  --UI
  putStrLn(p2c player: "'s turn")
  if isFull bd then return 0 --No one wins
  else do
    if strategyOption == 1 && player == mkOpponent then do
      slot <- (getRandomSlot bd player)
      gameLoop bd slot player strategyOption
      else do
      slot <- (readSlot bd player)
      gameLoop bd slot player strategyOption

strategyChosen :: Int -> IO()
strategyChosen s = do
  if s == 1 then putStrLn("Strategy Chosen: Human- Computer")
  else do putStrLn("Strategy Chosen: Human- Human")

gameLoop :: [[Int]] -> Int -> Int -> Int -> IO(Int)
gameLoop bd slot player strategy = do
  if slot == (-1) then return (-1)
  else do
    let newBd = dropInSlot bd (slot - 1) player
    if isWonBy newBd mkPlayer
      then do
        putStrLn (boardToStr p2c newBd)
        return mkPlayer
      else do
        if isWonBy newBd mkOpponent
          then do
            putStrLn(boardToStr p2c newBd)
            return mkOpponent
          else do
            putStrLn(boardToStr p2c newBd)
            if player == mkPlayer
              then mainGameHelper newBd mkOpponent strategy
              else mainGameHelper newBd mkPlayer strategy

--Asks for the strategy, either to play against a person or a computer
getStrategy :: IO(Int)
getStrategy = do
  putStrLn("Do you want to play against a 1) computer or against 2) person? (Choose 1 or 2)");
  option <- getLine
  let realNum = reads option :: [(Int, String)]
  if option == "1" then return 1
  else do
    if (length(realNum)) == 0
      then getStrategyHelper
      else do
        let (x, _) = head realNum
        if x == 1
          then return 1
          else do
          if x == 2
            then return 2
            else getStrategyHelper

getStrategyHelper :: IO (Int)
getStrategyHelper = do
  putStrLn "Try again with a valip input!"
  getStrategy

--Getting random number from computer
getRandomSlot :: [[Int]] -> Int -> IO(Int)
getRandomSlot bd player = do
  slot <- randomRIO(1,7)
  if isSlotOpen bd (slot-1)
    then return slot
    else getRandomSlot bd player
  

getNum = do
  putStrLn "Enter a positive num"
  line <- getLine
  let parsed = reads line :: [(Integer, String)] in
    if length parsed == 0 then getNum'
    else let (x, _) = head parsed in
      if x > 0 then return x
      else getNum'
    where
      getNum' = do
        putStrLn "Invalid input!"
        getNum