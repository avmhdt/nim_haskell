import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe
import System.Random
import Control.Monad.IO.Class

-- Player type is used to take control of the turns and who wins
data Player = One | Computer deriving (Show, Eq)

-- Change player returns the player that is NOT the one passed as argument
change :: Player -> Player
change One = Computer
change Computer = One

-- Board is an alias for a Sequence of Ints, as they allow us to update single
-- elements in easily.
type Board = Seq.Seq Int

-- This is the initial board structure
initialBoard :: Board
initialBoard = Seq.fromList [1, 3, 5, 7]

-- The move method checks if the a movement can be executed and returns the
-- updated board in case it is possible
move :: Board -> (Int, Int) -> Maybe Board
move board (row, palitos)
  | and [(Seq.index board row) >= palitos,
          row < 4] = Just (Seq.adjust (\x -> x - palitos) row board)
  | otherwise = Nothing

-- The display methods transforms a Board into a nice, enumerated String of
-- asterisks
display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (palitos board))
                where numbers = ["1. ", "2. ", "3. ", "4. "]
                      palitos board = [(concat . take n) (repeat "* ")
                                    | n <- Fol.toList board]

-- The next methods are the ones that control IO
main :: IO ()
--main = nim
main = do putStrLn "**** Nim ****"
          putStrLn "Dificuldade: Fácil(f) ou Difícil(d)? "
          dif <- getChar
          temp <- getLine
          nim (dif)

-- Main method welcomes the player, displays the initial board and calls the
-- first turn
nim :: Char -> IO ()
nim dif = do putStrLn (display initialBoard)
             if dif == 'f'
                then turn initialBoard One dif
                else do if dif == 'd'
                           then turn initialBoard Computer dif
                           else putStrLn "Dificuldade Inválida"

getRandN (a, b) g = fst $ randomR (a, b) g

-- The turn method displays the player and asks for a movement, then checks if
-- there was a problem performing that movement and continues the game. This is
-- the main game loop

turn :: Board -> Player -> Char -> IO ()
turn board player dif = do if player == One
                           then do putStrLn ("\nPlayer " ++ (show player) ++ "!")
                                   putStrLn "Linha para remover palitos: " 
                                   row <- getLine
                                   putStrLn "Remover quantos palitos? "
                                   palitos <- getLine
                                   let newBoard = move board ((read row) - 1, read palitos)
                                   if newBoard == Nothing
                                      then do putStrLn ("Movimento inválido")
                                              turn board player dif
                                      else isOver (fromJust newBoard) (change player) dif
                           else do if dif == 'f'
                                      then do g <- newStdGen
                                              let row = getRandN (1, 4) g 
                                              let palitos = (Fol.toList board) !! (row-1)
                                              if palitos == 0 
                                                 then turn board player dif
                                                 else do g <- newStdGen
                                                         let palitos = getRandN (1, (Fol.toList board) !! (row-1)) g
                                                         putStrLn ("\n" ++ (show player))
                                                         putStrLn ("Linha: " ++ (show row) ++ "\nPalitos: " ++ (show palitos))
                                                         let newBoard = move board ((row - 1), palitos)
                                                         if newBoard == Nothing
                                                            then do putStrLn ("Movimento inválido")
                                                                    turn board player dif
                                                            else isOver (fromJust newBoard) (change player) dif 
                                      else do if dif == 'd' 
                                              then putStrLn "Nível Difícil não implementado ainda"
                                              else putStrLn "Dificuldade Inválida"

-- isOver checks if the Board is empty, and checks whether the game is over or
-- the next turn must be called
isOver :: Board -> Player -> Char -> IO()
isOver board player dif = do if board == Seq.fromList [0, 0, 0, 0]
                             then putStrLn ("Player " ++ (show player)
                                            ++ " venceu!")
                             else do putStrLn ""
                                     putStrLn (display board)
                                     turn board player dif