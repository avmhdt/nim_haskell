{-
Trabalho 2 Linguagem de Programação
Autores:
José Santos Sá Carvalho - 201665557C
Júlia Almeida Valadares - 201765562AC
-}

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe
import System.Random
import Control.Monad.IO.Class

-- Jogador: One ou computador
data Player = One | Computer deriving (Show, Eq)

-- Muda o jogador para o próximo
change :: Player -> Player
change One = Computer
change Computer = One

-- Board (Tabuleiro) é uma sequencia de Ints.
type Board = Seq.Seq Int

-- Tabuleiro inicial
initialBoard :: Board
initialBoard = Seq.fromList [1, 3, 5, 7]

-- Checa se um movimento é possível, e retorna o tabuleiro resultante
move :: Board -> (Int, Int) -> Maybe Board
move board (row, palitos)
  | and [(Seq.index board row) >= palitos,
          row < 4] = Just (Seq.adjust (\x -> x - palitos) row board)
  | otherwise = Nothing

-- Transforma o tabuleiro em uma tabela de asteriscos
display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (palitos board))
                where numbers = ["1. ", "2. ", "3. ", "4. "]
                      palitos board = [(concat . take n) (repeat "* ")
                                    | n <- Fol.toList board]

-- Os próximos métodos controlam IO
main :: IO ()
main = do putStrLn "**** Nim ****"
          putStrLn "Dificuldade: Fácil(f) ou Difícil(d)? "
          dif <- getChar
          temp <- getLine
          nim (dif)

-- Método nim printa o tabuleiro inicial e recebe a dificuldade
nim :: Char -> IO ()
nim dif = do putStrLn (display initialBoard)
             if dif == 'f'
                then turn initialBoard One dif
                else do if dif == 'd'
                           then turn initialBoard Computer dif
                           else putStrLn "Dificuldade Inválida"

-- Retorna um número aleatório entre a e b
getRandN :: (Random a, RandomGen b) => (a, a) -> b -> a
getRandN (a, b) g = fst $ randomR (a, b) g

-- Estratégia vencedora:
-- Quantos palitos retirar?
howManySticks :: Board -> Int -> Int -> Int -> [Int]
howManySticks board row maxPalitos palitos | row > 4 = [0, 0]
                                           | palitos > maxPalitos = howManySticks board (row+1) ((Fol.toList board) !! row) 1 
                                           | allEven (sum [toBinInt x | x <- Fol.toList (fromJust (move board ((row-1), palitos)))]) = [row, palitos]  
                                           | otherwise = howManySticks board row maxPalitos (palitos+1)

-- All even digits
allEven :: Int -> Bool
allEven x = all even (digits x)

-- Digits of an integer:
digits :: Integral x => x -> [x]
digits 0 = [0]
digits x = digs x

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Integer to binary integer
toBinInt :: Int -> Int
toBinInt x = binListToInt (toBin x)

-- Digits list to integer:
binListToInt :: [Int] -> Int
binListToInt [] = 0
binListToInt (x:xs) = (x * (10 ^ length xs)) + binListToInt xs

-- Integer to binary:
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (helper n)

helper :: Int -> [Int]
helper 0 = []
helper n = let (q,r) = n `divMod` 2 in r : helper q

-- Loop principal do jogo. Pergunta qual vai ser o movimento, checa se é válido, 
-- e faz o movimento, recursivamente
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
                                              then do --putStrLn "Nível Difícil não implementado ainda"
                                                      -- let sumPalitosBin = sum [toBinInt x | x <- Fol.toList board]
                                                      let rowPalitos = howManySticks board 1 ((Fol.toList board) !! 0) 1
                                                      if rowPalitos !! 0 == 0 -- se nao conseguiu estratégia vencedora, faz aleatório
                                                         then do g <- newStdGen
                                                                 let row = getRandN (1, 4) g 
                                                                 let palitos = (Fol.toList board) !! (row-1)
                                                                 if palitos == 0 
                                                                    then turn board player dif
                                                                    else do g <- newStdGen
                                                                            let palitos = getRandN (1, (Fol.toList board) !! (row-1)) g
                                                                            putStrLn ("\n" ++ (show player) ++ " - aleatório")
                                                                            putStrLn ("Linha: " ++ (show row) ++ "\nPalitos: " ++ (show palitos))
                                                                            let newBoard = move board ((row - 1), palitos)
                                                                            if newBoard == Nothing
                                                                               then do putStrLn ("Movimento inválido")
                                                                                       turn board player dif
                                                                               else isOver (fromJust newBoard) (change player) dif 
                                                         else do
                                                                 putStrLn ("\n" ++ (show player) ++ " - estratégia vencedora!")
                                                                 putStrLn ("Linha: " ++ (show (rowPalitos !! 0)) ++ "\nPalitos: " ++ (show (rowPalitos !! 1)))
                                                                 let newBoard = move board ((rowPalitos !! 0) - 1, rowPalitos !! 1)
                                                                 let sumPalitos = sum [toBinInt x | x <- Fol.toList (fromJust newBoard)]
                                                                 putStrLn ("sumPalitos = " ++ (show sumPalitos))
                                                                 if newBoard == Nothing
                                                                    then do putStrLn ("Movimento inválido")
                                                                            turn board player dif
                                                                    else isOver (fromJust newBoard) (change player) dif
                                              else putStrLn "Dificuldade Inválida"

-- Checa se o tabuleiro está vazio, e se o jogo acabou e qual o vencedor
isOver :: Board -> Player -> Char -> IO()
isOver board player dif = do if board == Seq.fromList [0, 0, 0, 0]
                             then putStrLn ("Player " ++ (show (change player))
                                            ++ " venceu!")
                             else do putStrLn ""
                                     putStrLn (display board)
                                     turn board player dif
