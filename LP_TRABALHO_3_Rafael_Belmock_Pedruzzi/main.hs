-- Linguagens de Programação - Prof. Flavio Varejão - 2019-1
-- Terceiro trabalho de implementação
--
-- Aluno: Rafael Belmock Pedruzzi
--
-- main.hs: módulo main

import System.IO
import Data.Char
import Text.Printf
import Numeric

import TrabPoint

-- Funções para conversão dos números lidos como String em Float:
-- Converte uma String para Float:
leiaFloat :: String -> Float
leiaFloat = read
-- Converte uma lista de Strings para Float:
converteSF xs = map leiaFloat xs

-- Função que garante a impressão de 4 casas decimais:
show4Decimals :: Float -> String
show4Decimals x = showFFloat (Just 4) x ""

-- Função main. Ponto de partida do programa:
main = do entrada <- readFile "entrada.txt" -- Abrindo arquivo entrada.txt
          dist <- readFile "distancia.txt" -- Abrindo arquivo distancia.txt
          putStrLn $ show $ addIndice $ map converteSF $ map words $ lines entrada
          putStrLn $ show $ montaGrupos (leiaFloat dist) $ addIndice $ map converteSF $ map words $ lines entrada
          --putStrLn $ show $ map show4Decimals $ map leiaFloat $ words entrada

-- Função que adiciona um índice ao inicio de cada ponto da listade pontos:
addIndice xss = addIndiceAux xss 1

addIndiceAux [] i = []
addIndiceAux (xs:xss) i = (i : xs) : addIndiceAux xss (i+1)