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

-- Funções para conversão dos números lidos como String em Double:
-- Converte uma String para Double:
leiaDouble :: String -> Double
leiaDouble = read
-- Converte uma lista de Strings para Double:
converteSF xs = map leiaDouble xs

-- Função que garante a impressão de 4 casas decimais:
show4Decimals :: Double -> String
show4Decimals x = showFFloat (Just 4) x ""

-- Função main. Ponto de partida do programa:
main = do
    entrada <- readFile "entrada.txt" -- Abrindo arquivo entrada.txt
    dist <- readFile "distancia.txt" -- Abrindo arquivo distancia.txt
    --   putStrLn $ show $ montaPontos $ map converteSF $ map words $ lines entrada
    --   putStrLn $ show $ montaGrupos (leiaDouble dist) $ montaPontos $ map converteSF $ map words $ lines entrada
    let gss = montaGrupos (leiaDouble dist) $ montaPontos $ map converteSF $ map words $ lines entrada
    writeFile "saida.txt" $ gsString gss
    --putStrLn $ show $ map show4Decimals $ map leiaDouble $ words entrada