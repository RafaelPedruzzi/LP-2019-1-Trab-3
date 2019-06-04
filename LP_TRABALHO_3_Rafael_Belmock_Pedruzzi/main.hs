-- Linguagens de Programação - Prof. Flavio Varejão - 2019-1
-- Terceiro trabalho de implementação
--
-- Aluno: Rafael Belmock Pedruzzi
--
-- main.hs: módulo main

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
    -- Realizando o agrupamento:
    let gs = montaGrupos (leiaDouble dist) $ montaPontos $ map converteSF $ map words $ lines entrada
    writeFile "saida.txt" $ gsString gs -- Imprimindo os grupos em saida.txt
    writeFile "result.txt" $ show4Decimals $ sse gs -- Calculando e imprimindo o resultado da sse em result.txt