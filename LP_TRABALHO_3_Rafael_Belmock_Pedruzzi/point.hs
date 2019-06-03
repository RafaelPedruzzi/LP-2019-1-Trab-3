-- Linguagens de Programação - Prof. Flavio Varejão - 2019-1
-- Terceiro trabalho de implementação
--
-- Aluno: Rafael Belmock Pedruzzi
--
-- point.hs: módulo responsável pela implementação dos calculos e estruturas feitos com pontos muldidimensionais

module TrabPoint where

-- import Data.List

-- Estrutura que define o tipo ponto:
data Ponto = Ponto {
    i     :: Int,
    coord :: [Double]
} deriving Show

-- Função que cria a lista de pontos:
montaPontos :: [[Double]] -> [Ponto]
montaPontos xss = montaPontosAux xss 1

montaPontosAux :: [[Double]] -> Int -> [Ponto]
montaPontosAux (xs:[]) i = []
montaPontosAux (xs:xss) i = Ponto{i = i,coord = xs} : montaPontosAux xss (i+1)

-- Função que calcula a distância entre dois pontos:
-- Parâmetros: dois pontos.
-- Retorno: distancia euclidiana entre x1 e x2.
-- Condição: os pontos devem ter o mesmo número de dimensões.
distPoints :: Ponto -> Ponto -> Double
distPoints (Ponto _ xs1) (Ponto _ xs2) = sqrt $ distPointsAux xs1 xs2

distPointsAux :: [Double] -> [Double] -> Double
distPointsAux [] [] = 0
distPointsAux (x1:xs1) (x2:xs2) = (x1 - x2) * (x1 - x2) + distPointsAux xs1 xs2

-- Função que monta os grupos segundo o algoritimo de agrupamento por líder:
-- Parâmetros: a distância máxima entre um ponto e seu líder e a lista de pontos.
-- Retorno: lista com os grupos formados (líder é o ponto na primeira posição da lista do grupo).
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
montaGrupos :: Double -> [Ponto] -> [[Ponto]]
montaGrupos dist [] = []
montaGrupos dist (xs:xss) = grupo : montaGrupos dist [as | as <- xss, distPoints xs as > dist]
                            where
                                grupo = xs : [as | as <- xss, (distPoints xs as) <= dist]
-- montaGrupos dist ps = grupo : montaGrupos dist resto
--                             where
--                                 (grupo:resto) = partition ((distPoints xs as) <= dist) ps

-- Funcão que cria a String para impressão no arquivo saida.txt
-- parâmetros: a lista de grupos.
gsString :: [[Ponto]] -> String
gsString (g:[]) = gsStringAux g
gsString (g:gs) = gsStringAux g ++ "\n\n" ++ gsString gs

gsStringAux :: [Ponto] -> String
gsStringAux ((Ponto i _):[]) = show i
gsStringAux ((Ponto i _):ps) = (show i) ++ " " ++ gsStringAux ps

-- Função que calcula o centro de massa de um grupo:
-- Parâmetros: um grupo de pontos.
centroMassa :: [Ponto] -> Ponto
centroMassa g = Ponto{i = 0,coord = map (/ (fromIntegral (length g))) (centroMassaAux g)}

centroMassaAux :: [Ponto] -> [Double]
centroMassaAux ((Ponto i xs):[]) = xs
centroMassaAux ((Ponto i xs):ps) = zipWith (+) xs $ centroMassaAux ps

-- Função que calcula a sse do agrupamento:
-- Parâmetros: a lista de grupos.
sse :: [[Ponto]] -> Double
sse [] = 0
sse (g:gs) = (sseAux g) + (sse gs)

sseAux :: [Ponto] -> Double
sseAux [] = 0
sseAux (p:ps) = (d^2) + sseAux ps
           where
            d = distPoints p $ centroMassa $ p:ps
