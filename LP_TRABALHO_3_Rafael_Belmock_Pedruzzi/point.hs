-- Linguagens de Programação - Prof. Flavio Varejão - 2019-1
-- Terceiro trabalho de implementação
--
-- Aluno: Rafael Belmock Pedruzzi
--
-- point.hs: módulo responsável pela implementação dos calculos e estruturas feitos com pontos muldidimensionais

module TrabPoint where

-- Estrutura que define o tipo ponto:
data Ponto = Ponto {
    i     :: Int,
    coord :: [Double]
} deriving Show

-- Função que cria a lista de pontos:
montaPontos xss = montaPontosAux xss 1

montaPontosAux [] i = []
montaPontosAux (xs:xss) i = Ponto{i = i,coord = xs} : montaPontosAux xss (i+1)

-- Função que calcula a distância entre dois pontos:
-- Parâmetros: dois pontos.
-- Retorno: distancia euclidiana entre x1 e x2.
-- Condições: os pontos devem ter o mesmo número de dimensões.
distPoints (Ponto _ xs1) (Ponto _ xs2) = sqrt $ distPointsAux xs1 xs2

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

-- Funcão que cria a String para impressão no arquivo saida.txt
-- parâmetros: a lista de grupos.
gsString :: [[Ponto]] -> String
gsString (g:[]) = gsStringAux g
gsString (g:gs) = gsStringAux g ++ "\n\n" ++ gsString gs

gsStringAux :: [Ponto] -> String
gsStringAux ((Ponto i _):[]) = show i
gsStringAux ((Ponto i _):ps) = (show i) ++ " " ++ gsStringAux ps
