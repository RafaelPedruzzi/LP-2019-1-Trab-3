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
    i     :: Int,     -- Identificador do ponto.
    coord :: [Double] -- Lista das coordenadas do ponto.
} deriving Show

-- Função que cria a lista de pontos:
-- Parâmetros: uma lista contendo as coordenadas de cada ponto como listas de Double.
-- Retorno: uma lista de pontos com ids numerados a partir de 1.
montaPontos :: [[Double]] -> [Ponto]
montaPontos xss = montaPontosAux xss 1 -- Inicializando o primeiro id como 1 e montando os pontos.

-- Função recursiva auxiliar de montaPontos:
-- Parâmetros: uma lista contendo as coordenadas de cada ponto como listas de Double e o id do primeiro ponto.
-- Retorno: uma lista de pontos com ids numerados a partir de 1.
montaPontosAux :: [[Double]] -> Int -> [Ponto]
montaPontosAux [] i = []
montaPontosAux (xs:xss) i = Ponto{i = i,coord = xs} : montaPontosAux xss (i+1)

-- Função que calcula a distância entre dois pontos:
-- Parâmetros: dois pontos.
-- Retorno: distancia euclidiana entre os pontos.
-- Condição: os pontos devem ter o mesmo número de dimensões.
distPoints :: Ponto -> Ponto -> Double
distPoints (Ponto _ xs1) (Ponto _ xs2) = sqrt $ distPointsAux xs1 xs2

-- Função recursiva auxiliar de distPontos:
-- Parâmetros: listas com as coordenadas dos pontos.
-- Retorno: raiz quadrada da distância euclidiana entre as coordenadas.
-- Condição: as listas devem ter o mesmo tamanho.
distPointsAux :: [Double] -> [Double] -> Double
distPointsAux [] [] = 0
distPointsAux (x1:xs1) (x2:xs2) = (x1 - x2) * (x1 - x2) + distPointsAux xs1 xs2

-- Função que monta os grupos segundo o algoritimo de agrupamento por líder:
-- Parâmetros: a distância máxima entre um ponto e seu líder e a lista de pontos.
-- Retorno: lista com os grupos formados sendo cada grupo uma lista de pontos (líder é o ponto na primeira posição da lista do grupo).
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
montaGrupos :: Double -> [Ponto] -> [[Ponto]]
montaGrupos dist [] = []
montaGrupos dist (xs:xss) = grupo : montaGrupos dist resto
    where
        grupo = xs : [as | as <- xss, (distPoints xs as) <= dist] -- Pontos que satisfazem a condição de distância com relação ao primeiro ponto da lista (líder).
        resto = [as | as <- xss, distPoints xs as > dist] -- Pontos que não satisfazem a condição e formarão outros grupos.

-- Funcão que cria a String para impressão no arquivo saida.txt
-- Parâmetros: a lista de grupos.
-- Retorno: String no formato especificado para impressão dos grupos.
-- Condição: deve haver pelo menos um grupo na lista.
gsString :: [[Ponto]] -> String
gsString (g:[]) = gsStringAux g
gsString (g:gs) = gsStringAux g ++ "\n\n" ++ gsString gs

-- Função recursiva auxiliar de gsString que converte cada grupo em String:
-- Parâmetros: lista de pontos representando um grupo.
-- Retorno: String no formato especificado para impressão de um grupo.
-- Condição: deve haver pelo menos um grupo na lista.
gsStringAux :: [Ponto] -> String
gsStringAux ((Ponto i _):[]) = show i
gsStringAux ((Ponto i _):ps) = (show i) ++ " " ++ gsStringAux ps

-- Função que calcula o centro de massa de um grupo:
-- Parâmetros: lista de pontos representando um grupo.
-- Retorno: ponto que representa o centro de massa do grupo.
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
centroMassa :: [Ponto] -> Ponto
centroMassa g = Ponto{i = 0,coord = map (/ (fromIntegral (length g))) (centroMassaAux g)}

-- Função recursiva auxiliar de centroMassa:
-- Parâmetros: lista de pontos representando um grupo.
-- Retorno: lista de Double representando a soma das coordenadas do grupo.
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
centroMassaAux :: [Ponto] -> [Double]
centroMassaAux ((Ponto i xs):[]) = xs
centroMassaAux ((Ponto i xs):ps) = zipWith (+) xs $ centroMassaAux ps

-- Função que calcula a sse do agrupamento:
-- Parâmetros: a lista de grupos.
-- Retorno: sse do agrupamento.
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
sse :: [[Ponto]] -> Double
sse [] = 0
sse (g:gs) = (sseAux g cm) + (sse gs)
    where
        cm = centroMassa g

-- Função recursiva auxiliar de sse:
-- Parâmetros: lista de pontos representando um grupo e o centro de massa deste grupo.
-- Retorno: um Double representando a soma dos quadrados das distâncias de cada ponto para o líder do grupo.
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
sseAux :: [Ponto] -> Ponto -> Double
sseAux [] cm = 0
sseAux (p:ps) cm = (d * d) + sseAux ps cm
    where
        d = distPoints p cm
