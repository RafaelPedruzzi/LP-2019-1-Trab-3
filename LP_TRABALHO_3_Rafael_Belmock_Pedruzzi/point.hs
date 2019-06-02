-- Linguagens de Programação - Prof. Flavio Varejão - 2019-1
-- Terceiro trabalho de implementação
--
-- Aluno: Rafael Belmock Pedruzzi
--
-- point.hs: módulo responsável pela implementação dos calculos e estruturas feitos com pontos muldidimensionais

module TrabPoint where

-- Função que calcula a distância entre dois pontos:
-- Parâmetros: duas listas de Floats representando as cordenadas dos pontos.
-- Retorno: distancia euclidiana entre x1 e x2.
-- Condições: os pontos devem ter o mesmo número de dimensões.
-- ATENÇÃO: a primeira posição de cada ponto é ignorada.
distPoints (x1:xs1) (x2:xs2) = sqrt $ distPointsAux xs1 xs2

-- Função recursiva auxiliar de distPoints que realiza a soma do quadrado das diferenças de todas as coordenadas de x1 e x2:
-- Parâmetros: listas contendo as coordenadas restantes dos pontos.
distPointsAux [] [] = 0
distPointsAux (x1:xs1) (x2:xs2) = (x1 - x2) * (x1 - x2) + distPointsAux xs1 xs2


-- Função que monta os grupos segundo o algoritimo de agrupamento por líder:
-- Parâmetros: a distância máxima entre um ponto e seu líder e a lista de pontos.
-- Retorno: lista com os grupos formados (líder é o ponto na primeira posição da lista do grupo).
-- Condição: todos os pontos devem ter o mesmo número de dimensões.
-- ATENÇÃO: a primeira posição de cada ponto é ignorada.
montaGrupos :: Float -> [[Float]] -> [[[Float]]]

montaGrupos dist [] = []
montaGrupos dist (xs:xss) = grupo : montaGrupos dist [as | as <- xss, distPoints xs as > dist]
                    where
                        grupo = xs : [as | as <- xss, (distPoints xs as) <= dist]
