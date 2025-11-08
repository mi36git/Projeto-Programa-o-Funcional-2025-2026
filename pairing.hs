{- 
Tarefa 2 – Emparelhamento com AVE e Eliminação Direta
Módulo: Pairing
Funções:
    runAVEParing :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
    runElimParing :: TorneioElim -> ResultadosElim
-}

module Pairing
  ( runAVEParing, runElimParing
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Fileread (Jogador, TorneioAVE, ResultadosAVE, TorneioElim, ResultadosElim)

-- 1. Emparelhamento pelo método AVE

-- Extrai o AVE de um Jogador*-
ave :: Jogador -> Double
ave (_, _, _, a) = a

-- Extrai o nome de um Jogador
nomeJog :: Jogador -> String
nomeJog (n, _, _, _) = n

-- Emparelha elementos 2 a 2; se ímpar, o último fica de fora
emparelhar :: [a] -> [(a, a)]
emparelhar (x:y:xs) = (x, y) : emparelhar xs
emparelhar _        = []

-- | runAVEParing
-- Recebe:
--   * TorneioAVE -> (Nome, Nº de rondas, [Jogador])
--   * ResultadosAVE -> Lista de resultados das rondas anteriores
-- Devolve:
--   * Nova lista de ResultadosAVE, adicionando uma nova ronda
runAVEParing :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
runAVEParing (_, _, jogadores) resultadosAnteriores =
  let ordenados = sortBy (flip $ comparing ave) jogadores
      pares     = emparelhar ordenados
      novaRonda = [ (nomeJog j1, nomeJog j2, 0, 0) | (j1, j2) <- pares ]
  in resultadosAnteriores ++ novaRonda


-- 2. Emparelhamento pelo método de Eliminação Direta

-- | runElimParing
-- Recebe:
--   * TorneioElim -> (Nome, [Equipas])
-- Devolve:
--   * ResultadosElim -> [(EquipaA, EquipaB, "")]
--     (o terceiro elemento representa o vencedor, inicialmente vazio)
runElimParing :: TorneioElim -> ResultadosElim
runElimParing (_, equipas) =
  [ (a, b, "") | (a, b) <- emparelhar equipas ]
```