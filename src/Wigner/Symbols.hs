module Wigner.Symbols(
    a, b, x, x',
    ix_i, ix_j, ix_k, ix_1, ix_2,
    alpha, beta, delta, rho, theta,
    symbol, index,
    mapElement, mapElementWith, mapElementPairWith,
    default_map,
    SymbolCorrespondence, SymbolCorrespondence2
    ) where

import Data.Map as M
import Wigner.Expression
import Data.Maybe

symbol = Symbol


class CanBeIndex a where
    index :: a -> Index

instance CanBeIndex Int where index = IndexInt
instance CanBeIndex Symbol where index = IndexSymbol


a = symbol "a"
b = symbol "b"
i = symbol "i"
j = symbol "j"
k = symbol "k"
x = symbol "x"
x' = symbol "x^\\prime"

alpha = symbol "\\alpha"
beta = symbol "\\beta"
delta = symbol "\\delta"
rho = symbol "\\rho"
theta = symbol "\\theta"

ix_i = index i
ix_j = index j
ix_k = index k
ix_1 = index (1 :: Int)
ix_2 = index (2 :: Int)


type SymbolCorrespondence = M.Map Symbol Symbol
type SymbolCorrespondence2 = M.Map Symbol (Symbol, Symbol) -- for positive-P

default_map = fromList [(a, alpha), (b, beta)]

mapElementWith :: SymbolCorrespondence -> Element -> Element
mapElementWith sym_map (Element s i v) = Element (fromJust (M.lookup s sym_map)) i v where
    error_msg = "Symbol was not found in correspondence map"

mapElementPairWith :: SymbolCorrespondence2 -> Element -> (Element, Element)
mapElementPairWith sym_map (Element s i v) = (e1, e2) where
    (s1, s2) = case M.lookup s sym_map of
        Nothing -> error (show(s) ++ " was not found in the symbol map")
        Just x -> x
    e1 = Element s1 i v
    e2 = Element s2 i v

mapElement = mapElementWith default_map
