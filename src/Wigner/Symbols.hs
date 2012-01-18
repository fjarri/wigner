module Wigner.Symbols(
    a, b,
    ix_i, ix_j, ix_1, ix_2,
    var_x, var_x',
    alpha, beta, delta, rho, theta,
    symbol, index, variable,
    mapElement, mapElementWith) where

    import Data.Map as M
    import Wigner.Expression
    import Data.Maybe

    symbol = Symbol


    class CanBeIndex a where
        index :: a -> Index

    instance CanBeIndex Integer where index = IndexInteger
    instance CanBeIndex Symbol where index = IndexSymbol


    class CanBeVariable a where
        variable :: a -> Variable

    instance CanBeVariable Symbol where variable = VariableSymbol


    a = symbol "a"
    b = symbol "b"
    i = symbol "i"
    j = symbol "j"
    x = symbol "x"
    x' = symbol "x'"

    alpha = symbol "\\alpha"
    beta = symbol "\\beta"
    delta = symbol "\\delta"
    rho = symbol "\\rho"
    theta = symbol "\\theta"

    ix_i = index i
    ix_j = index j
    ix_1 = index (1 :: Integer)
    ix_2 = index (2 :: Integer)

    var_x = variable x
    var_x' = variable x'

    default_map = fromList [(a, alpha), (b, beta)]

    mapElementWith :: M.Map Symbol Symbol -> Element -> Element
    mapElementWith sym_map (Element s i v) = Element (fromJust (M.lookup s sym_map)) i v where
        error_msg = "Symbol was not found in correspondence map"

    mapElement = mapElementWith default_map
