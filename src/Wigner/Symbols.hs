module Wigner.Symbols(
    a, b,
    ix_i, ix_j, ix_1, ix_2,
    alpha, beta, delta, rho, theta,
    map_element, map_element') where

    import Data.Map as M
    import Wigner.Expression

    a = Symbol "a"
    b = Symbol "b"
    i = Symbol "i"
    j = Symbol "j"

    ix_i = IndexSymbol i
    ix_j = IndexSymbol j
    ix_1 = IndexInt 1
    ix_2 = IndexInt 2

    alpha = Symbol "\\alpha"
    beta = Symbol "\\beta"
    delta = Symbol "\\delta"
    rho = Symbol "\\rho"
    theta = Symbol "\\theta"

    default_map = fromList [(a, alpha), (b, beta)]

    extract :: String -> Maybe a -> a
    extract s (Just x) = x
    extract s Nothing = error s

    map_element' :: M.Map Symbol Symbol -> Element -> Element
    map_element' sym_map (Element s i v) = Element (extract error_msg (M.lookup s sym_map)) i v where
        error_msg = "Symbol was not found in correspondence map"

    map_element = map_element' default_map
