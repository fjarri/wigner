import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Expectations as E
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Wigner.OperatorAlgebra as OA

import Data.Ratio
import Wigner.Complex
import Wigner.Texable
import Wigner.Expression

import Wigner.XmdsInteraction

s_psi = S.symbol "\\Psi"
psi = D.operatorFuncIx s_psi [] [Func (Element S.x [] [])]

dpsi = dagger psi

psi6 = dpsi ^ 3 * psi ^ 3
psi4 = dpsi ^ 2 * psi ^ 2
psi2 = dpsi * psi

main = do
    putStrLn $ showTex $ OA.toSymmetricProduct OA.bosonicCommutationRelation psi2
