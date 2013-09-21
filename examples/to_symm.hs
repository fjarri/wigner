import qualified Wigner.Transformations as T
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.OperatorAlgebra as O
import Wigner.Texable
import Wigner.Expression

import qualified Data.Map as M
import qualified Data.List as L

index :: Integer -> Index
index i = S.index (fromInteger i :: Int)

s_psi = S.symbol "\\Psi"
corr = M.fromList [(s_psi, s_psi)]
psi1 = D.operatorFuncIx s_psi [index 1] [Func (Element S.x [] [])]
psi1' = D.operatorFuncIx s_psi [index 1] [Func (Element S.x' [] [])]
psi2 = D.operatorFuncIx s_psi [index 2] [Func (Element S.x [] [])]
psi2' = D.operatorFuncIx s_psi [index 2] [Func (Element S.x' [] [])]

x = S.x
x' = S.x'
psi i v = D.operatorFuncIx s_psi [index i] [Func (Element v [] [])]

sx v = (dagger (psi 2 v) * (psi 1 v) + dagger (psi 1 v) * (psi 2 v))
sy v = (dagger (psi 2 v) * (psi 1 v) - dagger (psi 1 v) * (psi 2 v))
sz v = (dagger (psi 1 v) * (psi 1 v) - dagger (psi 2 v) * (psi 2 v))

main = do
    putStrLn $ showTex $ O.toSymmetricProduct O.bosonicCommutationRelation (
        --sx x * sx x'
        --sy x * sy x'
        --sz x * sz x'
        sx x * sz x' + sz x * sx x'
        )
