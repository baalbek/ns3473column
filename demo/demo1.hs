

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Columns as C
import qualified NS3473.Buckling as X
import qualified NS3473Column.System as S

rebar = S.createRebarCollection 20 2
conc = M.newConc "35"

co = S.createColumn 300 300 4500 "2.0" conc rebar

runx = S.runSystem co (Just 100) (Just 45)



