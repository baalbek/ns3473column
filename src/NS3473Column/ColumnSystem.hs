module NS3473Column.ColumnSystem where

import qualified NS3473.Rebars as R
import qualified NS3473.Columns as C
import qualified NS3473.Concrete as M

class ColumnSystem a where 
    normalForce         :: a -> Double 
    moment              :: a -> Double  
    h1                  :: a -> Int         -- ^ Shortest Column side [mm]
    h2                  :: a -> Int         -- ^ Longest Column side [mm]
    collength           :: a -> Int         -- ^ Column Length [mm]
    lkf                 :: a -> Double      -- ^Effective length faktor [mm]
    numRebars           :: a -> Int         
    rebarDiam           :: a -> Int         
    column              :: a -> C.Column    
    column self = C.Column (fromIntegral (h1 self)) (fromIntegral (h2 self)) (fromIntegral (collength self)) (lkf self) conc (rebars self)
        where conc = (M.newConc "35") 
    rebars              :: a -> R.RebarCollection 
    rebars self = R.ColumnRebars rebar (fromIntegral (numRebars self)) 25
        where rebar = R.Rebar (fromIntegral (rebarDiam self))
