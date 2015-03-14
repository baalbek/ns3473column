{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module NS3473Column.System where

import Data.Maybe (fromJust)

import Control.Monad.Writer (Writer,runWriter,tell,writer)

import Text.Printf (printf)

import Data.Monoid ((<>))

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Columns as C
import qualified NS3473.Buckling as X

createRebarCollection :: Int     -- ^ Rebar diam
                         -> Int  -- ^ Rebar amount on one side
                         -> R.RebarCollection 
createRebarCollection rdiam rmnt = R.ColumnRebars rebar (fromIntegral rmnt) 25
    where rebar = R.Rebar (fromIntegral rdiam)

createColumn :: Int      -- ^ Shortest column side [mm]
                -> Int   -- ^ Longest column side [mm]
                -> Int   -- ^ Column lenght 
                -> String -- ^ Effective length factor 
                -> M.Concrete 
                -> R.RebarCollection   
                -> C.Column
createColumn h1 h2 cln lkf conc reb = C.Column h1' h2' cln' lkf' conc reb
    where h1' = fromIntegral h1
          h2' = fromIntegral h1
          cln' = fromIntegral cln
          lkf' = read lkf :: Double



check :: C.Column -> Writer String Bool
check = undefined 

runSystem :: C.Column 
             -> Maybe Double -- ^ Moment [kNm]
             -> Maybe Double -- ^ Normal force [kN]
             -> IO ()
runSystem column m nf = 
    printf "System %s - %s - %s\n" (show column) (show m) (show nf) >>
    return ()

