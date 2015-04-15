{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module NS3473Column.System where

-- import Data.Maybe (fromJust)

import Control.Monad (mplus) 

import Control.Monad.Writer (Writer,runWriter,tell,writer)

import Text.Printf (printf)

import Data.Monoid ((<>))

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Columns as C
import qualified NS3473.Buckling as X

type WriterSB = Writer String Double

createRebarCollection :: Int     -- ^ Rebar diam
                         -> Int  -- ^ Rebar amount along h2
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
createColumn h1 h2 cln lkf conc rebar = 
    C.Column (fromIntegral h1) (fromIntegral h2) (fromIntegral cln) (read lkf :: Double) conc rebar

-- | Moment based on creep, excentricity, etc
calcMf :: C.Column 
          -> Double 
          -> Double 
          -> WriterSB
calcMf co nf mo = let e1 = X.e1 co nf mo 
                      ae = X.ae co nf
                      creep = X.creep co nf mo in
                    tell (printf "e1: %.2f mm\nae: %.2f mm\ncreep: %.2f mm\n" e1 ae creep) >> 
                    return (nf*(ae+e1+creep)/1000.0)

runSystem :: C.Column 
             -> Maybe Double  -- ^ Normal Force [kN]
             -> Maybe Double  -- ^ Moment [kNm]
             -> IO ()
runSystem co nf mo = let Just nf' = mplus nf (Just 0.0)
                         Just mo' = mplus mo (Just 0.0)
                         factn = X.nf co nf'
                         mf = runWriter (calcMf co nf' mo') --  >>= \mf ->
                         factm = X.factM co (fst mf) in
                                             --return mf :: WriterSB) in
                                             -- return (X.factM co mf) :: WriterSB) in
                                             -- return (X.factM co mf) :: WriterSB >>= \factm ->
                                             -- return (X.nf co nf') :: WriterSB >>= \factn -> 
                     putStrLn (snd mf) >>
                     putStrLn (printf "mf: %.6f" factm) >>
                     putStrLn (printf "nf: %.6f" factn) >>
                     return ()
