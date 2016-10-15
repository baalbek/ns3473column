{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module NS3473Column.System where

-- import Data.Maybe (fromJust)

import Control.Monad.Writer (Writer,runWriter,tell)

import Text.Printf (printf)

import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Columns as C
import qualified NS3473.Buckling as X
import qualified NS3473Column.ColumnSystem as CS
import qualified NS3473Column.CmdLine as CMD

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

runSystem' :: C.Column 
             -> Double  -- ^ Normal Force [kN]
             -> Double  -- ^ Moment [kNm]
             -> IO ()
runSystem' co nf mo = let factn = X.nf co nf
                          mf = runWriter (calcMf co nf mo) 
                          factm = X.factM co (fst mf) in
                     putStrLn (snd mf) >>
                     putStrLn (printf "mf: %.6f" factm) >>
                     putStrLn (printf "nf: %.6f" factn) >>
                     return ()

runSystem :: CMD.CmdLine
             -> IO ()
runSystem cs = let col = (CS.column cs)
                   m = (CS.moment cs)
                   nf = (CS.normalForce cs) in
                   runSystem' col nf m 

calcAs :: CMD.CmdLine
          -> IO ()
calcAs cs = undefined

--printf "As en side: %.2f mm2\n" result
--    where wt' = (read (wt opts)) :: Double
--          result = (wt' * (X.ac column) * (X.fcd column) / fsd) 
