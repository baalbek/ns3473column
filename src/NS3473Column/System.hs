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
