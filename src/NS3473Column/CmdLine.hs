{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
module NS3473Column.CmdLine where

import System.Console.CmdArgs (cmdArgs,Data,Typeable,typ,def,help,groupname,(&=))

import qualified NS3473Column.ColumnSystem as CS

data CmdLine = CmdLine { 
        nf :: String,
        m :: String,
        h1 :: Int,
        h2 :: Int,
        cln :: Int,
        lkf :: String,
        d :: Int,
        n :: Int,
        wt :: String,
        x :: Bool
    }
    deriving (Typeable, Data, Eq, Show)

asDouble :: String -> Double
asDouble s | s == "0.0" = 0.0
           | otherwise = (read s :: Double)

instance CS.ColumnSystem CmdLine where
    normalForce self = asDouble (nf self)
    moment self = asDouble (m self)
    h1 self = (h1 self) 
    h2 self = (h2 self) 
    collength self = (cln self) 
    lkf self = asDouble (lkf self)                 
    numRebars self = (n self)           
    rebarDiam self = (d self)         

cmdLine = 
    CmdLine {
        nf = "0.0" &= groupname "Forces" &= help "Normal force (kN). Default: 0.0"
        ,m = "0.0" &= groupname "Forces" &= help "Moment (kNm). Default: 0.0"
        ,h1 = 100 &= groupname "Geometry" &= help "Shortest column side (mm). Default: 0.0"
        ,h2 = 100 &= groupname "Geometry" &= help "Longest column side (mm). Default: 0.0"
        ,cln = 2400 &= groupname "Geometry" &= help "Column length (mm). column side (mm). Default: 0.0"
        ,lkf = "1.0" &= groupname "Geometry" &= help "Effective length faktor (mm). Default: 1.0"
        ,n = 2 &= groupname "Rebars" &= help "Amount of rebars one side of column. Default: 2" 
        ,d = 12 &= groupname "Rebars" &= help "Rebar diameter (mm). Default: 12" 
        ,wt = "0" &= groupname "Rebars" &= help "Calculated rebar amount percentage from graph based on nf and m" 
        ,x = False &= groupname "Rebars" &= help "If set, will calculate rebar amount from wt. Default: false" 
    }
