{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Map as Map
import Text.Printf (printf)


import Control.Monad (mplus)

import System.Console.CmdLib 

import qualified NS3473Column.System as S
import qualified NS3473.Concrete as M

data Main = Main { 
        nf :: String,
        m :: String,
        h1 :: Int,
        h2 :: Int,
        cln :: Int,
        lkf :: String,
        d :: Int,
        n :: Int
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            nf     %> [ Group "Forces", Help "Normal force (kN)", ArgHelp "VAL", Default "0.0" ], 
            m      %> [ Group "Forces", Help "Moment (kNm)", ArgHelp "VAL", Default "0.0" ], 
            h1     %> [ Group "Geometry", Help "Shortest column side (mm)", ArgHelp "VAL", Required False], 
            h2     %> [ Group "Geometry", Help "Longest column side (mm)", ArgHelp "VAL", Required False],
            cln    %> [ Group "Geometry", Help "Column lenght (mm)", ArgHelp "VAL", Required False],
            lkf    %> [ Group "Geometry", Help "Effective length faktor (mm) (default: 1.0)", ArgHelp "VAL", Default "1.0" ],
            d      %> [ Group "Rebars", Help "Rebar diameter (mm) (default: 12)", ArgHelp "VAL", Required False, Default (12 :: Int) ],
            n      %> [ Group "Rebars", Help "Amount of rebars one side of column (default: 2)", ArgHelp "VAL", Required False, Default (2 :: Int) ]
        ]

instance RecordCommand Main where
    mode_summary _ = "NS 3473 Columns"

showOpt :: Main -> String
showOpt main = "yep"

nothingIfZero :: String -> Maybe Double
nothingIfZero s | s == "0.0" = Nothing
                | otherwise = Just (read s :: Double)

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> do
    let rebar = S.createRebarCollection (d opts) (n opts) 
    let column = S.createColumn (h1 opts) (h2 opts) (cln opts) (lkf opts) (M.newConc "35") rebar 
    let moment = nothingIfZero (m opts)
    let normForce = nothingIfZero (nf opts)
    S.runSystem column moment normForce 
    return ()

