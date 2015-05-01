{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

import Data.Map as Map
import Text.Printf (printf)


import Control.Monad (mplus)

import System.Console.CmdLib 

import qualified NS3473Column.System as S
import qualified NS3473.Concrete as M
import qualified NS3473.Buckling as X
import qualified NS3473.Columns as C
import NS3473.Common  (fsd)

data Main = Main { 
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
            n      %> [ Group "Rebars", Help "Amount of rebars one side of column (default: 2)", ArgHelp "VAL", Required False, Default (2 :: Int) ],
            wt     %> [ Group "Rebars", Help "Calculated rebar amount percentage from graph based on nf and m", ArgHelp "VAL", Required False ],
            x      %> [ Group "Choice", Help "If set, will calculate rebar amount from wt", ArgHelp "VAL", Required False, Default False ]
        ]

instance RecordCommand Main where
    mode_summary _ = "NS 3473 Columns"

asDouble :: String -> Double
asDouble s | s == "0.0" = 0.0
           | otherwise = (read s :: Double)

calcAs :: C.Column 
          -> Main 
          -> IO ()
calcAs column opts = printf "As en side: %.2f mm2\n" result
    where wt' = (read (wt opts)) :: Double
          result = (wt' * (X.ac column) * (X.fcd column) / fsd) 

runColumnSysem :: C.Column
                  -> Main 
                  -> IO ()
runColumnSysem column opts =
    S.runSystem column (asDouble (m opts)) (asDouble (nf opts)) 

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> 
        let rebar = S.createRebarCollection (d opts) (n opts) 
            column = S.createColumn (h1 opts) (h2 opts) (cln opts) (lkf opts) (M.newConc "35") rebar in
        case (x opts) of 
            True -> calcAs column opts 
            False -> runColumnSysem column opts 
