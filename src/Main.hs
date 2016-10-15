
import qualified NS3473Column.CmdLine as CMD 
import qualified NS3473Column.System as S


main :: IO ()
main = putStrLn (show CMD.cmdLine) >> 
        case (CMD.x CMD.cmdLine) of 
            True -> S.calcAs CMD.cmdLine
            False -> S.runSystem CMD.cmdLine 
