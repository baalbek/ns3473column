
import System.Console.CmdArgs (cmdArgs)
import qualified NS3473Column.CmdLine as CMD 
import qualified NS3473Column.System as S


main :: IO ()
main = cmdArgs CMD.cmdLine >>= \opts -> 
        putStrLn (show opts) >> 
        case (CMD.x opts) of 
            True -> S.calcAs opts --CMD.cmdLine
            False -> S.runSystem opts --CMD.cmdLine 
