
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple
    ( defaultMainWithHooks
    , simpleUserHooks
    , UserHooks(..)
    , Args
    )
import Distribution.Simple.InstallDirs (InstallDirs(..), fromPathTemplate)
import Distribution.Simple.LocalBuildInfo (installDirTemplates, LocalBuildInfo)
import Distribution.Simple.Setup (CopyFlags)
import System.Directory (copyFile)

main = defaultMainWithHooks simpleUserHooks
    { postCopy = myPostCopy
    }

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostCopy args flags description buildInfo = do
    putStrLn "Moving non Haskell plugins"

    copyFile "./plugins/TicTacToe/main.py" $ binaryDir ++ "/tictactoe"
    copyFile "./plugins/Mark/main.py" $ binaryDir ++ "/mark"
  where
    binaryDir = (fromPathTemplate . bindir . installDirTemplates) buildInfo
