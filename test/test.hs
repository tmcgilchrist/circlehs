import           Control.Monad ( unless )
import           Control.Monad.IO.Class (liftIO, MonadIO)

import           System.Directory ( getDirectoryContents )
import           System.Process ( callProcess )
import           System.Exit ( exitFailure )
import           System.IO ( hSetBuffering, stdout, stderr, BufferMode (..) )

testMain :: [IO Bool] -> IO ()
testMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

testCliMain :: [String] -> IO ()
testCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering

main :: IO ()
main =
  testMain [
  ]
