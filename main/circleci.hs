{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_circlehs
import           DependencyInfo_circlehs

import           Data.String (String)

import           Options.Applicative
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (IO, BufferMode (..), stdout, stderr
                           , hSetBuffering, putStrLn)

data RunType =
    DryRun
  | RealRun
  deriving (Eq, Show)

data SafeCommand a =
    VersionCommand
  | DependencyCommand
  | RunCommand RunType a
  deriving (Eq, Show)

-- | A 'command' combinator that adds helper and description in a
--   slightly cleaner way
command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser' =
  command label (info (parser' <**> helper) (progDesc description))

-- | Dispatch multi-mode programs with appropriate helper to make the
--   default behaviour a bit better.
dispatch :: Parser a -> IO a
dispatch p = getArgs >>= \x -> case x of
  [] -> let -- We don't need to see the Missing error if we're getting the whole usage string.
            removeError' (h, e, c) = (h { helpError = mempty }, e, c)
            removeError (Failure (ParserFailure failure)) = Failure (ParserFailure ( removeError' <$> failure ))
            removeError a = a
        in  execParserPure (prefs showHelpOnError) (info (p <**> helper) idm) <$> getArgs
            >>= handleParseResult . removeError
  _  -> execParser (info (p <**> helper) idm)

-- | Simple interface over 'dispatch' and 'safeCommand'
--
-- @ name -> version -> dependencyInfo -> parser -> action @
--
--   Example usage:
--
-- > cli "my-cli" buildInfoVersion dependencyInfo myThingParser $ \c ->
-- >  case c of
-- >      DoThingA -> ...
-- >      DoThingB -> ...
cli :: Show a => [Char] -> [Char] -> [[Char]] -> Parser a -> (a -> IO b) -> IO b
cli name v deps commandParser act = do
  dispatch (safeCommand commandParser) >>= \a ->
    case a of
      VersionCommand ->
        putStrLn (name <> ": " <> v) >> exitSuccess
      DependencyCommand ->
        mapM putStrLn deps >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        act c

-- | Turn a Parser for a command of type a into a safe command
--   with a dry-run mode and a version flag
safeCommand :: Parser a -> Parser (SafeCommand a)
safeCommand commandParser =
      VersionCommand <$ versionFlag
  <|> DependencyCommand <$ dependencyFlag
  <|> RunCommand <$> dryRunFlag <*> commandParser

versionFlag :: Parser ()
versionFlag =
  flag' () $
       short 'v'
    <> long "version"
    <> help "Version information"

dependencyFlag :: Parser ()
dependencyFlag =
  flag' () $
       long "dependencies"
    <> hidden

dryRunFlag :: Parser RunType
dryRunFlag =
  flag RealRun DryRun $
       long "dry-run"
    <> hidden

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "circleci" buildInfoVersion dependencyInfo parser $ \cmd ->
    case cmd of
      List ->
        putStrLn "*implement me*" >> exitFailure

parser :: Parser Command
parser =
  subparser . mconcat $ commands

-- |
-- Available Cli commands
data Command
  = List -- List CicleCI
  deriving (Eq, Show)

commands :: [Mod CommandFields Command]
commands =
  [ command' "list" "List CircleCI projects"
    (pure List)
  ]
