module Darcs.UI.Commands.ShowPatchIndex ( showPatchIndexFiles, showPatchIndexAll, showPatchIndexStatus, patchIndexTest ) where
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Flags ( DarcsFlag, useCache )
import Prelude hiding ( (^) )
import Darcs.UI.Options ( DarcsOption, (^), oid, odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository ( withRepository, RepoJob(..) )
import Darcs.Repository.PatchIndex
import Control.Arrow ()

showPatchIndexBasicOpts :: DarcsOption a
                           (Bool -> Bool -> Bool -> Maybe String -> a)
showPatchIndexBasicOpts = O.files ^ O.directories ^ O.nullFlag ^ O.workingRepoDir

showPatchIndexOpts :: DarcsOption a
                      (Bool
                       -> Bool
                       -> Bool
                       -> Maybe String
                       -> Maybe O.StdCmdAction
                       -> Bool
                       -> Bool
                       -> O.Verbosity
                       -> Bool
                       -> O.UseCache
                       -> Maybe String
                       -> Bool
                       -> Maybe String
                       -> Bool
                       -> a)
showPatchIndexOpts = showPatchIndexBasicOpts `withStdOpts` oid

showPatchIndexAll :: DarcsCommand [DarcsFlag]
showPatchIndexAll = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-all",
  commandDescription = "Dump complete content of patch index.",
  commandHelp =
      "The `darcs show patch-index all' command lists all information in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexAllCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = odesc showPatchIndexBasicOpts,
  commandDefaults = defaultFlags showPatchIndexOpts,
  commandCheckOptions = ocheck showPatchIndexOpts,
  commandParseOptions = onormalise showPatchIndexOpts }

showPatchIndexFiles :: DarcsCommand [DarcsFlag]
showPatchIndexFiles = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-files",
  commandDescription = "Dump current files registered in patch index.",
  commandHelp =
      "The `darcs show patch-index files' command lists all current files registered in the patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexFilesCmd,
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = odesc showPatchIndexBasicOpts,
  commandDefaults = defaultFlags showPatchIndexOpts,
  commandCheckOptions = ocheck showPatchIndexOpts,
  commandParseOptions = onormalise showPatchIndexOpts }

showPatchIndexStatus :: DarcsCommand [DarcsFlag]
showPatchIndexStatus = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-status",
  commandDescription = " Report patch-index status",
  commandHelp =
      "The `darcs show patch-index-status' reports if the patch index is in sync, out of sync, or does not exist",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showPatchIndexStatus',
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = odesc showPatchIndexBasicOpts,
  commandDefaults = defaultFlags showPatchIndexOpts,
  commandCheckOptions = ocheck showPatchIndexOpts,
  commandParseOptions = onormalise showPatchIndexOpts }

patchIndexTest :: DarcsCommand [DarcsFlag]
patchIndexTest = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "patch-index-test",
  commandDescription = "Test patch-index",
  commandHelp =
      "The `darcs show patch-index-test' tests patch index",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = piTest',
  commandPrereq = amInHashedRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = odesc showPatchIndexBasicOpts,
  commandDefaults = defaultFlags showPatchIndexOpts,
  commandCheckOptions = ocheck showPatchIndexOpts,
  commandParseOptions = onormalise showPatchIndexOpts }

showPatchIndexAllCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPatchIndexAllCmd _ opts _ =
  withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _ _) ->
    dumpPatchIndex repodir

showPatchIndexFilesCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPatchIndexFilesCmd _ opts _ = withRepository (useCache opts) $ RepoJob $ \(Repo repodir _ _ _) ->
  dumpPatchIndexFiles repodir

showPatchIndexStatus' :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPatchIndexStatus' _ opts _ = withRepository (useCache opts) $ RepoJob $ \(repo@(Repo repodir _ _ _)) -> do
  ex <- doesPatchIndexExist repodir
  if ex
   then do
          sy <- isPatchIndexInSync repo
          if sy
            then putStrLn "Patch Index is in sync with repo."
            else putStrLn "Patch Index is outdated. Run darcs optimize enable-patch-index"
   else putStrLn "Patch Index is not yet created. Run darcs optimize enable-patch-index"

piTest' :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
piTest' _ opts _ = withRepository (useCache opts) $ RepoJob piTest
