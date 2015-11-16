-- | This module is used by the push and put commands to apply a bundle to a
-- remote repository. By remote I do not necessarily mean a repository on another
-- machine, it is just not the repository we're located in.
module Darcs.UI.RemoteApply ( remoteApply ) where

import System.Exit ( ExitCode )

import Darcs.UI.Flags ( DarcsFlag, remoteDarcs, applyAs )
import Darcs.Util.Text ( breakCommand )
import Darcs.Util.URL ( isHttpUrl, isSshUrl, splitSshUrl, SshFilePath(..) )
import Darcs.UI.External
    ( darcsProgram
    , pipeDoc
    , pipeDocSSH
    , maybeURLCmd
    )
import Darcs.UI.Options ( parseFlags )
import Darcs.UI.Options.All ( debug, compress )
import qualified Darcs.Repository.Ssh as Ssh ( remoteDarcs )
import Darcs.Util.Printer ( Doc, RenderMode(..) )

remoteApply :: [DarcsFlag] -> String -> Doc -> IO ExitCode
remoteApply opts repodir bundle
    = case applyAs opts of
        Nothing
            | isSshUrl repodir -> applyViaSsh opts (splitSshUrl repodir) bundle
            | isHttpUrl repodir -> applyViaUrl opts repodir bundle
            | otherwise -> applyViaLocal opts repodir bundle
        Just un -> if isSshUrl repodir
                   then applyViaSshAndSudo opts (splitSshUrl repodir) un bundle
                   else applyViaSudo un repodir bundle

applyViaSudo :: String -> String -> Doc -> IO ExitCode
applyViaSudo user repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc Standard "sudo" ["-u",user,darcs,"apply","--all","--repodir",repo] bundle

applyViaLocal :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaLocal opts repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc Standard darcs ("apply":"--all":"--repodir":repo:applyopts opts) bundle

applyViaUrl :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaUrl opts repo bundle =
    do maybeapply <- maybeURLCmd "APPLY" repo
       case maybeapply of
         Nothing -> applyViaLocal opts repo bundle
         Just apply ->
           do let (cmd, args) = breakCommand apply
              pipeDoc Standard cmd (args ++ [repo]) bundle

applyViaSsh :: [DarcsFlag] -> SshFilePath -> Doc -> IO ExitCode
applyViaSsh opts repo =
    pipeDocSSH (parseFlags compress opts) Standard repo
           [Ssh.remoteDarcs (remoteDarcs opts) ++" apply --all "++unwords (applyopts opts)++
                     " --repodir '"++sshRepo repo++"'"]

applyViaSshAndSudo :: [DarcsFlag] -> SshFilePath -> String -> Doc -> IO ExitCode
applyViaSshAndSudo opts repo username =
    pipeDocSSH (parseFlags compress opts) Standard repo
           ["sudo -u "++username++" "++Ssh.remoteDarcs (remoteDarcs opts)++
                     " apply --all --repodir '"++sshRepo repo++"'"]

applyopts :: [DarcsFlag] -> [String]
applyopts opts = if parseFlags debug opts then ["--debug"] else []
