{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.UI.External
    ( sendEmail
    , generateEmail
    , sendEmailDoc
    , resendEmail
    , signString
    , verifyPS
    , execDocPipe
    , execPipeIgnoreError
    , pipeDoc
    , pipeDocSSH
    , maybeURLCmd
    , viewDoc
    , viewDocWith
    , haveSendmail
    , sendmailPath
    , diffProgram
    , darcsProgram
    , editText
    , editFile
    , catchall
    --  * Locales
    , setDarcsEncodings
    , getSystemEncoding
    , isUTF8Locale
    ) where

import Prelude hiding ( catch )

import Data.Maybe ( isJust, isNothing, maybeToList )
import Control.Monad ( unless, when, filterM, liftM2, void )
import GHC.MVar ( MVar )
import System.Exit ( ExitCode(..) )
import System.Environment
    ( getEnv
#if __GLASGOW_HASKELL__ >= 706
    , getExecutablePath
#else
    , getProgName
#endif
    )
import System.IO ( hPutStr, hPutStrLn, hClose,
                   hIsTerminalDevice, stdout, stderr, Handle )
import System.Directory ( doesFileExist,
                          findExecutable )
import System.FilePath.Posix ( (</>) )
import System.Process ( createProcess, proc, CreateProcess(..), runInteractiveProcess, waitForProcess, StdStream(..) )
import System.Process.Internals ( ProcessHandle )

import GHC.IO.Encoding ( setFileSystemEncoding, setForeignEncoding, char8 )

import Foreign.C.String ( CString, peekCString )

import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( try, finally, catch, IOException )
import System.IO.Error ( ioeGetErrorType )
import GHC.IO.Exception ( IOErrorType(ResourceVanished) )
import Data.Char ( toUpper, toLower )
import Text.Regex
#if defined (HAVE_MAPI)
import Foreign.C ( withCString )
#endif
#ifdef HAVE_MAPI
import Foreign.Ptr ( nullPtr )
import Darcs.Repository.Lock ( canonFilename, writeDocBinFile )
#endif

import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.UI.Options.All ( Sign(..), Verify(..), Compression(..) )
import Darcs.Util.Path
    ( AbsolutePath
    , toFilePath
    , FilePathLike
    )
import Darcs.Util.Progress ( withoutProgress, debugMessage )

import Darcs.Util.ByteString (linesPS, unlinesPS)
import qualified Data.ByteString as B (ByteString, empty, null, readFile
            ,hGetContents, writeFile, hPut, length
            ,take, concat, drop, isPrefixOf, singleton, append)
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Darcs.Repository.Lock
    ( withTemp
    , withNamedTemp
    , withOpenTemp
    )
import Darcs.Repository.Ssh ( getSSH, SSHCmd(..) )
import Darcs.Util.CommandLine ( parseCmd, addUrlencoded )
import Darcs.Util.Exec ( execInteractive, exec, Redirect(..), withoutNonBlock )
import Darcs.Util.URL ( SshFilePath, sshUhost )
import Darcs.Util.Printer ( Doc, Printers, hPutDocLnWith, hPutDoc, hPutDocLn, hPutDocWith, ($$), renderPS,
                 simplePrinters,
                 hPutDocCompr,
                 text, empty, packedString, vcat, renderString, RenderMode(..) )
import qualified Darcs.Util.Ratified as Ratified
import Darcs.UI.Email ( formatHeader )

sendmailPath :: IO String
sendmailPath = do
  l <- filterM doesFileExist $ liftM2 (</>)
                [ "/usr/sbin", "/sbin", "/usr/lib" ]
                [ "sendmail" ]
  ex <- findExecutable "sendmail"
  when (isNothing ex && null l) $ fail "Cannot find the \"sendmail\" program."
  return $ head $ maybeToList ex ++ l

diffProgram :: IO String
diffProgram = do
  l <- filterM (fmap isJust . findExecutable) [ "gdiff", "gnudiff", "diff" ]
  when (null l) $ fail "Cannot find the \"diff\" program."
  return $ head l

-- |Get the name of the darcs executable (as supplied by @getExecutablePath@)
darcsProgram :: IO String
#if __GLASGOW_HASKELL__ >= 706
darcsProgram = getExecutablePath
#else
darcsProgram = getProgName
#endif

maybeURLCmd :: String -> String -> IO (Maybe String)
maybeURLCmd what url =
  do let prot = map toUpper $ takeWhile (/= ':') url
     fmap Just (getEnv ("DARCS_" ++ what ++ "_" ++ prot))
             `catch` \(_ :: IOException) -> return Nothing

pipeDoc :: RenderMode -> String -> [String] -> Doc -> IO ExitCode
pipeDoc = pipeDocInternal (PipeToOther simplePrinters)

data WhereToPipe = PipeToSsh Compression -- ^ if pipe to ssh, can choose to compress or not
                 | PipeToOther Printers  -- ^ otherwise, can specify printers

pipeDocInternal :: WhereToPipe -> RenderMode -> String -> [String] -> Doc -> IO ExitCode
pipeDocInternal whereToPipe target c args inp = withoutNonBlock $ withoutProgress $
    do debugMessage $ unwords (c:args)
       (Just i,_,_,pid) <- createProcess (proc c args){ std_in = CreatePipe
                                                   {- , delegate_ctlc = True -- requires process 1.2.2.0 -} }
       debugMessage "Start transferring data"
       case whereToPipe of
          PipeToSsh GzipCompression -> hPutDocCompr target i inp
          PipeToSsh NoCompression   -> hPutDoc target i inp
          PipeToOther printers      -> hPutDocWith printers target i inp
       hClose i
       rval <- waitForProcess pid
       debugMessage "Finished transferring data"
       when (rval == ExitFailure 127) $
            putStrLn $ "Command not found:\n   "++ show (c:args)
       return rval

pipeDocSSH :: Compression -> RenderMode -> SshFilePath -> [String] -> Doc -> IO ExitCode
pipeDocSSH compress target remoteAddr args input = do
    (ssh, ssh_args) <- getSSH SSH
    pipeDocInternal (PipeToSsh compress) target ssh (ssh_args++ (sshUhost remoteAddr:args)) input

sendEmail :: String -> String -> String -> String -> String -> String -> IO ()
sendEmail f t s cc scmd body =
  sendEmailDoc f t s cc scmd Nothing (text body)


generateEmail
    :: Handle  -- ^ handle to write email to
    -> String  -- ^ From
    -> String  -- ^ To
    -> String  -- ^ Subject
    -> String  -- ^ CC
    -> Doc     -- ^ body
    -> IO ()
generateEmail h f t s cc body = do
     putHeader "To" t
     putHeader "From" f
     putHeader "Subject" s
     unless (null cc) $ putHeader "Cc" cc
     putHeader "X-Mail-Originator" "Darcs Version Control System"
     hPutDocLn Standard h body
  where putHeader field value
            = B.hPut h (B.append (formatHeader field value) newline)
        newline = B.singleton 10

haveSendmail :: IO Bool
haveSendmail = (sendmailPath >> return True) `catch` (\(_ :: IOException) -> return False)

-- | Send an email, optionally containing a patch bundle
--   (more precisely, its description and the bundle itself)
sendEmailDoc
  :: String           -- ^ from
  -> String           -- ^ to
  -> String           -- ^ subject
  -> String           -- ^ cc
  -> String           -- ^ send command
  -> Maybe (Doc, Doc) -- ^ (content,bundle)
  -> Doc              -- ^ body
  -> IO ()
sendEmailDoc _ "" _ "" _ _ _ = return ()
sendEmailDoc f "" s cc scmd mbundle body =
  sendEmailDoc f cc s "" scmd mbundle body
sendEmailDoc f t s cc scmd mbundle body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= "" then
    withOpenTemp $ \(h,fn) -> do
      generateEmail h f t s cc body
      hClose h
      withOpenTemp $ \(hat,at) -> do
        ftable' <- case mbundle of
                   Just (content,bundle) -> do
                       hPutDocLn Standard hat bundle
                       return [ ('b', renderString Standard content) , ('a', at) ]
                   Nothing ->
                       return [ ('b', renderString Standard body) ]
        hClose hat
        let ftable = [ ('t',addressOnly t),('c',cc),('f',f),('s',s) ] ++ ftable'
        r <- execSendmail ftable scmd fn
        when (r /= ExitSuccess) $ fail ("failed to send mail to: "
                                       ++ t ++ cc_list cc
                                       ++ "\nPerhaps sendmail is not configured.")
#ifdef HAVE_MAPI
   else do
     r <- withCString t $ \tp ->
           withCString f $ \fp ->
            withCString cc $ \ccp ->
             withCString s $ \sp ->
              withOpenTemp $ \(h,fn) -> do
               hPutDoc Standard h body
               hClose h
               writeDocBinFile "mailed_patch" body
               cfn <- canonFilename fn
               withCString cfn $ \pcfn ->
                c_send_email fp tp ccp sp nullPtr pcfn
     when (r /= 0) $ fail ("failed to send mail to: " ++ t)
#else
   else fail "no mail facility (sendmail or mapi) located at configure time!"
#endif
  where addressOnly a =
          case dropWhile (/= '<') a of
          ('<':a2) -> takeWhile (/= '>') a2
          _        -> a

        cc_list [] = []
        cc_list c = " and cc'ed " ++ c

resendEmail :: String -> String -> B.ByteString -> IO ()
resendEmail "" _ _ = return ()
resendEmail t scmd body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= ""
   then
    withOpenTemp $ \(h,fn) -> do
     hPutStrLn h $ "To: "++ t
     hPutStrLn h $ find_from (linesPS body)
     hPutStrLn h $ find_subject (linesPS body)
     hPutDocLn Standard h $ fixit $ linesPS body
     hClose h
     let ftable = [('t',t)]
     r <-  execSendmail ftable scmd fn
     when (r /= ExitSuccess) $ fail ("failed to send mail to: " ++ t)
   else
#ifdef HAVE_MAPI
    fail "Don't know how to resend email with MAPI"
#else
    fail "no mail facility (sendmail or mapi) located at configure time (use the sendmail-command option)!"
#endif
  where br            = BC.pack "\r"
        darcsurl      = BC.pack "DarcsURL:"
        content       = BC.pack "Content-"
        from_start    = BC.pack "From:"
        subject_start = BC.pack "Subject:"
        fixit (l:ls)
         | B.null l = packedString B.empty $$ vcat (map packedString ls)
         | l == br = packedString B.empty $$ vcat (map packedString ls)
         | B.take 9 l == darcsurl || B.take 8 l == content
            = packedString l $$ fixit ls
         | otherwise = fixit ls
        fixit [] = empty
        find_from (l:ls) | B.take 5 l == from_start = BC.unpack l
                         | otherwise = find_from ls
        find_from [] = "From: unknown"
        find_subject (l:ls) | B.take 8 l == subject_start = BC.unpack l
                            | otherwise = find_subject ls
        find_subject [] = "Subject: (no subject)"

execSendmail :: [(Char,String)] -> String -> String -> IO ExitCode
execSendmail ftable scmd fn =
  if scmd == "" then do
     cmd <- sendmailPath
     exec cmd ["-i", "-t"] (File fn, Null, AsIs)
  else case parseCmd (addUrlencoded ftable) scmd of
         Right (arg0:opts, wantstdin) ->
           do let stdin = if wantstdin then File fn else Null
              exec arg0 opts (stdin, Null, AsIs)
         Left e -> fail $ "failed to send mail, invalid sendmail-command: "++show e
         _ -> fail "failed to send mail, invalid sendmail-command"

#ifdef HAVE_MAPI
foreign import ccall "win32/send_email.h send_email" c_send_email
             :: CString -> {- sender -}
                CString -> {- recipient -}
                CString -> {- cc -}
                CString -> {- subject -}
                CString -> {- body -}
                CString -> {- path -}
                IO Int
#endif

execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap (renderPS Standard)
                     $ execDocPipe Standard c args
                     $ packedString ps

execAndGetOutput :: RenderMode -> FilePath -> [String] -> Doc
                 -> IO (ProcessHandle, MVar (), B.ByteString)
execAndGetOutput target c args instr = do
       (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       _ <- forkIO $ hPutDoc target i instr >> hClose i
       mvare <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
       return (pid, mvare, out)

execDocPipe :: RenderMode -> String -> [String] -> Doc -> IO Doc
execDocPipe target c args instr = withoutProgress $ do
       (pid, mvare, out) <- execAndGetOutput target c args instr
       rval <- waitForProcess pid
       takeMVar mvare
       case rval of
         ExitFailure ec ->fail $ "External program '"++c++
                          "' failed with exit code "++ show ec
         ExitSuccess -> return $ packedString out

-- The following is needed for diff, which returns non-zero whenever
-- the files differ.
execPipeIgnoreError :: RenderMode -> String -> [String] -> Doc -> IO Doc
execPipeIgnoreError target c args instr = withoutProgress $ do
       (pid, mvare, out) <- execAndGetOutput target c args instr
       _ <- waitForProcess pid
       takeMVar mvare
       return $ if B.null out then empty else packedString out

signString :: Sign -> Doc -> IO Doc
signString NoSign d = return d
signString Sign d = signPGP [] d
signString (SignAs keyid) d = signPGP ["--local-user", keyid] d
signString (SignSSL idf) d = signSSL idf d

signPGP :: [String] -> Doc -> IO Doc
signPGP args = execDocPipe Standard "gpg" ("--clearsign":args)

signSSL :: String -> Doc -> IO Doc
signSSL idfile t =
    withTemp $ \cert -> do
    opensslPS ["req", "-new", "-key", idfile,
               "-outform", "PEM", "-days", "365"]
                (BC.pack "\n\n\n\n\n\n\n\n\n\n\n")
                >>= opensslPS ["x509", "-req", "-extensions",
                               "v3_ca", "-signkey", idfile,
                               "-outform", "PEM", "-days", "365"]
                >>= opensslPS ["x509", "-outform", "PEM"]
                >>= B.writeFile cert
    opensslDoc ["smime", "-sign", "-signer", cert,
                "-inkey", idfile, "-noattr", "-text"] t
    where opensslDoc = execDocPipe Standard "openssl"
          opensslPS = execPSPipe "openssl"


verifyPS :: Verify -> B.ByteString -> IO (Maybe B.ByteString)
verifyPS NoVerify ps = return $ Just ps
verifyPS (VerifyKeyring pks) ps = verifyGPG pks ps
verifyPS (VerifySSL auks) ps = verifySSL auks ps

verifyGPG :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifyGPG goodkeys s =
    withOpenTemp $ \(th,tn) -> do
      B.hPut th s
      hClose th
      rval <- exec "gpg"  ["--batch","--no-default-keyring",
                           "--keyring",fix_path $ toFilePath goodkeys, "--verify"]
                           (File tn, Null, Null)
      case rval of
          ExitSuccess -> return $ Just gpg_fixed_s
          _ -> return Nothing
      where gpg_fixed_s = let
                not_begin_signature x =
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----"
                    &&
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----\r"
                in unlinesPS $ map fix_line $ tail $ dropWhile not_begin_signature $ linesPS s
            fix_line x | B.length x < 3 = x
                       | BC.pack "- -" `B.isPrefixOf` x = B.drop 2 x
                       | otherwise = x
#if defined(WIN32)
            fix_sep c | c=='/' = '\\'   | otherwise = c
            fix_path p = map fix_sep p
#else
            fix_path p = p
#endif

verifySSL :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifySSL goodkeys s = do
    certdata <- opensslPS ["smime", "-pk7out"] s
                >>= opensslPS ["pkcs7", "-print_certs"]
    cruddy_pk <- opensslPS ["x509", "-pubkey"] certdata
    let key_used = B.concat $ tail $
                   takeWhile (/= BC.pack"-----END PUBLIC KEY-----")
                           $ linesPS cruddy_pk
        in do allowed_keys <- linesPS `fmap` B.readFile (toFilePath goodkeys)
              if key_used `notElem` allowed_keys
                then return Nothing -- Not an allowed key!
                else withTemp $ \cert ->
                     withTemp $ \on ->
                     withOpenTemp $ \(th,tn) -> do
                     B.hPut th s
                     hClose th
                     B.writeFile cert certdata
                     rval <- exec "openssl" ["smime", "-verify", "-CAfile",
                                             cert, "-certfile", cert]
                                             (File tn, File on, Null)
                     case rval of
                       ExitSuccess -> Just `fmap` B.readFile on
                       _ -> return Nothing
    where opensslPS = execPSPipe "openssl"

viewDoc :: Doc -> IO ()
viewDoc = viewDocWith simplePrinters Encode

viewDocWith :: Printers -> RenderMode -> Doc -> IO ()
viewDocWith pr mode msg = do
  isTerminal <- hIsTerminalDevice stdout
  void $ if isTerminal && lengthGreaterThan (20 :: Int) (lines $ renderString mode msg)
     then do mbViewerPlusArgs <- getViewer
             case mbViewerPlusArgs of
                  Just viewerPlusArgs -> do
                    let (viewer : args) = words viewerPlusArgs
                    pipeDocToPager viewer args pr mode msg
                  Nothing -> return $ ExitFailure 127 -- No such command
               -- TEMPORARY passing the -K option should be removed as soon as
               -- we can use the delegate_ctrl_c feature in process
               `ortryrunning` pipeDocToPager  "less" ["-RK"] pr mode msg
               `ortryrunning` pipeDocToPager  "more" [] pr mode msg
#ifdef WIN32
               `ortryrunning` pipeDocToPager  "more.com" [] pr mode msg
#endif
               `ortryrunning` pipeDocToPager "" [] pr mode msg
     else pipeDocToPager "" [] pr mode msg
              where lengthGreaterThan n _ | n <= 0 = True
                    lengthGreaterThan _ [] = False
                    lengthGreaterThan n (_:xs) = lengthGreaterThan (n-1) xs

getViewer :: IO (Maybe String)
getViewer = Just `fmap` (getEnv "DARCS_PAGER" `catchall` getEnv "PAGER")
            `catchall`
            return Nothing

pipeDocToPager :: String -> [String] -> Printers -> RenderMode -> Doc -> IO ExitCode

pipeDocToPager "" _ pr mode inp = do
  hPutDocLnWith pr mode stdout inp
  return ExitSuccess

pipeDocToPager c args pr mode inp = pipeDocInternal (PipeToOther pr) mode c args inp

-- | Given two shell commands as arguments, execute the former.  The
-- latter is then executed if the former failed because the executable
-- wasn't found (code 127), wasn't executable (code 126) or some other
-- exception occurred (save from a resource vanished/broken pipe error).
-- Other failures (such as the user holding ^C)
-- do not cause the second command to be tried.
ortryrunning :: IO ExitCode
             -> IO ExitCode
             -> IO ExitCode
a `ortryrunning` b = do
  ret <- try a
  case ret of
    (Right (ExitFailure 126)) -> b -- command not executable
    (Right (ExitFailure 127)) -> b -- command not found
#ifdef WIN32
    (Right (ExitFailure 9009)) -> b -- command not found by cmd.exe on Windows
#endif
    (Right x) -> return x          -- legitimate success/failure
    (Left (e :: IOException)) -> case ioeGetErrorType e of
                                   -- case where pager is quit before darcs has fed it entirely:
                                   ResourceVanished -> return ExitSuccess
                                   -- other exception:
                                   _                -> b


editText :: String -> B.ByteString -> IO B.ByteString
editText desc txt = withNamedTemp desc $ \f -> do
  B.writeFile f txt
  _ <- runEditor f
  B.readFile f

-- | @editFile f@ lets the user edit a file which could but does not need to
-- already exist. This function returns the exit code from the text editor and a
-- flag indicating if the user made any changes.
editFile :: FilePathLike p
         => p
         -> IO (ExitCode, Bool)
editFile ff = do
    old_content <- file_content
    ec <- runEditor f
    new_content <- file_content
    return (ec, new_content /= old_content)
  where
    f = toFilePath ff
    file_content = do
      exists <- doesFileExist f
      if exists then do content <- B.readFile f
                        return $ Just content
                else return Nothing


runEditor :: FilePath
          -> IO ExitCode
runEditor f = do
    ed <- getEditor
    execInteractive ed f
         `ortryrunning` execInteractive "vi" f
         `ortryrunning` execInteractive "emacs" f
         `ortryrunning` execInteractive "emacs -nw" f
#ifdef WIN32
         `ortryrunning` execInteractive "edit" f
#endif


getEditor :: IO String
getEditor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "DARCSEDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "nano"

catchall :: IO a
         -> IO a
         -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)


-- | In some environments, darcs requires that certain global GHC library variables that
-- control the encoding used in internal translations are set to specific values.
--
-- @setDarcsEncoding@ enforces those settings, and should be called before the
-- first time any darcs operation is run, and again if anything else might have
-- set those encodings to different values.
--
-- Note that it isn't thread-safe and has a global effect on your program.
--
-- The current behaviour of this function is as follows, though this may
-- change in future:
--
-- Encodings are only set on GHC 7.4 and up, on any non-Windows platform.
--
-- Two encodings are set, both to @GHC.IO.Encoding.char8@:
-- @GHC.IO.Encoding.setFileSystemEncoding@ and @GHC.IO.Encoding.setForeignEncoding@.
--
-- Prevent HLint from warning us about a redundant do if the macro isn't
-- defined:
setDarcsEncodings :: IO ()
setDarcsEncodings = do

-- This is needed for appropriate behaviour from getArgs and from general
-- filesystem calls (e.g. getDirectoryContents, readFile, ...)
    setFileSystemEncoding char8

-- This ensures that foreign calls made by hashed-storage to stat
-- filenames returned from getDirectoryContents are translated appropriately
    setForeignEncoding char8

    return ()

-- The following functions are copied from the encoding package (BSD3
-- licence, by Henning Günther).

-- | @getSystemEncoding@ fetches the current encoding from locale
foreign import ccall "system_encoding.h get_system_encoding"
     get_system_encoding :: IO CString


getSystemEncoding :: IO String
getSystemEncoding = do
    enc <- get_system_encoding
    peekCString enc


-- | @isUTF8@ checks if an encoding is UTF-8 (or ascii, since it is a
-- subset of UTF-8).
isUTF8Locale :: String -> Bool
isUTF8Locale codeName = case normalizeEncoding codeName of
    -- ASCII
    "ascii"              -> True
    "646"                -> True
    "ansi_x3_4_1968"     -> True
    "ansi_x3.4_1986"     -> True
    "cp367"              -> True
    "csascii"            -> True
    "ibm367"             -> True
    "iso646_us"          -> True
    "iso_646.irv_1991"   -> True
    "iso_ir_6"           -> True
    "us"                 -> True
    "us_ascii"           -> True
    -- UTF-8
    "utf_8"              -> True
    "u8"                 -> True
    "utf"                -> True
    "utf8"               -> True
    "utf8_ucs2"          -> True
    "utf8_ucs4"          -> True
    -- Everything else
    _                    -> False
  where
    normalizeEncoding s = map toLower $ subRegex sep s "_"
    sep = mkRegex "[^0-9A-Za-z]+"
