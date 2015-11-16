{-# LANGUAGE OverloadedStrings #-}
--  Copyright (C) 2002-2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, TypeOperators #-}

-- | Help text and UI messages for @darcs send@
module Darcs.UI.Message.Send where

import Darcs.Util.Path ( FilePathLike(..), toFilePath )
import Darcs.UI.Commands ( formatPath )
import Darcs.Repository.Flags ( DryRun(..) )
import Darcs.Util.Text ( sentence )
import Darcs.Util.Printer

cmdDescription :: String
cmdDescription =
    "Prepare a bundle of patches to be applied to some target repository."

cmdHelp :: String
cmdHelp = unlines
    [ "Send is used to prepare a bundle of patches that can be applied to a target"
    , "repository.  Send accepts the URL of the repository as an argument.  When"
    , "called without an argument, send will use the most recent repository that"
    , "was either pushed to, pulled from or sent to.  By default, the patch bundle"
    , "is saved to a file, although you may directly send it by mail."
    , ""
    , "The `--output`, `--output-auto-name`, and `--to` flags determine"
    , "what darcs does with the patch bundle after creating it.  If you provide an"
    , "`--output` argument, the patch bundle is saved to that file.  If you"
    , "specify `--output-auto-name`, the patch bundle is saved to a file with an"
    , "automatically generated name.  If you give one or more `--to` arguments,"
    , "the bundle of patches is sent to those locations. The locations may either"
    , "be email addresses or urls that the patch should be submitted to via HTTP."
    , ""
    , "If you provide the `--mail` flag, darcs will look at the contents"
    , "of the `_darcs/prefs/email` file in the target repository (if it exists),"
    , "and send the patch by email to that address.  In this case, you may use"
    , "the `--cc` option to specify additional recipients without overriding the"
    , "default repository email address."
    , ""
    , "If `_darcs/prefs/post` exists in the target repository, darcs will"
    , "upload to the URL contained in that file, which may either be a"
    , "`mailto:` URL, or an `http://` URL.  In the latter case, the"
    , "patch is posted to that URL."
    , ""
    , "If there is no email address associated with the repository, darcs will"
    , "prompt you for an email address."
    , ""
    , "Use the `--subject` flag to set the subject of the e-mail to be sent."
    , "If you don't provide a subject on the command line, darcs will make one up"
    , "based on names of the patches in the patch bundle."
    , ""
    , "Use the `--in-reply-to` flag to set the In-Reply-To and References headers"
    , "of the e-mail to be sent. By default no additional headers are included so"
    , "e-mail will not be treated as reply by mail readers."
    , ""
    , "If you want to include a description or explanation along with the bundle"
    , "of patches, you need to specify the `--edit-description` flag, which"
    , "will cause darcs to open up an editor with which you can compose a message"
    , "to go along with your patches."
    , ""
    , "If you want to use a command different from the default one for sending"
    , "email, you need to specify a command line with the `--sendmail-command`"
    , "option. The command line can contain some format specifiers which are"
    , "replaced by the actual values. Accepted format specifiers are `%s` for"
    , "subject, `%t` for to, `%c` for cc, `%b` for the body of the mail, `%f` for"
    , "from, `%a` for the patch bundle and the same specifiers in uppercase for the"
    , "URL-encoded values."
    , "Additionally you can add `%<` to the end of the command line if the command"
    , "expects the complete email message on standard input. E.g. the command lines"
    , "for evolution and msmtp look like this:"
    , ""
    , "    evolution \"mailto:%T?subject=%S&attach=%A&cc=%C&body=%B\""
    , "    msmtp -t %<"
    , ""
    , "Do not confuse the `--author` options with the return address"
    , "that `darcs send` will set for your patch bundle."
    , ""
    , "For example, if you have two email addresses A and B:"
    , ""
    , "* If you use `--author A` but your machine is configured to send mail from"
    , "  address B by default, then the return address on your message will be B."
    , "* If you use `--from A` and your mail client supports setting the"
    , "  From: address arbitrarily (some non-Unix-like mail clients, especially,"
    , "  may not support this), then the return address will be A; if it does"
    , "  not support this, then the return address will be B."
    , "* If you supply neither `--from` nor `--author` then the return"
    , "  address will be B."
    , ""
    , "In addition, unless you specify the sendmail command with"
    , "`--sendmail-command`, darcs sends email using the default email"
    , "command on your computer. This default command is determined by the"
    , "`configure` script. Thus, on some non-Unix-like OSes,"
    , "`--from` is likely to not work at all."
    ]

cannotSendToSelf :: String
cannotSendToSelf = "Can't send to current repository! Did you mean send --context?"

creatingPatch :: String -> Doc
creatingPatch repodir = "Creating patch to" <+> text (formatPath repodir) <> "..."

noWorkingSendmail :: Doc
noWorkingSendmail = "No working sendmail instance on your machine!"

nothingSendable :: Doc
nothingSendable = "No recorded local changes to send!"

selectionIs :: [Doc] -> Doc
selectionIs descs = text "We have the following patches to send:" $$ vcat descs

selectionIsNull :: Doc
selectionIsNull = text "You don't want to send any patches, and that's fine with me!"

emailBackedUp :: String -> Doc
emailBackedUp mf = sentence $ "Email body left in" <+> text mf

promptCharSetWarning :: String -> String
promptCharSetWarning msg = "Warning: " ++ msg ++ "  Send anyway?"

charsetAborted :: Doc
charsetAborted = "Aborted.  You can specify charset with the --charset option."

charsetCouldNotGuess :: String
charsetCouldNotGuess = "darcs could not guess the charset of your mail."

currentEncodingIs :: String -> String
currentEncodingIs e = "Current locale encoding: " ++ e

charsetUtf8MailDiffLocale :: String
charsetUtf8MailDiffLocale = "your mail is valid UTF-8 but your locale differs."

aborted :: Doc
aborted = "Aborted."

success :: String -> String -> Doc
success to cc = sentence $
    "Successfully sent patch bundle to:" <+> text to <+> copies cc
  where
    copies "" = ""
    copies x  = "and cc'ed" <+> text x

postingPatch :: String -> Doc
postingPatch url = "Posting patch to" <+> text url

wroteBundle :: FilePathLike a => a -> Doc
wroteBundle a = sentence $ "Wrote patch to" <+> text (toFilePath a)

savedButNotSent :: String -> Doc
savedButNotSent to =
        text ("The usual recipent for this bundle is: " ++ to)
    $$  text "To send it automatically, make sure sendmail is working,"
    <+> text "and add 'send mail' to _darcs/prefs/defaults or"
    <+> text " ~/.darcs/defaults"

willSendTo :: DryRun -> [String] -> Doc
willSendTo dr addresses =
    "Patch bundle" <+> will <+> " be sent to:" <+> text (unwords addresses)
  where
    will = case dr of { YesDryRun -> "would"; NoDryRun  -> "will" }

promptTarget :: String
promptTarget = "What is the target email address? "

aboutToEdit :: FilePath -> String
aboutToEdit file = "About to edit file " ++ file

promptNoDescriptionChange :: String
promptNoDescriptionChange = "File content did not change. Continue anyway?"
