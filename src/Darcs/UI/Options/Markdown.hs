-- Support for @darcs help markdown@
module Darcs.UI.Options.Markdown ( optionsMarkdown ) where

import Data.Functor.Compose ( Compose(..) )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )
import Darcs.UI.Options.Util ( DarcsOptDescr )

optionsMarkdown :: [DarcsOptDescr f] -> String
optionsMarkdown opts = unlines
  [ "<table>", unlines (map optionMarkdown opts), "</table>" ]

optionMarkdown :: DarcsOptDescr f -> String
optionMarkdown (Compose (Option a b (NoArg _) h)) = unlines
 [ "<tr>", "<td>", showShortOptionsMd a, "</td>"
         , "<td>", showLongOptionsMd b , "</td>"
         , "<td>", h, "</td>"
 , "</tr>" ]
optionMarkdown (Compose (Option a b (ReqArg _ arg) h)) = unlines
 [ "<tr>", "<td>", showShortOptionsMd a, "</td>"
         , "<td>", showLongOptionsMd (map (++(' ' : arg)) b), "</td>"
         , "<td>", h, "</td>", "</tr>" ]
optionMarkdown (Compose (Option a b (OptArg _ arg) h)) = unlines
 [ "<tr>", "<td>", showShortOptionsMd a, "</td>"
         , "<td>", showLongOptionsMd (map (++("[="++arg++"]")) b), "</td>"
         , "<td>", h, "</td>", "</tr>" ]

showShortOptionsMd :: [Char] -> String
showShortOptionsMd []     = ""
showShortOptionsMd [c]    = "`-"++[c]++"` "
showShortOptionsMd (c:cs) = "`-"++[c]++"`,"++showShortOptionsMd cs

showLongOptionsMd :: [String] -> String
showLongOptionsMd []     = " "
showLongOptionsMd [s]    = "`--" ++ s ++ "` "
showLongOptionsMd (s:ss) = "`--" ++ s ++ "`,"++ showLongOptionsMd ss
