module Darcs.Util.Prompt
    (
    -- * User prompts
      askEnter
    , askUser
    , askUserListItem
    , PromptConfig(..)
    , promptYorn
    , promptChar
    ) where


import Prelude hiding ( catch )

import Control.Monad ( void )

import Data.Char ( toUpper, toLower, isSpace )

import System.Console.Haskeline ( runInputT, defaultSettings, getInputLine,
                                  getInputChar, outputStr, outputStrLn )

import Darcs.Util.Progress ( withoutProgress )

-- | Ask the user for a line of input.
askUser :: String    -- ^ The prompt to display
        -> IO String -- ^ The string the user entered.
askUser prompt = withoutProgress $ runInputT defaultSettings $
                    getInputLine prompt
                        >>= maybe (error "askUser: unexpected end of input") return

-- | Ask the user to press Enter
askEnter :: String  -- ^ The prompt to display
         -> IO ()
askEnter prompt = void $ askUser prompt

-- | @askUserListItem prompt xs@ enumerates @xs@ on the screen, allowing
--   the user to choose one of the items
askUserListItem :: String
                -> [String]
                -> IO String
askUserListItem prompt xs = withoutProgress $ runInputT defaultSettings $ do
    outputStr . unlines $ zipWith (\n x -> show n ++ ". " ++ x) [1::Int ..] xs
    loop
  where
    loop = do
      answer <- getInputLine prompt
                  >>= maybe (error "askUser: unexpected end of input") return
      case maybeRead answer of
        Just n | n > 0 && n <= length xs -> return (xs !! (n-1))
        _ -> outputStrLn "Invalid response, try again!" >> loop


maybeRead :: Read a
          => String
          -> Maybe a
maybeRead s = case reads s of
    [(x, rest)] | all isSpace rest -> Just x
    _         -> Nothing


data PromptConfig = PromptConfig { pPrompt :: String
                                 , pBasicCharacters :: [Char]
                                 , pAdvancedCharacters :: [Char] -- ^ only shown on help
                                 , pDefault :: Maybe Char
                                 , pHelp    :: [Char]
                                 }


-- | Prompt the user for a yes or no
promptYorn :: String -> IO Bool
promptYorn p = (== 'y') `fmap` promptChar (PromptConfig p "yn" [] Nothing [])


-- | Prompt the user for a character, among a list of possible ones.
--   Always returns a lowercase character. This is because the default
--   character (ie, the character shown in uppercase, that is automatically
--   selected when the user presses the space bar) is shown as uppercase,
--   hence users may want to enter it as uppercase.
promptChar :: PromptConfig -> IO Char
promptChar (PromptConfig p basic_chs adv_chs md help_chs) =
  withoutProgress $ runInputT defaultSettings loopChar
 where
 chs = basic_chs ++ adv_chs
 loopChar = do
    let chars = setDefault (basic_chs ++ (if null adv_chs then "" else "..."))
        prompt = p ++ " [" ++ chars ++ "]" ++ helpStr
    a <- getInputChar prompt >>= maybe (error "promptChar: unexpected end of input") (return . toLower)
    case () of
     _ | a `elem` chs                   -> return a
       | a == ' '                       -> maybe tryAgain return md
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 helpStr = case help_chs of
           []                      -> ""
           (h:_) | null adv_chs    -> ", or " ++ (h:" for help: ")
                 | otherwise       -> ", or " ++ (h:" for more options: ")
 tryAgain = do outputStrLn "Invalid response, try again!"
               loopChar
 setDefault s = case md of Nothing -> s
                           Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c

