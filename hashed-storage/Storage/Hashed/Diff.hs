module Storage.Hashed.Diff where

import Prelude hiding ( lookup, filter )
import qualified Data.ByteString.Lazy.Char8 as BL
import Storage.Hashed.Tree
import Storage.Hashed.AnchoredPath
import Data.List.LCS
import Data.List ( groupBy )

unidiff :: Tree IO -> Tree IO -> IO BL.ByteString
unidiff l r =
    do (from, to) <- diffTrees l r
       diffs <- sequence $ zipCommonFiles diff from to
       return $ BL.concat diffs
    where diff p a b = do x <- readBlob a
                          y <- readBlob b
                          return $ diff' p x y
          diff' p x y =
              case unifiedDiff x y of
                x' | BL.null x' -> BL.empty
                   | otherwise ->
                       (BL.pack $ "--- " ++ anchorPath "old" p ++ "\n" ++
                              "+++ " ++ anchorPath "new" p ++ "\n")
                       `BL.append` x'

type Line = BL.ByteString
data WeaveLine = Common Line
               | Remove Line
               | Add Line
               | Replace Line Line
               | Skip Int deriving Show

-- | A weave -- two files woven together, with common and differing regions
-- marked up. Cf. 'WeaveLine'.
type Weave = [WeaveLine]

-- | Sort of a sub-weave.
type Hunk = [WeaveLine]

-- | Produce unified diff (in a string form, ie. formatted) from a pair of
-- bytestrings.
unifiedDiff :: BL.ByteString -> BL.ByteString -> BL.ByteString
unifiedDiff a b = printUnified $ concat unifiedHunks
    where unifiedHunks = reduceContext 3 $ map unifyHunk $ hunks $ weave a b

-- | Weave two bytestrings. Intermediate data structure for the actual unidiff
-- implementation. No skips are produced.
weave :: BL.ByteString -> BL.ByteString -> Weave
weave a' b' = weave' left common right
    where left = init' (BL.split '\n' a') -- drop trailing newline
          right = init' (BL.split '\n' b') -- drop trailing newline
          init' [] = []
          init' x = init x
          common = lcs left right
          weave' []     []     [] = []
          weave' []     c      [] = error $ "oops: Left & Right empty, Common: " ++ show c
          weave' []     []     (b:bs) = Add b : weave' [] [] bs
          weave' (a:as) []     [] = Remove a : weave' as [] []
          weave' (a:as) []     (b:bs) = Replace a b : weave' as [] bs
          weave' (a:as) (c:cs) (b:bs)
                 | a == c && b == c = Common a : weave' as cs bs
                 | a == c && b /= c = Add b : weave' (a:as) (c:cs) bs
                 | a /= c && b == c = Remove a : weave' as (c:cs) (b:bs)
                 | a /= c && b /= c = Replace a b : weave' as (c:cs) bs
                 | otherwise = error "oops!"
          weave' a c b = error $ "oops: \nLeft: " ++ show a ++ "\nCommon: " ++ show c ++ "\nRight: " ++ show b

-- | Break up a 'Weave' into 'Hunk's.
hunks :: Weave -> [Hunk]
hunks = groupBy grp
    where grp (Common _) (Common _) = True
          grp (Common _) _ = False
          grp _ (Common _) = False
          grp _ _ = True

-- | Reformat a 'Hunk' into a format suitable for unified diff. Replaces are
-- turned into add/remove pairs, all removals in a hunk go before all
-- adds. 'Hunk's of 'Common' lines are left intact. Produces input suitable for
-- 'reduceContext'.
unifyHunk :: Hunk -> Hunk
unifyHunk h = case h of
                (Common _:_) -> h
                _ -> reorder $ concatMap breakup h
    where reorder h' = [ Remove a | Remove a <- h' ] ++ [ Add a | Add a <- h' ]
          breakup (Replace f t) = [Remove f, Add t]
          breakup x = [x]

-- | Break up a 'Weave' into unified 'Hunk's, leaving @n@ lines of context around
-- every hunk. Consecutive 'Common' lines not used as context are replaced with
-- 'Skip's.
reduceContext :: Int -> [Hunk] -> [Hunk]
reduceContext n hs =
    case hs of
      [] -> []
      [Common _:_] -> []
      [x] -> [x]
      [h,t] -> [reduce 0 n h, reduce n 0 t]
      (h:rest) -> reduce 0 n h :
                    map (reduce n n) (init rest) ++
                    [reduce n 0 $ last rest]
    where
      reduce s e h@(Common _:_)
          | length h <= s + e = h
          | otherwise = take s h ++
                        [Skip $ length h - e - s ] ++
                        drop (length h - e) h
      reduce _ _ h = h

-- | Format a 'Weave' for printing.
deweave :: Weave -> BL.ByteString
deweave = BL.unlines . map disp
    where disp (Common l) = BL.cons ' ' l
          disp (Remove l) = BL.cons '-' l
          disp (Add l) = BL.cons '+' l
          disp (Replace _ t) = BL.cons '!' t
          disp (Skip n) = BL.pack $ "-- skip " ++ show n ++ " lines --"

-- | Print a \"hunked\" weave in form of an unified diff. 'Hunk' boundaries are
-- marked up as 'Skip' lines. Cf. 'reduceContext'.
printUnified :: Weave -> BL.ByteString
printUnified hunked = printHunks 1 1 $ groupBy splits hunked
    where splits (Skip _) _ = False
          splits _ (Skip _) = False
          splits _ _ = True
          printHunks _ _ [] = BL.empty
          printHunks l r ([Skip n]:rest) =
              printHunks (n+l) (n+r) rest
          printHunks l r (h:rest) =
              (BL.pack $ "@@ -" ++ show l ++ "," ++
                show (removals h) ++ " +" ++ show r ++
                "," ++ show (adds h) ++ " @@\n")
              `BL.append` deweave h `BL.append`
               printHunks (l + removals h) (r + adds h) rest
          commons h = length [ () | (Common _) <- h ]
          adds h = commons h + length [ () | (Add _) <- h ]
          removals h = commons h + length [ () | (Remove _) <- h ]
