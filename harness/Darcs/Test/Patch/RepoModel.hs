module Darcs.Test.Patch.RepoModel where
import Darcs.Patch.Apply ( Apply, ApplyState )
import Test.QuickCheck ( Gen )

type Fail = Either String

unFail :: Either [Char] t -> t
unFail (Right x) = x
unFail (Left err) = error $ "unFail failed: " ++ err

maybeFail :: Either t a -> Maybe a
maybeFail (Right x) = Just x
maybeFail _ = Nothing

class RepoModel model where
  type RepoState model :: (* -> *) -> *
  showModel :: model x -> String
  eqModel :: model x -> model x -> Bool
  aSmallRepo :: Gen (model x)
  repoApply :: (Apply p, ApplyState p ~ RepoState model) => model x -> p x y -> Fail (model y)

type family ModelOf (patch :: * -> * -> *) :: * -> *

