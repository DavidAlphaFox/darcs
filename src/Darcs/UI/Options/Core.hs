{-# LANGUAGE RecordWildCards #-}
{-| Option specifications using continuations with a changing answer type.

Based on

@<www.is.ocha.ac.jp/~asai/papers/tr08-2.pdf>@

with additional inspiration provided by

@<http://okmij.org/ftp/typed-formatting/FPrintScan.html#DSL-FIn>@

which shows how the same format specifiers can be used for both @sprintf@ and
@sscanf@.

The 'OptSpec' type corresponds to the format specifiers for the sprintf and
sscanf functions, which I called 'ounparse' and 'oparse' here; they no
longer work on 'String's but instead on any list (the intention is, of
course, that this is a list of flags).

As explained in the original paper by Kenichi Asai, we cannot use
'Control.Monad.Trans.Cont.Cont', even with the recent additions of the
@shift@ and @reset@ combinators, since 'Control.Monad.Trans.Cont.Cont'
requires that the answer type remains the same over the whole computation,
while the trick used here requires that the answer type can change.

Besides parsing and unparsing, the 'OptSpec' type contains two more members:
'odesc' is the list of 'OptDescr' that 'System.Console.GetOpt.getOpt' needs
as input for parsing the command line and for generating the usage help,
while 'ocheck' takes a list of flags and returns a list of error messages,
which can be used to check for conflicting options.

-}
module Darcs.UI.Options.Core where

import Prelude hiding ( (^) )
import Data.Monoid ( Monoid(..) )

import Darcs.UI.Options.Iso

-- * Option specifications

{-| A type for option specifications.

It consists of four components: a parser, an unparser, a checker, and a list
of descriptions.

The parser converts a flag list to some result value. This can never fail:
we demand that primitive parsers are written so that there is always a
default value (use 'Maybe' with default 'Nothing' as a last resort).

The unparser does the opposite of the parser: a value is converted back
to a flag list.

The checker returns a list of error messages (which should be empty if there
are no problems found). This can be used to e.g. check whether there are
conflicting flags in the list.

Separating the checker and parser is unusual. The reason for this is that we
want to support flags coming from multiple sources, such as the command line
or a defaults file. Prioritising these sources is done by concatenating the
flag lists in the order of precedence, so that earlier flags win over later
ones. That means that when parsing the (final) flag list, conflicting flags
are resolved by picking the first flag that matches an option. The checker,
on the other hand, can be called for each source separately.

The last component is a list of descriptors for each single switch/flag that
the option is made of.

The 'OptSpec' type is heavily parameterized. The type arguments are:

[@f@] The flag type, such as 'Darcs.UI.Flags.DarcsFlag'.

[@d@] A type that describes an single flag, such as
  'System.Console.GetOpt.OptDescr' or 'Darcs.UI.Options.DarcsOptDescr'. It
  should be a 'Data.Functor.Functor'.

Abstracting over these types is not technically necessary: for the intended
application in Darcs, we could as well fix them as
@d='Darcs.UI.Options.DarcsOptDescr'@, and @f='Darcs.UI.Flags.DarcsFlag'@,
saving two type parameters. However, doing that here would only obscure
what's going on, making the code harder to understand, not easier. Besides,
the resulting more general type signatures give us additional guarantees,
known as \"free theorems\" (free as in beer, not in speak).

In contrast, the type parameters

[@a@, @b@] are necessary to make chaining of options a la typed printf/scanf
  possible. In a nutshell, @a@ is the result type of a function that
  consumes the result of parsing or unparsing an option, while @b@ is the
  complete type of such a function.

The 'ounparse' and 'oparse' members use continuation passing style, which is
the reason for their apparently \"inverted\" type signature. To understand
them, it helps to look at the type of \"primitive\" (not yet combined)
options (see 'PrimOptSpec' below). For a primitive option, @b@ gets
instantiated to @v -> a@, where @v@ is the type of values associated with
the option. The whole option spec then has type

>  o :: 'OptSpec' d f a (v -> a)

so that the 'oparse' and 'ounparse' members are instantiated to

>  ounparse :: forall a. ([f] -> a) -> (x -> a)
>  oparse   :: forall a. (x -> a) -> ([f] -> a)

which can be easily seen to be equivalent to

>  ounparse :: x -> [f]
>  oparse   :: [f] -> x

Chaining such options results in a combined option of type

>  o1 ^ o2 ^ ... :: OptSpec d f a (v1 -> v2 -> ... -> a)

that is, @b@ gets instantiated to

>  v1 -> v2 -> ... -> a

To use such an option (primitive or combined), you pass in the consumer. A
typical consumer of option values is a command implementation. Given

>  cmd :: v1 -> v2 -> ... -> [String] -> IO ()

we can parse the flags and pass the results to @cmd@:

>  oparse (o1 ^ o2 ^ ...) cmd flags

-}
data OptSpec d f a b = OptSpec
  { ounparse :: ([f] -> a) -> b
    -- ^ Convert option value (back) to flag list, in CPS.
  , oparse :: b -> ([f] -> a)
    -- ^ Convert flag list to option value, in CPS. Note: as a pure
    -- function, it is not supposed to fail.
  , ocheck :: [f] -> [String]
    -- ^ Check for erros in a flag list, returns error messages.
  , odesc :: [d f]
    -- ^ Descriptions, one for each flag that makes up the option.
  }

-- ** Primitive combinators

{- $category

The type @'OptSpec' d f@, together with the operation '^' and the unit
'oid' forms a category. We could express this with an

@
  instance 'Control.Category.Category' ('OptSpec' d f) where
    'Control.Category.id' = 'oid'
    ('Control.Category..') = ('^')
@

I decided against doing that because I like the 'id' and '.' from the
"Prelude".

Proving the category laws is easy because the operation and unit are
implemented independently for each component. This means @'OptSpec' d f@
is simply the product of four categories, reducing the problem to proving
the laws for each component separately.

['odesc'] This is just list concatenation, which is a monoid, and every
  monoid is a category (by adding two phantom type arguments).

['ocheck'] Same here, noting that @([f] ->)@ is a monoid homomorphism
  (as expressed by the @instance 'Monoid' b => 'Monoid' (a -> b)@ in
  "Data.Monoid").

['oparse'] This can be seen by flipping the arguments (which is a functor
  i.e. preserves category laws), so the type becomes @[f] -> b -> a@, and
  noting as before that @([f] ->)@ is a monoid homomorphism and thus a
  functor (by adding two phantom type arguments), reducing the operation to
  simple function composition. If this rather abstract argument doesn't
  convince you, do the calculations as an exercise.

['ounparse'] for this I don't have an easy abstract argument at hand,
  so I'll do the calculation:

@
    o1 ^ (o2 ^ o3)
  =   definition outer (^)
    \k -> o1 (\f1 -> (o2 ^ o3) (\f23 -> k (f1 ++ f23)))
  =   definition inner (^)
    \k -> o1 (\f1 -> (\k' -> o2 (\f2 -> o3 (\f3 -> k' (f2 ++ f3)))) (\f23 -> k (f1 ++ f23)))
  =   beta reduce: f1 --> \f23 -> k (f1 ++ f23)
    \k -> o1 (\f1 -> (o2 (\f2 -> o3 (\f3 -> (\f23 -> k (f1 ++ f23)) (f2 ++ f3)))))
  =   beta reduce: f23 --> f2 ++ f3
    \k -> o1 (\f1 -> (o2 (\f2 -> o3 (\f3 -> (k (f1 ++ (f2 ++ f3)))))))
@

and from the other side:

@
    (o1 ^ o2) ^ o3
  =   definition outer (^)
    \k -> (o1 ^ o2) (\f12 -> o3 (\f3 -> k (f12 ++ f3)))
  =   definition inner (^)
    \k -> (\k' -> o1 (\f1 -> o2 (\f2 -> k' (f1 ++ f2)))) (\f12 -> o3 (\f3 -> k (f12 ++ f3)))
  =   beta reduce: k' --> \f12 -> o3 (\f3 -> k (f12 ++ f3))
    \k -> (o1 (\f1 -> o2 (\f2 -> (\f12 -> o3 (\f3 -> k (f12 ++ f3))) (f1 ++ f2))))
  =   beta reduce: f12 --> f1 ++ f2
    \k -> (o1 (\f1 -> o2 (\f2 -> (o3 (\f3 -> k ((f1 ++ f2) ++ f3))))))
@

so again we have reduced the problem to the associativity of @('++')@. Left
and right unit laws are left to the reader...

-}

-- | Identity 'OptSpec', unit for '^'
oid :: OptSpec d f a a
oid = OptSpec {..} where
  ounparse k = k []
  oparse k _ = k
  ocheck _ = []
  odesc = []

-- | 'OptSpec' composition, associative
(^) :: OptSpec d f b c -> OptSpec d f a b -> OptSpec d f a c
OptSpec ou1 op1 oc1 od1 ^ OptSpec ou2 op2 oc2 od2 = OptSpec {..} where
  ounparse k = ou1 (\fs1 -> ou2 (\fs2 -> k (fs1 ++ fs2)))
  oparse k fs = op2 (op1 k fs) fs
  ocheck fs = oc1 fs ++ oc2 fs
  odesc = od1 ++ od2

-- ** Derived combinators

-- | Normalise a flag list by parsing and then unparsing it. This adds all
-- implicit (default) flags to the list, which is useful as long as there is
-- legacy code that circumvents the 'OptSpec' abstraction and directly tests
-- for flag membership.
--
-- prop> onormalise opts = (oparse opts . ounparse opts) id
onormalise :: OptSpec d f [f] b -> [f] -> [f]
onormalise opts = (oparse opts . ounparse opts) id

-- | The list of default flags for an 'OptSpec'.
--
-- prop> defaultFlags opts = onormalise opts []
defaultFlags :: OptSpec d f [f] b -> [f]
defaultFlags opts = onormalise opts []

-- ** Lifting isomorphisms

-- | Lift an isomorphism between @b@ and @c@ to one between
-- @'OptSpec' d f a b@ and @'OptSpec' d f a c@.
--
-- The forward component of the 'Iso' is needed for 'ounparse', the backward
-- component for 'oparse'. For the other two components this is the identity.
oimap :: Iso b c -> OptSpec d f a b -> OptSpec d f a c
oimap (Iso fw bw) (OptSpec ou op oc od) = OptSpec {..} where
  ounparse k = fw (ou k)
  oparse k = op (bw k)
  ocheck = oc
  odesc = od

instance IsoFunctor (OptSpec d f a) where
  imap = oimap

-- * Primitive options

-- | Type of primitive (not yet combined) options. The type parameter @b@
-- gets instantiated to @(v -> a)@, adding one argument of type @v@
-- to the answer type of the continuation.
type PrimOptSpec d f a v = OptSpec d f a (v -> a)

-- | Combine two list valued options of the same type \"in parellel\". This
-- is done by concatenating the resulting option values ('oparse'), flags
-- ('ounparse'), errors ('ocheck'), and descriptors ('odesc'),
-- respectively, of the input options.
oappend :: PrimOptSpec d f a [v] -> PrimOptSpec d f a [v] -> PrimOptSpec d f a [v]
OptSpec ou1 op1 oc1 od1 `oappend` OptSpec ou2 op2 oc2 od2 = OptSpec {..} where
  ounparse k bs = ou1 (\fs1 -> ou2 (\fs2 -> k (fs1 ++ fs2)) bs) bs
  oparse k fs = op2 (\bs2 -> op1 (\bs1 -> k (bs1 ++ bs2)) fs) fs
  ocheck fs = oc1 fs ++ oc2 fs
  odesc = od1 ++ od2

-- | Unit for 'oappend'.
oempty :: PrimOptSpec d f a [v]
oempty = OptSpec {..} where
  ounparse k _ = k []
  oparse k _ = k []
  ocheck _ = []
  odesc = []

-- | See 'oappend' and 'oempty'.
instance Monoid (PrimOptSpec d f a [v]) where
  mappend = oappend
  mempty = oempty

-- | Parse a list of flags against a primitive option spec, returning the
-- value associated with the option. As noted above, this cannot fail because
-- options always have a default value.
--
-- prop> parseFlags o fs = oparse o id fs
parseFlags :: (forall a. PrimOptSpec d f a v) -> [f] -> v
parseFlags o fs = oparse o id fs
