{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , ImpredicativeTypes
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Transformed
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- 'Transformed' allows 'Interactive', 'Diagrammable' things to be transformed
-- and overlapped, such that the layout combinators from diagrams apply.
--
-----------------------------------------------------------------------------

module Graphics.UI.Toy.Transformed ( Transformed(..), mkTransformed ) where

import Control.Arrow            ( first, second )
import Control.Newtype          ( Newtype, unpack, over, overF )
import Control.Newtype.TH       ( mkNewtype )
import Data.Data                ( Data, Typeable1 )
import Data.Foldable            ( foldMap )
import Diagrams.Prelude hiding  ( over )

import Graphics.UI.Toy          ( Interactive(..), InputState, MousePos )
import Graphics.UI.Toy.Diagrams ( Diagrammable(..), Clickable(..) )


-- | @'Transformed' a@ is like @[a]@, except that each element is stored with a
--   transformation that is used for 'Diagrammable' and 'Interactive'.
--
--   The semantics of the 'Monoid', 'Semigroup', 'Transformable', 'HasOrigin',
--   'Enveloped', and 'Juxtaposable' instances are all intended to mimic the
--   ones for 'Diagram'.  The 'Interactive', and 'Clickable' instances
--   appropriately transfrom the mouse coordinates into local coordinates.
newtype Transformed a = Transformed [(Transformation (V a), a)]
  deriving (Monoid, Semigroup)

deriving instance Typeable1 Transformed
deriving instance (Data a, Data (Transformation (V a))) => Data (Transformed a)

$(mkNewtype ''Transformed)

-- | @'mkTransformed' x@ constructs a 'Transformed' container for a single
--   object.
mkTransformed :: HasLinearMap (V a) => a -> Transformed a
mkTransformed = Transformed . (:[]) . (mempty, )

type instance V (Transformed a) = V a

instance HasLinearMap (V a) => HasOrigin (Transformed a) where
  moveOriginTo p = translate (origin .-. p)
  
instance HasLinearMap (V a) => Transformable (Transformed a) where
  transform a = Transformed `over` map (first (a <>))

instance (Enveloped a, HasLinearMap (V a))
      => Enveloped (Transformed a) where
  getEnvelope = foldMap (\(t, x) -> transform t $ getEnvelope x) . unpack

instance HasStyle a => HasStyle (Transformed a) where
  applyStyle s = Transformed `over` map (second $ applyStyle s)

instance ( v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable b v q a, Semigroup q)
        => Diagrammable b v q (Transformed a) where
  diagram = foldMap (\(t, x) -> transform t $ diagram x) . unpack

instance (Enveloped a, HasLinearMap (V a))
      => Juxtaposable (Transformed a) where
  juxtapose = juxtaposeDefault

instance ( Transformable (InputState b), V a ~ MousePos b, V a ~ V (InputState b)
         , Interactive b a )
      => Interactive b (Transformed a) where
  -- TODO: or together the boolean results
  tick     i = liftA (, True)
             . overInpT (\i' -> liftA fst . tick i') i
  mouse    m = overInpT (mouse m)
  keyboard k = overInpT (keyboard k)

overInpT :: ( Monad m, Functor m, Newtype n' [(Transformation (V t), a1)]
            , Transformable t, V a ~ V t )
         => (t -> a -> m a1) -> t -> Transformed a -> m n'
overInpT f i = Transformed `overM` mapM (\(t, x) -> (t,) <$> f (transform t i) x)

overM :: (Monad m, Functor m, Newtype n' o', Newtype n o)
      => (o -> n) -> (o -> m o') -> n -> m n'
overM x f = (x `overF` (>>= f)) . return

instance (Clickable a, HasLinearMap (V a))
      => Clickable (Transformed a) where
  clickInside d p = any (\(t, x) -> clickInside x $ transform t p) $ unpack d