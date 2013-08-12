{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  , UndecidableInstances
  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Dict
-- Copyright   :  (c) 2012 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Data structure versions of type classes relevant for \"toys\".  While it's
-- a bit of a design smell to have these value-level dictionaries for these
-- typeclasses, they can be very handy for \"toy\" demos and ghci.
--
--------------------------------------------------------------------------------
module Graphics.UI.Toy.Dict
  (
-- * Dictionary
    WithDict(..), Dictionary(..), withDict

-- * Interactive Dictionary
  , InteractiveDict(..), mkInteractiveDict

-- * Diagrammable Dictionary
  , DiagrammableDict(..)

-- * GtkInteractive Dictionary
  , ToyDict(..)
  , mkPureToyDict, mkMouseToyDict, mkDiagrammableToyDict , mkTraversableToyDict
  ) where

import Control.Arrow          ( first )
import Data.Default           ( Default(..) )
import Data.Semigroup         ( Semigroup )
import Data.Traversable       ( Traversable(..), foldMapDefault )

import Diagrams.Prelude
  ( V, QDiagram, InnerSpace, HasLinearMap, OrderedField, VectorSpace, Scalar
  , Juxtaposable(..), HasOrigin(..), Enveloped(..), Transformable(..)
  , juxtaposeDefault
  , liftA, mempty )

import Graphics.UI.Toy
  ( Interactive(..), InputState, MousePos, MouseEvent, KeyEvent
  , mousePos, keyHeld, simpleMouse, simpleKeyboard )

import Graphics.UI.Toy.Diagrams ( Diagrammable(..), Clickable(..) )


-------------------------------------------------------------------------------
-- WithDict data / Dictionary
-------------------------------------------------------------------------------

-- | @'WithDict' d a@ pairs a dictionary-like datatype along with a value.
--
-- In some ways this is like OOP, where a pointer to the vtable travels around
-- with the data.  For many typeclasses, we're able to write instances for
-- @WithDict@, such that the data is processed with the value-provided method
-- dictionary.
--
-- One thing to note is that, somewhat arbitrarily, the decision has been made
-- that this particular \"WithDict\" forwards along the implementations of
-- 'Juxtaposable', 'HasOrigin', 'Enveloped', 'Transformable', and 'Clickable'.
-- This means that creating \"*Dict\"s for these typeclasses, for use 'WithDict'
-- is not possible.
data WithDict d a = WithDict a (d a)

type instance V (WithDict d a) = V a

instance (Enveloped a, HasOrigin a, HasLinearMap (V a))
      => Juxtaposable (WithDict d a) where
  juxtapose = juxtaposeDefault

instance (HasOrigin a, VectorSpace (V a))
      => HasOrigin (WithDict d a) where
  moveOriginTo p (WithDict x d) = moveOriginTo p x `WithDict` d

instance (Enveloped a, v ~ V a, InnerSpace v, HasLinearMap v)
      => Enveloped (WithDict d a) where
  getEnvelope (WithDict x _) = getEnvelope x

instance (Transformable a, HasLinearMap (V a))
      => Transformable (WithDict d a) where
  transform t (WithDict x d) = transform t x `WithDict` d

instance Clickable a => Clickable (WithDict d a) where
  clickInside (WithDict x _) = clickInside x


-- | An instance of 'Dictionary' provides a 'dict', populated from an instance
--   of the actual typeclass.
class Dictionary d where
  dict :: d

-- | @'withDict' x@ wraps a value with a reified version of a dictionary.
withDict :: Dictionary (d a) => a -> WithDict d a
withDict x = WithDict x dict


-------------------------------------------------------------------------------
-- Interactive Dictionary
-------------------------------------------------------------------------------

-- | @'InteractiveDict' b a@ wraps all of the methods needed to make an
--   @'Interactive' b a@ instance.
data InteractiveDict b a = InteractiveDict
  { tickFunc     ::               InputState b -> a -> IO (a, Bool)
  , mouseFunc    :: MouseEvent -> InputState b -> a -> IO a
  , keyboardFunc :: KeyEvent   -> InputState b -> a -> IO a
  }

instance Interactive b a => Dictionary (InteractiveDict b a) where
  dict = InteractiveDict tick mouse keyboard

instance Interactive b (WithDict (InteractiveDict b) a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     d   i x
  mouse    m i (WithDict x d) = fmap (`WithDict` d)         $ mouseFunc    d m i x
  keyboard k i (WithDict x d) = fmap (`WithDict` d)         $ keyboardFunc d k i x

instance Default (InteractiveDict b a) where
  def = InteractiveDict
    { tickFunc = const $ return . (, False)
    , mouseFunc = const . const $ return
    , keyboardFunc = const . const $ return
    }

-- | 'mkInteractiveDict' takes simplified versions of 'tick', 'mouse',
--   and 'keyboard', and uses them to implement an @'InteractiveDict' b a@.
mkInteractiveDict
  :: (              InputState b -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   b -> a -> a)
  -> (KeyEvent                   -> a -> a)
  -> InteractiveDict b a
mkInteractiveDict tf mf kf = InteractiveDict
  (\i -> return . tf i)
  (simpleMouse mf)
  (simpleKeyboard kf)


-------------------------------------------------------------------------------
-- Diagrammable Dictionary
-------------------------------------------------------------------------------

-- | @'DiagrammableDict' b v q a@ wraps all of the methods needed to make an
--   instance of @'Diagrammable' b v a@.
data DiagrammableDict b v q a = DiagrammableDict
  { diagramFunc :: a -> QDiagram b v q }


instance Diagrammable b v q a => Dictionary (DiagrammableDict b v q a) where
  dict = DiagrammableDict diagram

instance (Semigroup q, InnerSpace v, HasLinearMap v, OrderedField (Scalar v))
      => Default (DiagrammableDict b v q a) where
  def = DiagrammableDict $ const mempty


-------------------------------------------------------------------------------
-- GtkInteractive Dictionary
-------------------------------------------------------------------------------

-- | @'ToyDict' a@ is similar to a data-dict version of the 'GtkInteractive'
--   constraint synonym, except that it's not gtk-specific.  This is because
--   display uses 'Diagrammable' (specifically, 'DiagrammableDict'), rather
--   than 'GtkDisplay'.
--
--   @'WithDict' ('ToyDict' ib b v Any) a@ is a good type to describe toys that
--   have their implementation constructed at runtime.
data ToyDict ib b v q a = ToyDict
  { diagrammableDict :: DiagrammableDict b v q a
  , interactiveDict :: InteractiveDict ib a
  }

instance (Interactive ib a, Diagrammable b v q a) => Dictionary (ToyDict ib b v q a) where
  dict = ToyDict dict dict

instance ( Semigroup q, InnerSpace v, HasLinearMap v, OrderedField (Scalar v) )
      => Default (ToyDict ib b v q a) where
  def = ToyDict def def

instance Interactive ib (WithDict (ToyDict ib b v q) a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     (interactiveDict d)   i x
  mouse    m i (WithDict x d) = fmap        (`WithDict` d)  $ mouseFunc    (interactiveDict d) m i x
  keyboard k i (WithDict x d) = fmap        (`WithDict` d)  $ keyboardFunc (interactiveDict d) k i x

instance Diagrammable b v q (WithDict (ToyDict ib b v q) a) where
  diagram (WithDict x d) = diagramFunc (diagrammableDict d) x

-- | @'mkPureToyDict'@ takes all of the parameters of @'mkInteractiveDict'@, with
--   an additional function to transform the state to a diagram.  The parameters
--   correspond to 'diagram', 'tick', 'mouse', and 'keyboard', respectively.
mkPureToyDict
  :: (a -> QDiagram b v q)
  -> (              InputState ib -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   ib -> a -> a)
  -> (KeyEvent                    -> a -> a)
  -> ToyDict ib b v q a
mkPureToyDict df tf mf kf = ToyDict
  (DiagrammableDict df)
  (mkInteractiveDict tf mf kf)

-- | @'mkMouseToyDict'@ is a convenience constructor that takes a diagram display
--   function and a function for transforming the state based on whether the
--   mouse is clicked and its position.
mkMouseToyDict
  :: (a -> QDiagram b v q)
  -> (Bool -> MousePos ib -> a -> a)
  -> ToyDict ib b v q a
mkMouseToyDict df mf = ToyDict
  (DiagrammableDict df)
  (InteractiveDict
    (\_ x -> return (x, False))
    (\_ i x -> return $ mf (keyHeld "Mouse1" i || keyHeld "Mouse2" i) (mousePos i) x)
    (\_ _ x -> return x))


-- | 'mkDiagrammableToyDict' yields a @'ToyDict'@ for any type that is
--   'Diagrammable' and 'Interactive'.
mkDiagrammableToyDict :: (Interactive ib a, Diagrammable b v q a)
                      => ToyDict ib b v q a
mkDiagrammableToyDict = ToyDict (DiagrammableDict $ diagramFunc dict) dict

-- | @'mkTraversableToyDict'@ provides a 'ToyDict' for any
--   @'Traversable'@ of interactive, diagrammable elements.  The resulting
--   dictionary implements these such that every subcomponent receives all
--   'tick', 'mouse', 'keyboard', and 'diagram' call.  The resulting
--   'CairoDiagram's are merged together via '(<>)'
mkTraversableToyDict
  :: ( Traversable t, Interactive ib a, Diagrammable b v q a
     , Semigroup q, HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
  => ToyDict ib b v q (t a)
mkTraversableToyDict = ToyDict
  (DiagrammableDict . foldMapDefault $ diagramFunc dict)
  (InteractiveDict
  -- TODO: or together the boolean results
    { tickFunc     = \  i x -> liftA (, True) $ traverse (liftA fst . tick i) x
    , mouseFunc    = \m i x -> traverse (mouse m i) x
    , keyboardFunc = \k i x -> traverse (keyboard k i) x
    })
