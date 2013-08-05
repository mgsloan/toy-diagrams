{-# LANGUAGE
   ConstraintKinds
 , FlexibleInstances
 , FlexibleContexts
 , GeneralizedNewtypeDeriving
 , MultiParamTypeClasses
 , ScopedTypeVariables
 , TemplateHaskell
 , TypeFamilies
 , TypeOperators
 , TypeSynonymInstances
 , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Button
-- Copyright   :  (c) 2012 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  mgsloan@gmail.com
--
-- Simple button UI element.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Button
  ( ButtonState(..), Button(..)

  -- * Lenses
  , buttonState, buttonHit, buttonDiagram

  -- * Mutation
  , clearButtonHit

  -- * Construction
  , mkButton
  ) where

import Control.Lens
import Data.AffineSpace.Point (Point(P))
import Diagrams.Prelude hiding (view)
import Diagrams.Lens

import Graphics.UI.Toy
import Graphics.UI.Toy.Diagrams

data ButtonState
  = NormalState
  | HoverState
  | PressState
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | A button stores the state necessary to know if the button is currently
--   being pressed ('_buttonHeld'), and if it was hit ('_buttonHit').  The
--   semantics of '_buttonHit' are up to the user, as it's up to the user to
--   call 'clearButtonHit' or otherwise set its value to @False@.
--
--   In order to draw the button, and figure out when mouse-clicks are inside
--   it, the function '_buttonDiagram' is used. It draws the button based on
--   the current '_buttonHeld' state.
data Button b v = Button
  { _buttonState   :: ButtonState -- ^ Whether the mouse is hovering / pressing.
  , _buttonHit     :: Bool        -- ^ Whether the button was hit.
  , _buttonDiagram :: Button b v -> Diagram b v
                                  -- ^ Draw button based on the state.
  }

type instance V (Button b v) = v

$(makeLenses ''Button)

-- | Builds a button, given the function used to draw it.  The first argument
--   of this function is a boolean that indicates whether it's held.
mkButton :: (Button b v -> Diagram b v) -> Button b v
mkButton = Button NormalState False

-- | This function literally just 'set's 'buttonHit' to 'False'.
clearButtonHit :: Button b v -> Button b v
clearButtonHit = set buttonHit False

instance ( Wrapped' v (MousePos ib)
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
      => Interactive ib (Button b v) where
  mouse m i b = return $ transition b
   where
    ci = clickInside b . P . view unwrapped' $ mousePos i
    transition = case (ci, m, b ^. buttonState) of
      (True, Just (True,  0),          _) -> buttonState .~ PressState
      (True, Just (False, 0), PressState) -> (buttonState .~ HoverState)
                                           . (buttonHit .~ True)
      (True,               _, PressState) -> id
      (True,               _,          _) -> buttonState .~ HoverState
      (False,              _,          _) -> buttonState .~ NormalState

instance Diagrammable b v Any (Button b v) where
  diagram x = (x ^. buttonDiagram) x

instance (InnerSpace v, HasLinearMap v, OrderedField (Scalar v))
      => Enveloped (Button b v) where
  getEnvelope b = getEnvelope (diagram b :: Diagram b v)

instance (InnerSpace v, HasLinearMap v, OrderedField (Scalar v))
      => Clickable (Button b v) where
  clickInside b = clickInside (diagram b :: Diagram b v)