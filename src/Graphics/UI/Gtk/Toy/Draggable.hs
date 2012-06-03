{-# LANGUAGE ConstraintKinds
           , DeriveDataTypeable
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , StandaloneDeriving
           , TemplateHaskell
           , TypeSynonymInstances
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Draggable
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Utilities for things that can be clicked and dragged around.
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Draggable
  ( Draggable(..), CairoDraggable

  -- * Lenses
  , dragState, dragOffset, dragContent

  -- * Interaction with mouse
  -- | Starts drag when mouse 1 (left) is pressed, and ends when released.
  , mouseDrag

  -- * Update
  , mkDraggable, startDrag, updateDrag, endDrag

  -- * Query
  , isDragging) where

import Control.Newtype (Newtype, pack, unpack, over, overF)
import Data.Data (Data, Typeable, Typeable2)
import Data.Label
import Data.Maybe (isJust)
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Graphics.Rendering.Diagrams.Points

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

-- | Draggable things are translatable, and store state during the drag
--   process.
data Draggable b a = Draggable 
  { _dragState :: Maybe (V a, V a)
  , _dragOffsetAcc :: V a
  , _dragContent :: a
  }

deriving instance (Read a, Read (V a)) => Read (Draggable b a)
deriving instance (Show a, Show (V a)) => Show (Draggable b a)
deriving instance (Data a, Data (V a), Data b) => Data (Draggable b a)
deriving instance Typeable2 Draggable


type CairoDraggable a = Draggable Cairo a

type instance V (Draggable b a) = V a

$(mkLabels [''Draggable])

instance ( V a ~ v, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable b a)
        => Diagrammable b (Draggable b a) where
  diagram d@(Draggable _ _ a)
    = translate (get dragOffset d) $ diagram a

instance ( V a ~ R2, Clickable a )
      => Interactive (Draggable b a) where
  mouse = simpleMouse mouseDrag

instance ( V a ~ R2, CairoDiagrammable a, Clickable a )
      => GtkInteractive (Draggable Cairo a) where
  display = displayDiagram diagram

instance ( V a ~ v, Clickable a, InnerSpace v )
      => Clickable (Draggable b a) where
  clickInside d p = clickInside (_dragContent d) $ p .-^ _dragOffsetAcc d

instance ( V a ~ v, Diagrammable b a, Enveloped a, HasLinearMap v, InnerSpace v)
      => Enveloped (Draggable b a) where
  getEnvelope d = translate (get dragOffset d) 
                . getEnvelope $ get dragContent d

-- | Creates dragging state for some object, with an initial offset.
mkDraggable :: V a -> a -> Draggable b a
mkDraggable = Draggable Nothing

-- | Pure mouse handler, compatible with the type expected by "simpleMouse".
mouseDrag (Just (True,  0)) p d | clickInside d (P $ pack p) = startDrag (pack p) d
mouseDrag Nothing           p d                              = updateDrag (pack p) d
mouseDrag (Just (False, 0)) p d                              = endDrag d
mouseDrag _ _ d = d

-- | Switches into dragging mode at the given position.
startDrag :: V a -> Draggable b a -> Draggable b a
startDrag p = set dragState (Just (p, p))

-- | Updates the drag with a new mouse position, if the object is being
--   dragged.  TODO: consider having a check for the input state to
--   check if the mouse is being held down?
updateDrag :: V a -> Draggable b a -> Draggable b a
updateDrag p (Draggable (Just (_, s)) o c) = Draggable (Just (p, s)) o c
updateDrag _ d = d

-- | Switches out of dragging mode.
endDrag :: (AdditiveGroup (V a)) 
        => Draggable b a -> Draggable b a
endDrag d = Draggable Nothing (get dragOffset d) $ _dragContent d

-- | Queries whether we're currently in dragging-mode.
isDragging :: Draggable b a -> Bool
isDragging = isJust . get dragState

-- | Lens on the current amount of drag-induced offset for the diagram.
dragOffset :: (AdditiveGroup (V a))
           => Draggable b a :-> V a
dragOffset = lens getter setter
 where
  delta = maybe zeroV (uncurry (^-^))
  getter   (Draggable c o _) = o ^+^ delta c
  setter o' (Draggable c o x) = Draggable c (o' ^-^ delta c) x
