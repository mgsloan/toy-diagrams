{-# LANGUAGE
    ConstraintKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Draggable
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  mgsloan@gmail.com
--
-- Utilities for making things that can be clicked and dragged around.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Draggable
  ( Draggable(..)

  -- * Lenses
  , dragState, dragStart, dragOffset, dragContent

  -- * Query
  , isDragging

  -- * Mutation
  , mouseDrag
  , startDrag, updateDrag, endDrag

  -- * Construction
  , mkDraggable, mkHandle
  ) where

import Control.Lens
import Data.AffineSpace.Point ( Point(..) )
import Data.Data              ( Data, Typeable1 )
import Data.Maybe             ( isJust )
import Data.Semigroup         ( Semigroup )

import Diagrams.Prelude
  ( V, Scalar, R2, InnerSpace, HasLinearMap, OrderedField, AdditiveGroup(..)
  , Enveloped(..), Transformable, HasStyle, TrailLike
  , (.-^), (^-^), translate, circle
  )
import Diagrams.Lens

import Graphics.UI.Toy          ( Interactive(..), MousePos, simpleMouse )
import Graphics.UI.Toy.Diagrams ( Clickable(..), Diagrammable(..), blackLined )


-- | Draggable things are translatable, and store state related to dragging.
--   If the '_dragState' has a value, then it indicates that it is currently
--   being dragged, and stores (current mouse pos, initial mouse pos).
--   Subtracting these and adding two '_dragState' gives 'dragOffset', the
--   amount that the '_dragContent' should be offset when drawing.
data Draggable a = Draggable
  { _dragState :: Maybe (V a, V a)
  , _dragStart :: V a
  , _dragContent :: a
  }

deriving instance (Read a, Read (V a)) => Read (Draggable a)
deriving instance (Show a, Show (V a)) => Show (Draggable a)
deriving instance (Data a, Data (V a)) => Data (Draggable a)
deriving instance Typeable1 Draggable

type instance V (Draggable a) = V a

$(makeLenses ''Draggable)

instance ( v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable b v q a, Semigroup q )
        => Diagrammable b v q (Draggable a) where
  diagram d@(Draggable _ _ a)
    = translate (d ^. dragOffset) $ diagram a

instance ( Clickable a, Wrapped' (V a) (MousePos ib), InnerSpace (V a) )
      => Interactive ib (Draggable a) where
  mouse = simpleMouse mouseDrag

instance ( V a ~ v, Clickable a, InnerSpace v )
      => Clickable (Draggable a) where
  clickInside d p = clickInside (d ^. dragContent) $ p .-^ (d ^. dragOffset)

instance ( V a ~ v, Enveloped a, HasLinearMap v, InnerSpace v )
      => Enveloped (Draggable a) where
  getEnvelope d = translate (d ^. dragOffset)
                $ getEnvelope (d ^. dragContent)

-- | Creates dragging state for some object, with an initial offset.
mkDraggable :: V a -> a -> Draggable a
mkDraggable = Draggable Nothing

-- | Creates a draggable circle of the given radius.
mkHandle :: (TrailLike a, Transformable a, HasStyle a, V a ~ R2)
         => Double -> Draggable a
mkHandle = mkDraggable zeroV . blackLined . circle

-- | Pure mouse handler, compatible with the type expected by "simpleMouse".
--   Only triggers for left mouse clicks (mouse 1).
mouseDrag :: (Eq t, Num t, Wrapped' (V a) o, InnerSpace (V a), Clickable a)
          => Maybe (Bool, t) -> o -> Draggable a -> Draggable a
mouseDrag m v d = case m of
  (Just (True,  0))
    | clickInside d (P p) -> startDrag p d
  Nothing                 -> updateDrag p d
  (Just (False, 0))       -> endDrag d
  _                       -> d
 where
  p = v ^. unwrapped'

-- | Switches into dragging mode at the given position.
startDrag :: AdditiveGroup (V a) => V a -> Draggable a -> Draggable a
startDrag p = dragState .~ Just (p, p)

-- | Updates the drag with a new mouse position, if the object is being dragged.
updateDrag :: V a -> Draggable a -> Draggable a
updateDrag p (Draggable (Just (_, s)) o c) = Draggable (Just (p, s)) o c
updateDrag _ d = d

-- | Switches out of dragging mode.
endDrag :: AdditiveGroup (V a)
        => Draggable a -> Draggable a
endDrag d = Draggable Nothing (d ^. dragOffset) (d ^. dragContent)

-- | Queries whether the 'Draggable' is currently being dragged.
isDragging :: Draggable a -> Bool
isDragging = isJust . view dragState

-- | Lens on the current amount of offset for the diagram.
dragOffset :: forall a. AdditiveGroup (V a)
           => Lens' (Draggable a) (V a)
dragOffset = lens getter setter
 where
  delta = maybe zeroV (uncurry (^-^))
  getter :: Draggable a -> V a
  getter    (Draggable c a _) = a ^+^ delta c
  setter :: Draggable a -> V a -> Draggable a
  setter (Draggable c _ x) a' = Draggable c (a' ^-^ delta c) x
