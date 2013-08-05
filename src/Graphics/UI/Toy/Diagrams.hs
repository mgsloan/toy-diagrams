{-# LANGUAGE
    ConstraintKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  , TypeSynonymInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Diagrams
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Diagrams utilities
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Diagrams
  ( Clickable(..)
  , Diagrammable(..)
  -- * Miscellanious Utilities
  , blackLined, monoStyle
  , underlayScaled, overlayScaled
  , underlayWithExtents, overlayWithExtents
  ) where

import Data.Basis ( Basis )
import Data.Maybe ( fromMaybe )
import Diagrams.Prelude


-- | Clickable things have some concept of which positions are significant when
--   clicked.  Used for buttons, sliders, etc.
class Clickable a where
  clickInside :: a -> Point (V a) -> Bool

{- GHC bug?  ScopedTypeVariables don't seem to work
--   Defaults to running the 'Diagram's query function when 'Diagrammable'
--   (default method signatures).
  default clickInside :: forall b. (HasLinearMap (V a), Diagrammable b (V a) a)
                      => a -> Point (V a) -> Bool
  clickInside x = clickInside (diagram x :: Diagram b (V a))
-}

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d)

-- | Typeclass providing a default way to be displayed as a diagram.
class Diagrammable b v q a where
  diagram :: a -> QDiagram b v q

instance Diagrammable b v q (QDiagram b v q) where
  diagram = id

-- | Utility function to set a decent style for line drawings: black lines with
--   a two pixel stroke.
blackLined :: HasStyle a => a -> a
blackLined = lw 2 . lc black

-- | Style for 18 pt monospace text.
monoStyle :: Style R2
monoStyle = font "monospace" $ fontSize 18 mempty

-- * Diagrams Util

-- These functions do a "setEnvelope (getEnvelope d)" in order to avoid the
-- envelope computations unecessarily unioning the superimposed thing.
--
-- TODO: Move these into diagrams?

underlayScaled, overlayScaled
  :: ( HasLinearMap v, InnerSpace v, Ord (Basis v), OrderedField (Scalar v)
     , Semigroup m, Monoid m )
  => QDiagram b v m -> QDiagram b v m -> QDiagram b v m

underlayWithExtents, overlayWithExtents
  :: ( HasLinearMap v, InnerSpace v, Ord (Basis v), OrderedField (Scalar v)
     , Semigroup m, Monoid m )
  => QDiagram b v m -> (v -> QDiagram b v m) -> QDiagram b v m

-- | @'underlayScaled' d s@ scales 's' such that it the 'boundingBox' aligns
--   with the boundingbox of 'd', and yields 'd' composited 'atop' it.
underlayScaled d s
  = setEnvelope (getEnvelope d)
  $ d <> boxFit (boundingBox d) s

-- | @'overlayScaled' d s@ scales @s@ such that it the 'boundingBox' aligns
--   with the 'boundingBox' of @d@, and yields it composited 'atop' @d@.
overlayScaled d s
  = setEnvelope (getEnvelope d)
  $ boxFit (boundingBox d) s <> d

-- | @'underlayMatchExtents' d f@ supplies @f@ with the extents of the
--   'boundingBox' of @d@, and yields @d@ composited 'atop' the result.
underlayWithExtents d f
  = setEnvelope (getEnvelope d)
  $ d <> (alignLowestCorner (boundingBox d) . f . boxExtents $ boundingBox d)

-- | @'overlayMatchExtents' d f@ supplies @f@ with the extents of the
--   'boundingBox' of @d@, and yields the result composited 'atop' @d@.
overlayWithExtents d f
  = setEnvelope (getEnvelope d)
  $ (alignLowestCorner (boundingBox d) . f . boxExtents $ boundingBox d) <> d

-- | @'alignLowestCorner' a b@  Translates @b@ such that its lowest corner is
--   aligned to the lowest corner of the @a@.
alignLowestCorner
  :: (Enveloped a, Transformable a, Ord (Basis (V a)))
  => BoundingBox (V a) -> a -> a
alignLowestCorner b x = fromMaybe x $ do
  (l_b, _) <- getCorners b
  (l_x, _) <- getCorners $ boundingBox x
  return $ translate (l_b .-. l_x) x
