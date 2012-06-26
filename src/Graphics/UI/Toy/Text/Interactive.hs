{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Toy.Text.Interactive where

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Text

import Control.Arrow (first, second)

import Data.Colour.Names (black, yellow)
import Data.Maybe (maybeToList)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.Text (textLineBounded)

import System.IO.Unsafe

cursorText :: (Mark m, CanBeCursor m) => MarkedText m
cursorText = MarkedText "" [((0, 0), mkCursor)]

instance (Eq m, Mark m, CanBeCursor m)
      => Interactive ib (MarkedText m) where
  keyboard = simpleKeyboard textKeyHandler

instance (Eq m, Mark m, CanBeCursor m)
      => GtkDisplay (MarkedText m) where
  display = displayDiagram
          $ \mt -> scaleY (-1)
                 $ strutY 18
                   ===
                  ( strutX 10 ||| alignT (diagram mt) )

textKeyHandler :: (Eq m, Mark m, CanBeCursor m)
               => KeyEvent -> MarkedText m -> MarkedText m
textKeyHandler (True, e) mt = case e of
  Right k -> insert [k]
  Left  k -> case k of
    "Return"    -> insert "\n"
    "Left"      -> mutateCursors (subtract 1)
    "Right"     -> mutateCursors (+1)
    "Home"      -> mutateCursors (const (-maxIx, -maxIx))
    "End"       -> mutateCursors (const (maxIx, maxIx))
    "Delete"    -> editCursors (\(ivl, _) -> (second  (+1)  ivl, cursorText))
    "BackSpace" -> editCursors (\(ivl, _) -> (first (+(-1)) ivl, cursorText))
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
    _           -> mt
 where
  editCursors f = edit (whenMarked isCursor f) mt

  insert s = editCursors $ second $ const (MarkedText s [((p, p), mkCursor)])
    where p = length s

  mutateCursors f = mutateMarks
                    ( \(i, m) -> if isCursor m then Just (f i, m) else Just (i, m) )
                    mt

  toMaybe f x = if f x then Just x else Nothing

  maxIx = textLength mt

textKeyHandler _ ts = ts