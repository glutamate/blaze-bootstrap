{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.Radian where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)
import Text.Blaze.Internal
import Data.String

plot :: H.Html -> H.Html
plot = Parent "plot" "<plot" "</plot>"

plotData :: H.Html -> H.Html
plotData = Parent "plot-data" "<plot-data" "</plot-data>"

plotOptions :: H.Html -> H.Html
plotOptions = Parent "plot-options" "<plot-options" "</plot-options>"

metadata :: H.Html 
metadata = Leaf "metadata" "<metadata" ">"

lines :: H.Html 
lines = Leaf "lines" "<lines" ">"

points :: H.Html 
points = Leaf "points" "<points" ">"

[x,y] = map genAttribute $ words attrs where
  genAttribute s = attribute (fromString s) (fromString (s++"=\""))
  attrs = "x y"

--x = attribute "x" " x=\""
