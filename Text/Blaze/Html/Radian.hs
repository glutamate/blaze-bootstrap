{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.Radian where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)
import Text.Blaze.Internal
import qualified Data.Text as T
import Data.String (fromString)
import Data.Monoid
import Prelude hiding (lines)

plot :: H.Html -> H.Html
plot = Parent "plot" "<plot" "</plot>"

plotData :: H.Html -> H.Html
plotData = Parent "plot-data" "<plot-data" "</plot-data>"

plotOptions :: H.Html -> H.Html
plotOptions = Parent "plot-options" "<plot-options" "</plot-options>"

metadata :: H.Html 
metadata = Parent "metadata" "<metadata" "</metadata>" $ ""

lines :: H.Html 
lines = Parent "lines" "<lines" "</lines>" $ ""

bars :: H.Html 
bars = Parent "bars" "<bars" "</bars>" $ ""

points :: H.Html 
points = Parent "points" "<points" "</points>" $ ""



[x,y, cols, format, aspect, stroke, strokeWidth] = map genAttribute $ words attrs where
  genAttribute s = attribute (fromString s) (fromString (' ':s++"=\""))
  attrs = "x y cols format aspect stroke stroke-width"

--x = attribute "x" " x=\""

plotDataCSV :: T.Text -> [T.Text] -> [[T.Text]] -> H.Html
plotDataCSV nm hdrs rows
    = plotData ! H.name (H.toValue nm) ! cols (H.toValue $ T.intercalate "," hdrs) ! format "csv"
               $ preEscapedText $ "\n" <> (T.unlines $ map (T.intercalate ",") rows)

histogram :: T.Text -> [Double] -> H.Html
histogram nm xs = do
  plotDataCSV nm ["val"] $ map ((:[]) . T.pack . show) xs
  plot ! H.name (toValue $ "plot"<>nm) $ do
     bars ! H.customAttribute "hist" (toValue $ "[[histogram("<> nm<>".val, 50)]]") 
          ! x "[[hist.centres]]" 
          ! y "[[hist.probs]]"

exampleSines :: H.Html
exampleSines = plot ! H.height "200" 
                    ! aspect "2" 
                    ! strokeWidth "2"
                    ! x "[[seq(0,4*PI,101)]]" $ do
  lines ! y "[[sin(x)]]" ! stroke "red"
  lines ! y "[[cos(x)]]" ! stroke "blue"

--       axis-x-label="Time"
--      axis-y-label="sin(x) / cos(x)">

