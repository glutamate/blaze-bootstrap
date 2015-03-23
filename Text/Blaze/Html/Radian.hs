{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.Radian where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)
import Text.Blaze.Internal
import Text.Blaze.Html.Bootstrap
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Monoid
import Prelude hiding (lines)
import qualified Data.Array.Unboxed as Array
import Data.List (foldl1', intercalate)
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import OpenBrain.CSV
import Debug.Trace

import Text.Blaze.Html.Utils


plot :: H.Html -> H.Html
plot = Parent "plot" "<plot" "</plot>"

plotData :: H.Html -> H.Html
plotData = Parent "plot-data" "<plot-data" "</plot-data>"

plotRow :: H.Html -> H.Html
plotRow = Parent "plot-row" "<plot-row" "</plot-row>"

plotCol :: H.Html -> H.Html
plotCol = Parent "plot-col" "<plot-col" "</plot-col>"

plotOptions :: H.Html -> H.Html
plotOptions = Parent "plot-options" "<plot-options" "</plot-options>"

metadata :: H.Html
metadata = Parent "metadata" "<metadata" "</metadata>" $ ""

dateParseFormat :: T.Text -> Attribute
dateParseFormat k = H.customAttribute "date-parse-format" $ preEscapedToValue $ k


lines, lines_ :: H.Html
lines = Parent "lines" "<lines" "</lines>" $ ""

lines_ = lines

bars :: H.Html
bars = Parent "bars" "<bars" "</bars>" $ ""

points :: H.Html
points = Parent "points" "<points" "</points>" $ ""



[x,y, cols, format, aspect, stroke, strokeWidth, axisXlabel, axisYlabel, marker, fill, markerSize, rangeX, xTicks, plotTitle, fillOpacity] = map genAttribute $ words attrs where
  genAttribute s = attribute (fromString s) (fromString (' ':s++"=\""))
  attrs = "x y cols format aspect stroke stroke-width axis-x-label axis-y-label marker fill marker-size range-x axis-x-ticks title fill-opacity"

--x = attribute "x" " x=\""

radExpr :: T.Text -> H.AttributeValue
radExpr t = toValue $ "[[" <> t <> "]]"

plotDataCSV :: T.Text -> [T.Text] -> H.Html -> [[T.Text]] -> H.Html
plotDataCSV nm hdrs meta rows
    = plotData ! H.id (H.toValue nm) ! H.name (H.toValue nm) ! cols (H.toValue $ T.intercalate "," hdrs) ! format "csv" $ do
        meta
        preEscapedText $ "\n" <> (T.unlines $ map (T.intercalate ",") rows)

cassavaPlotData :: CSV.ToNamedRecord a => T.Text -> H.Html -> [a] -> H.Html
cassavaPlotData _ _ [] = return ()
cassavaPlotData nm meta xs = do
  let hdrs = allHeaders $ head xs
      hdrsL = map (T.unpack . DTE.decodeUtf8) $ V.toList hdrs
  let csv = toStrict $ CSV.encodeByName hdrs xs
  plotData ! H.id (H.toValue nm) ! H.name (H.toValue nm) ! cols (H.toValue $ intercalate "," hdrsL) ! format "csv" $ do
    meta
    trace (show csv) $ unsafeByteString $ "\n" <> csv



radianScripts :: H.Html
radianScripts = do
  scriptLink "http://openbrainsrc.github.io/Radian/js/jquery.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/jquery.csv.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/escodegen.browser.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/d3.v2.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/angular.min.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/radian.min.js"
  scriptLink "http://openbrainsrc.github.io/Radian/js/bootstrap.min.js"

radianHeaders :: H.Html
radianHeaders = do
   H.meta ! H.charset "utf-8"
   styleSheetLink "http://openbrainsrc.github.io/Radian/css/radian.css"

radianDummyModule :: T.Text -> H.Html
radianDummyModule nm =
  H.script $ preEscapedText $ "angular.module('"<>nm<>"', ['radian']);"

------------------
-- ScatterPlot  --
------------------

scatterPlot, linePlot :: Int -> Int -> T.Text -> [(Double,Double)] -> H.Html
scatterPlot h w nm xys = do
  let tshow = T.pack . show
  plotDataCSV nm ["x", "y"] "" $ map (\(x,y)-> [tshow x, tshow y]) xys
  plot  ! H.height (toValue h) ! H.width (toValue w) $ do
    points ! x (radExpr $ nm<>".x")
           ! y (radExpr $ nm<>".y")

linePlot h w nm xys = do
  let tshow = T.pack . show
  plotDataCSV nm ["x", "y"] "" $ map (\(x,y)-> [tshow x, tshow y]) xys
  plot  ! H.height (toValue h) ! H.width (toValue w) $ do
    lines  ! x (radExpr $ nm<>".x")
           ! y (radExpr $ nm<>".y")



------------------
-- Histograming --
------------------


dynHistogram :: T.Text -> [Double] -> H.Html
dynHistogram nm xs = do
  plotDataCSV nm ["val"] "" $ map ((:[]) . T.pack . show) xs
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

staticHistogram :: T.Text -> Int -> ((H.Html->H.Html) ->(H.Html->H.Html)) -> [Double] -> H.Html
staticHistogram nm nbins modPlot xs = do
  let hist = histValues nbins xs
      tshow = T.pack . show
  plotDataCSV nm ["val", "histcount"] "" $ map (\(x,y) -> [tshow x, tshow y]) hist
  (modPlot plot) ! H.name (toValue $ "plot"<>nm) $ do
     bars ! x (radExpr $ nm<>".val")
          ! y (radExpr $ nm<>".histcount")

histArr :: (Int, Int) -> [Int] -> Array.UArray Int Double
histArr bnds is = Array.accumArray (+) 0 bnds [( i, 1) | i<-is, Array.inRange bnds i]

histValues :: Int -> [Double] -> [(Double,Double)]
histValues nbins vls =
    let (hArr, lo, hi, binSize) = histList nbins vls
    in zip [lo, lo+binSize..hi] hArr

histList :: Int -> [Double] -> ([Double] , Double, Double, Double)
histList _ [] = ([], 0, 0, 1)
histList nbins vls = let lo = foldl1' min vls
                         hi = foldl1' max vls
                         num = realToFrac $ length vls
                         binSize = (hi-lo)/(realToFrac nbins+1)
                         ixs = map (\v-> floor $! (v-lo)/binSize ) vls
                         hArr = histArr (0,nbins-1) $ ixs
                         empiricalIntegral = sum $ map (*binSize) $ Array.elems hArr
                     in ((/empiricalIntegral) `fmap` Array.elems hArr, lo, hi, binSize)
