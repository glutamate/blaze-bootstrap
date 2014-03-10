{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Text.Blaze.Html.Page where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

import Text.Blaze.Html.Bootstrap
import Data.Monoid
import Control.Monad

data Page = Page { pageHeaders:: H.Html,
                   pageNavBar :: NavTree,
                   pageContents :: H.Html,
                   pageSidebar :: H.Html,
                   pageFooter :: H.Html,
                   pageScripts :: H.Html}

instance Monoid Page where
  mempty = Page mempty [] mempty mempty mempty mempty
  (Page h1 nB1 con1 side1 foot1 sc1) `mappend` (Page h2 nB2 con2 side2 foot2 sc2)
     = Page (h1<>h2)
            (nB1 <> nB2)
            (con1 <> con2)
            (side1 <> side2)
            (foot1 <> foot2)
            (sc1 <> sc2)

type PageLayout = Page -> H.Html

blogLayout :: PageLayout
blogLayout page@(Page {..}) = defaultWrapper page $ do
  when (not $ null pageNavBar) $
    staticNavBar pageNavBar
  H.div !. "container" $ do
    H.div !. "row" $ do
      H.div !. "col-sm-8" $ pageContents
      H.div !. "col-sm-3 col-sm-offset-1" $ pageSidebar
  H.div ! A.id "footer" $ do
    H.div !. "container" $ pageFooter

defaultWrapper :: Page -> H.Html -> H.Html
defaultWrapper page@(Page {..}) body = H.docTypeHtml $ do
  H.head $ pageHeaders
  H.body $ do body
              pageScripts

navBar :: NavTree -> Page
navBar nb = mempty { pageNavBar = nb }

scriptSrc :: T.Text -> Page
scriptSrc src = mempty { pageScripts = H.script ! A.src (toValue src) $ "" }