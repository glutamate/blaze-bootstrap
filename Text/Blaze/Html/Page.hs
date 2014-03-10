{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Text.Blaze.Html.Page where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)

import Text.Blaze.Html.Bootstrap
import Data.Monoid
import Control.Monad

data Page = Page { pageHeaders:: [H.Html],
                   pageNavBar :: NavTree }

instance Monoid Page where
  mempty = Page [] []
  (Page h1 nB1) `mappend` (Page h2 nB2)
     = Page (h1++h2)
            (nB1 ++ nB2)

type PageLayout = Page -> H.Html

blogLayout :: PageLayout
blogLayout (Page {..}) = H.docTypeHtml $ do
  H.head $ mconcat pageHeaders
  H.body $ do when (not $ null pageNavBar) $
                staticNavBar pageNavBar

