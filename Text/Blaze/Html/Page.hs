{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Text.Blaze.Html.Page where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

import Text.Blaze.Html.Bootstrap
import Data.Monoid
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer.Lazy

data Page = Page { pageHeaders:: H.Html,
                   pageNavBar :: NavBar,
                   pageContents :: H.Html,
                   pageSidebar :: H.Html,
                   pageFooter :: H.Html,
                   pageScripts :: H.Html}

instance Monoid Page where
  mempty = Page mempty mempty mempty mempty mempty mempty
  (Page h1 nB1 con1 side1 foot1 sc1) `mappend` (Page h2 nB2 con2 side2 foot2 sc2)
     = Page (h1<>h2)
            (nB1 <> nB2)
            (con1 <> con2)
            (side1 <> side2)
            (foot1 <> foot2)
            (sc1 <> sc2)

type PageLayout = Page -> H.Html

blogLayout :: PageLayout
blogLayout Page{..} = defaultWrapper pageHeaders pageScripts $ do  
  staticNavBar pageNavBar
  H.div !. "container" $ do
    H.div !. "row" $ do
      H.div !. "col-sm-8" $ pageContents
      H.div !. "col-sm-3 col-sm-offset-1" $ pageSidebar
  H.div ! A.id "footer" $ do
    H.div !. "container" $ pageFooter

defaultWrapper :: H.Html -> H.Html -> H.Html -> H.Html
defaultWrapper pageHeaders pageScripts body = H.docTypeHtml $ do
  H.head $ pageHeaders
  H.body $ do body
              pageScripts

navBar :: Monad m => NavBar -> WriterT Page m ()
navBar nb = tell $ mempty { pageNavBar = nb }

scriptSrc :: Monad m =>  T.Text -> WriterT Page m ()
scriptSrc src = tell $ mempty { pageScripts = H.script ! A.src (toValue src) $ "" }

scriptLit :: Monad m =>  T.Text -> WriterT Page m ()
scriptLit code = tell $ mempty { pageScripts = H.script $ H.preEscapedToHtml code }

cssLink :: Monad m =>  T.Text -> WriterT Page m ()
cssLink src = tell $ mempty { pageHeaders = H.link ! A.rel "stylesheet" ! A.href (toValue src) ! A.media "screen" }

header :: Monad m =>  H.Html -> WriterT Page m ()
header html = tell $ mempty { pageHeaders = html }

contents :: Monad m =>  H.Html -> WriterT Page m ()
contents html = tell $ mempty { pageContents = html }

sidebar :: Monad m =>  H.Html -> WriterT Page m ()
sidebar html = tell $ mempty { pageSidebar = html }

footer :: Monad m =>  H.Html -> WriterT Page m ()
footer html = tell $ mempty { pageFooter = html }

pageWriterT :: Monad m => WriterT Page m () -> m Page
pageWriterT = execWriterT

pageWriter :: WriterT Page Identity () -> Page
pageWriter = runIdentity . execWriterT
