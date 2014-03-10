module Text.Blaze.Html.Page where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)

import Data.Monoid

data Page = Page

instance Monoid Page where
  mempty = Page
  Page `mappend` Page = Page
