{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.Bootstrap where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)

import Text.Blaze.Internal


formRole :: Attribute
formRole = attribute "role" " role=\"" "form"

formControl = H.class_ "form-control"
