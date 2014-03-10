{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.AngularJS where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)
import Text.Blaze.Internal

import qualified Data.Text as T
import Data.Monoid

ngModel :: AttributeValue -> Attribute
ngModel = attribute "ng-model" " ng-model=\"" 

ngClick :: AttributeValue -> Attribute
ngClick = attribute "ng-click" " ng-click=\"" 

ngInit :: T.Text -> T.Text -> Attribute
ngInit k v = H.customAttribute "ng-init" $ preEscapedToValue $ k <> "=" <> v

ngDisabled :: T.Text -> Attribute
ngDisabled dis = H.customAttribute "ng-disabled" $ toValue dis

ngShow :: T.Text -> Attribute
ngShow dis = H.customAttribute "ng-show" $ toValue dis

ngHide :: T.Text -> Attribute
ngHide dis = H.customAttribute "ng-hide" $ toValue dis
