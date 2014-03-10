{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Blaze.Html.Bootstrap where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (form, label)

import Text.Blaze.Internal
import qualified Data.Text as T
import Data.Monoid


formRole :: Attribute
formRole = role "form"

formControl = A.class_ "form-control"


glyphicon :: T.Text -> H.Html
glyphicon s = H.span !. ("glyphicon glyphicon-"<>s)  $ ""

dataToggle :: T.Text -> Attribute
dataToggle k = H.customAttribute "data-toggle" $ preEscapedToValue $ k

dataTarget :: T.Text ->  Attribute
dataTarget k = H.customAttribute "data-target" $ preEscapedToValue $ k 

--role :: Attribute
role = attribute "role" " role=\""

infixl 9 !.
e !. c = e ! A.class_ (H.toValue (c::T.Text))

type NavTree = [NavItem]

data NavItem = Header H.Html
             | Divider
             | Link H.Html
             | SubTree H.Html NavTree

staticNavBar :: NavTree -> H.Html
staticNavBar items = H.div !. "navbar navbar-default navbar-static-top"
                           ! role "navigation" $ do
  H.div !. "container" $ do
    H.div !. "navbar-header" $ do
      H.button ! A.type_ "button" !. "navbar-toggle"
               ! dataToggle "collapse" ! dataTarget "navbar-collapse" $ do
        H.span !. "sr-only" $ "Toggle navigation"
        H.span !. "icon-bar" $ ""
        H.span !. "icon-bar" $ ""
        H.span !. "icon-bar" $ ""
      H.a !. "navbar-brand" ! A.href "#" $ "Project name"
    H.div !. "navbar-collapse collapse" $ do
      H.ul !. "nav navbar-nav" $ do
        mapM_ navBarItem items

navBarItem (Header h) = H.li h
navBarItem (Link h) = H.li h
navBarItem (Divider) = mempty
navBarItem (SubTree hdr items) = H.li !. "dropdown" $ do
  H.a ! A.href "#" !. "dropdown-toggle" ! dataToggle "dropdown" $ do
    hdr
    H.b !. "caret" $ ""
  H.ul !. "dropdown-menu" $ do
    mapM_ dropdownItem items

dropdownItem (Header h) = H.li !. "dropdown-header" $ h
dropdownItem (Divider) = H.li !. "divider" $ ""
dropdownItem (Link h) = H.li h
-- TODO http://stackoverflow.com/questions/18023493/bootstrap-3-dropdown-sub-menu-missing
dropdownItem (SubTree hdr items) = error "dropdown submenus not yet implemented"