{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Text.Blaze.Html.Bootstrap where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (form, label)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label)

import Text.Blaze.Internal
import qualified Data.Text as T
import Data.Monoid

import Control.Monad

styleSheetLink src= H.link ! A.rel "stylesheet" ! A.href src ! A.type_ "text/css"
scriptLink src = H.script ! A.src src $ ""

formRole :: Attribute
formRole = role "form"

formControl = A.class_ "form-control"


glyphicon :: T.Text -> H.Html
glyphicon s = H.span !. ("glyphicon glyphicon-"<>s)  $ ""

dataToggle :: T.Text -> Attribute
dataToggle k = H.customAttribute "data-toggle" $ preEscapedToValue $ k

dataTarget :: T.Text ->  Attribute
dataTarget k = H.customAttribute "data-target" $ preEscapedToValue $ k

dataDismiss :: T.Text ->  Attribute
dataDismiss k = H.customAttribute "data-dismiss" $ preEscapedToValue $ k

--role :: Attribute
role = attribute "role" " role=\""

infixl 9 !.
e !. c = e ! A.class_ (H.toValue (c::T.Text))

infixl 9 !#
e !# c = e ! A.id (H.toValue (c::T.Text))

infixl 9 $:
e $: c = e (H.toHtml c)

ahref dest = H.a ! A.href (H.toValue dest)

whenJust Nothing _ = return ()
whenJust (Just x) f = f x

type NavTree = [NavItem]

data NavItem = Header H.Html
             | Divider
             | Link H.Html
             | ActiveLink H.Html
             | SubTree H.Html NavTree

-- example brand : H.a !. "navbar-brand" ! A.href "#" $ "Project name"

data NavBar = NavBar { navBarBrand :: H.Html,
                       navBarLeft :: NavTree,
                       navBarRight :: NavTree }

instance Monoid NavBar where
  mempty = NavBar mempty [] []
  (NavBar b1 l1 r1) `mappend` (NavBar b2 l2 r2)
    =  NavBar (b1<>b2) (l1++l2) (r1++r2)

instance ToMarkup NavBar where
  toMarkup NavBar{..} = H.div !. "navbar navbar-default"
                                ! role "navigation" $ do
  H.div !. "container" $ do
    H.div !. "navbar-header" $ do
      H.button ! A.type_ "button" !. "navbar-toggle"
               ! dataToggle "collapse" ! dataTarget "navbar-collapse" $ do
        H.span !. "sr-only" $ "Toggle navigation"
        H.span !. "icon-bar" $ ""
        H.span !. "icon-bar" $ ""
        H.span !. "icon-bar" $ ""
      navBarBrand
    H.div !. "navbar-collapse collapse" $ do
      H.ul !. "nav navbar-nav" $ do
        mapM_ navBarItem navBarLeft
      H.ul !. "nav navbar-nav navbar-right" $ do
        mapM_ navBarItem navBarRight

staticNavBar :: NavBar -> H.Html
staticNavBar nb = (H.toHtml nb) !. "navbar-static-top"

fixedNavBar ::  NavBar -> H.Html
fixedNavBar nb = (H.toHtml nb) !. "navbar-fixed-top"


navBarItem (Header h) = H.li h
navBarItem (Link h) = H.li h
navBarItem (ActiveLink h) = H.li !. "active" $  h
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
dropdownItem (ActiveLink h) = H.li !. "active" $ h
-- TODO http://stackoverflow.com/questions/18023493/bootstrap-3-dropdown-sub-menu-missing
dropdownItem (SubTree hdr items) = error "dropdown submenus not yet implemented"


data LoginRegister = LoginRegister
  { loginFormTitle :: Maybe T.Text,
    nameLabel :: T.Text,
    loginAction :: T.Text,
    registerAction:: T.Text,
    registerQuestions :: [(T.Text, T.Text)]
  }

--generated by blaze-from-html from http://bootsnipp.com/snippets/featured/loginregister-in-tabbed-interface
login_register_form :: LoginRegister -> Html
login_register_form LoginRegister{..} = do
    H.div ! class_ "container" $
     H.div ! class_ "row" $ H.div ! class_ "span12" $ H.div ! class_ "" ! A.id "loginModal" $ do
        case loginFormTitle of
           Nothing -> ""
           Just title ->
              H.div ! class_ "modal-header" $ do
                  button ! type_ "button" ! class_ "close" ! dataAttribute "dismiss" "modal" $ mempty
                  h3 $ toHtml title
        H.div ! class_ "modal-body" $ H.div ! class_ "well" $ do
            ul ! class_ "nav nav-tabs" $ do
                li ! class_ "active" $ a ! href "#login" ! dataAttribute "toggle" "tab" $ "Login"
                li $ a ! href "#create" ! dataAttribute "toggle" "tab" $ "Create Account"
            H.div ! A.id "myTabContent" ! class_ "tab-content" $ do
                H.div ! class_ "tab-pane active" ! A.id "login" $ do
                  H.form ! class_ "form-horizontal" ! action (toValue loginAction) ! method "POST" $ fieldset $ do
                    H.div ! A.id "legend" $ legend ! class_ "" $ "Login"
                    H.div ! class_ "control-group" $ do
                        --  Username
                        H.label ! class_ "control-label" ! for (toName nameLabel) $ (toHtml nameLabel)
                        H.div ! class_ "controls" $ input ! type_ "text" ! A.id (toValue nameLabel) ! name (toName nameLabel) ! placeholder "" ! class_ "input-xlarge"
                    H.div ! class_ "control-group" $ do
                        --  Password
                        H.label ! class_ "control-label" ! for "password" $ "Password"
                        H.div ! class_ "controls" $ input ! type_ "password" ! A.id "password" ! name "password" ! placeholder "" ! class_ "input-xlarge"
                    H.div ! class_ "control-group" $ do
                        --  Button
                        H.div ! class_ "controls" $ button ! class_ "btn btn-success" $ "Login"
                H.div ! class_ "tab-pane" ! A.id "create" $ do
                  H.form ! class_ "form-horizontal" ! action (toValue registerAction) ! method "POST" $ fieldset $ do
                    H.div ! A.id "legend" $ legend ! class_ "" $ "Register"
                    H.div ! class_ "control-group" $ do
                      H.label ! class_ "control-label" $ (toHtml nameLabel)
                      H.div ! class_ "controls" $ input ! type_ "text" ! value "" ! name (toName nameLabel) ! class_ "input-xlarge"
                    H.div ! class_ "control-group" $ do
                        --  Password
                        H.label ! class_ "control-label" ! for "password" $ "Password"
                        H.div ! class_ "controls" $ input ! type_ "password" ! A.id "password" ! name "password" ! placeholder "" ! class_ "input-xlarge"
                    forM_ registerQuestions $ \(lbl, nm) -> do
                      H.div ! class_ "control-group" $ do
                        H.label ! class_ "control-label" $ (toHtml lbl)
                        H.div ! class_ "controls" $ input ! type_ "text" ! value "" ! name (toValue nm) ! class_ "input-xlarge"
                    H.div $ button ! class_ "btn btn-primary" $ "Create Account"


toName txt = toValue $ T.toLower $ T.filter (/=' ') txt


modal elemid mtitle mbody mfooter = H.div !. "modal fade" ! A.id elemid ! A.tabindex "-1" ! role "dialog" $ do
  H.div !. "modal-dialog" $ do
    H.div !. "modal-content" $ do
      H.div !. "modal-header" $ do
        H.button ! A.type_ "button" !. "close" ! dataDismiss "modal" $ do
             H.span (preEscapedToHtml ("&times;"::T.Text))
             H.span !. "sr-only" $ "Close"
        H.h4 !. "modal-title" ! A.id "myModalLabel" $ mtitle

      H.div !. "modal-body" $  mbody
      H.div !. "modal-footer" $ mfooter

progressBar = H.div !. "progress" $ do
  H.div !. "progress-bar" ! role "progressbar" ! A.style "width: 0%;" $ ""

postButton :: T.Text -> T.Text -> H.Html -> H.Html
postButton url btnClass label =  H.form ! A.action (H.toValue $ url) ! A.method "post" $ do
    H.button !. ("btn " <> btnClass) ! A.type_ "submit" $ label

row x = H.div ! class_ "row" $ x
