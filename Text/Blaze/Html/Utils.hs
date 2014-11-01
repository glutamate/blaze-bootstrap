{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Text.Blaze.Html.Utils where


import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A hiding (form, label)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form, label)

import Text.Blaze.Internal
import qualified Data.Text as T
import Data.Monoid

import Control.Monad

infixl 9 !.
e !. c = e ! A.class_ (H.toValue (c::T.Text))

infixl 9 !#
e !# c = e ! A.id (H.toValue (c::T.Text))

infixr 0 $:
e $: c = e (H.toHtml c)

ahref dest = H.a ! A.href (H.toValue dest)

whenJust Nothing _ = return ()
whenJust (Just x) f = f x

--role :: Attribute
role = attribute "role" " role=\""

styleSheetLink src= H.link ! A.rel "stylesheet" ! A.href src ! A.type_ "text/css"
scriptLink src = H.script ! A.src src $ ""

formRole :: Attribute
formRole = role "form"

tableHeadRow :: [Html] -> Html
tableHeadRow = thead . tr . mapM_ th 
