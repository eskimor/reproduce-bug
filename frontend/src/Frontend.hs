{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Prelude
import Reflex
import Reflex.Dom
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import GHCJS.DOM.EventM (mouseOffsetXY, EventM, event, mouseClientXY, uiPageXY, mouseButton, eventTarget)
import GHCJS.DOM.Types (IsUIEvent, IsElement, MonadJSM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)

frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = void $ prerender (text "Loading") app
  }

app :: (MonadWidget t m, MonadJSM m)  => m ()
app = mdo
  (elm, out) <- el' "div" $ do
    dynText relEvD
    flip mapM_ ([0..1000] :: [Int]) $ \_ -> do
      cb <- checkbox False def
      let vv = value cb
      let cv = current vv
      {- dynText ((\b a -> if b then a else "-") <$> vv <*> relEvD) -}
      let ev2 = gate cv relEv
      evD <- holdDyn "-" ev2
      dynText evD
  relEv <- fmap (pack . show) <$> offsetMouseEvent elm Mousemove
  relEvD <- holdDyn "foo" relEv
  pure ()






offsetMouseEvent
    :: (TriggerEvent tx m, IsUIEvent (EventType event), IsElement (RawElement el), MonadIO m, MonadJSM m)
    => Element er el t -> EventName event -> m (Event tx (Int, Int))
offsetMouseEvent elm ev = wrapDomEvent (_element_raw elm) (elementOnEventName ev) uiPageXY
