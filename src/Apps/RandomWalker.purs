module Apps.RandomWalker (app) where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Data.String (drop)


app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec "")
        { render = \state -> log state
        , updateInterval = 33
        , handleKeyboard = \e s -> Just $ e.keyCode <> s
        -- , handleMouse = \e s -> Debug.unsafeLog e.location # \_ -> Debug.unsafeLog e # \_ -> Nothing
        , tick = \_ s ->
            case s of
                "" -> Nothing
                _ -> Just $ drop 1 s
        }
