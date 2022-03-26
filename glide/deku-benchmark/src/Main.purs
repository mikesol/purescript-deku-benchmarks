module Main where

import Prelude

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Deku.Change (change)
import Deku.Control.Functions ((%>), (@>))
import Deku.Graph.Attribute (cb)
import Deku.Graph.DOM (AsSubgraph(..), SubgraphSig, root, subgraph, xsubgraph, (:=))
import Deku.Graph.DOM as D
import Deku.Graph.DOM.Shorthand as S
import Deku.Interpret (makeFFIDOMSnapshot)
import Deku.Run (TriggeredScene(..), defaultOptions, run)
import Effect (Effect)
import Effect.Ref (new, read, write)
import FRP.Behavior (sample_)
import FRP.Behavior.Time (seconds)
import FRP.Event (Event, makeEvent, subscribe)
import Math (pi, sin)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.HTMLInputElement (checked, fromEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)

endN = 1_000 :: Int

sgMap :: Map Int (Maybe Boolean)
sgMap = fromFoldable $ map (\a -> a /\ Just false) (0 .. (endN - 1))

animationFrameEvent :: Event Unit
animationFrameEvent = makeEvent \k -> do
  w <- window
  running <- new true
  let
    ff = void $ flip requestAnimationFrame w do
      r' <- read running
      when r' do
        k unit
        ff
  ff
  pure $ write false running

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \elt -> do
    ffi <- makeFFIDOMSnapshot
    void $ subscribe
      ( run (sample_ (seconds) animationFrameEvent) (pure unit) defaultOptions ffi
          $
            ( \(TriggeredScene { trigger: Seconds time }) push ->
                root elt
                  { n: D.text "0"
                  , d: D.div [] { sg: subgraph sgMap (AsSubgraph (sg push)) }
                  } /\
                  { rate: 4.0
                  , prevTime: time
                  , prevInternalTime: 0.0
                  , ixLow: 0
                  , ixHigh: 0
                  , nClicked: 0
                  }
            ) @> \env state -> case env of
              Left (TriggeredScene { trigger: Seconds time }) -> do
                let
                  elapsedTime = time - state.prevTime
                  internalTime = state.prevInternalTime + (elapsedTime * state.rate)
                  lowerBound = max 0 $ Int.floor internalTime
                  diffUp = Int.floor (min 5.0 (sin (internalTime * 0.5 * pi) + 1.0) * 40.0)
                  upperBound = lowerBound + diffUp
                  toTurnOff = (state.ixLow .. (lowerBound - 1)) <>
                    (guard (upperBound < state.ixHigh) (upperBound .. state.ixHigh))
                  toTurnOn = if upperBound > state.ixHigh then (max lowerBound state.ixHigh .. upperBound) else []
                change
                  { "root.d.sg": xsubgraph $ fromFoldable
                      ( (map (\i -> i /\ Just false) toTurnOff)
                          <> (map (\i -> i /\ Just true) toTurnOn)
                      )
                  } $> state
                  { ixLow = lowerBound
                  , ixHigh = upperBound
                  , prevTime = time
                  , prevInternalTime = internalTime
                  }
              Right tf ->
                let
                  np1 = (if tf then add else sub) state.nClicked 1
                in
                  change { "root.n": show np1 } $>
                    state { nClicked = np1 }
      )
      (_.res >>> pure)

sg :: (Boolean -> Effect Unit) -> SubgraphSig Int Boolean Int
sg raise _ =
  ( \_ _ ->
      S.input
        [ D.Xtype := "checkbox"
        , D.OnClick := cb \e -> for_ (target e >>= fromEventTarget) (checked >=> raise)
        ]
        {} /\ unit
  ) %> \env _ ->
    case env of
      Left ckd -> change
        { input:
            D.input'attr
              [ D.Checked := show ckd ]
              <>
                if ckd then [ D.OnClick := cb \_ -> pure unit ]
                else [ D.OnClick := cb \e -> for_ (target e >>= fromEventTarget) (checked >=> raise) ]

        }
      Right _ -> pure unit