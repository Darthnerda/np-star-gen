module Main where

import Control.Monad.State
import Prelude

import Control.Alternative (guard)
import Control.Monad.Cont (callCC)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.List.Trans (head, scanl, repeat)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask, asks)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, and, elem, find, or)
import Data.Traversable (traverse)
import Data.Identity (Identity)
import Data.Int (round, toNumber, trunc)
import Data.List.Lazy (List(..), Step(..), catMaybes, find, index, iterate, length, nil, snoc, uncons, (:), replicateM, cycle)
import Data.List.Lazy (singleton) as List
import Data.Array (range)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.NonEmpty (NonEmpty, singleton, (:|))
import Data.Number (cos, e, exp, isNaN, log, pow, sin, sqrt, pi)
import Data.Number (round, fromString) as Number
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Effect.Console (log) as Console
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON

import Record (union, set)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event
import DOM.HTML.Indexed.StepValue
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol)



---
--- Types
---

type Position = {
  x :: Number,
  y :: Number
}

type Tick = Number

type Radian = Number

type Circle = {
  center :: Position,
  radius :: Number,
  rotationsPerTick :: Number
}

type Star = {
  uid :: Int,
  puid :: Maybe Int,
  name :: String,
  e :: Int,
  i :: Int,
  s :: Int,
  st :: Int,
  g :: Int,
  r :: Int,
  x :: Number,
  y :: Number
}

-- Wormholes are a two element array of the uids of the connected stars
type Wormhole = Array Int

type Galaxy = {
  stars :: Array Star,
  wormholes :: Array Wormhole
}







--- 
--- RandProvider
---

-- RandProvider is a pure wrapper for an (assumed) infinite list of uniformly random numbers
-- It has two functions
--      consume - consume provides the next uniform
--      choice - choice picks randomly out of a list of choices

type RandProviderT m = StateT (List Number) m
type RandProvider = RandProviderT Identity

consume :: RandProvider Number
consume = do
  {head, tail} <- gets $ unsafeFromJust <<< uncons
  put tail
  pure head

choice :: forall a. List a -> RandProvider a
choice lst = do
  len <- pure $ length lst
  u <- consume
  let idx = round $ u * (toNumber len - 1.0)
  pure $ unsafePartial fromJust $ index lst idx








---
--- Betavariate and associated functions
---

betavariate :: StarMakerT RandProvider Number
betavariate = do
  { alpha, beta } <- ask
  alpha_gamma <- lift $ rgamma alpha 1.0
  beta_gamma <- lift $ rgamma beta 1.0
  pure $ alpha_gamma / (alpha_gamma + beta_gamma)

sgMagic ∷ Number
sgMagic = 1.0 + log 4.5
log4 ∷ Number
log4 = log 4.0

rgamma :: Number -> Number -> RandProvider Number
rgamma alpha beta
  | alpha > 1.0 = do
      u1 <- retryUnlessM (\a -> and $ [ a > 1e-7, a < 0.9999999]) consume
      u2 <- (1.0 - _) <$> consume
      let ainv = sqrt $ 2.0 * alpha - 1.0
          bbb = alpha - log4
          ccc = alpha + ainv
          v = log (u1 / ( 1.0 - u1 ) ) / ainv
          x = alpha * exp v
          z = u1 * u1 * u2
          r = bbb + ccc * v - x
      if or [(r + sgMagic - 4.5 * z > 0.0), (r >= log z)]
        then pure $ x * beta
        else rgamma alpha beta
  | alpha == 1.0 = do
      u <- retryUnlessM (_ <= 1e-7) consume
      pure $ beta * negate log u
  | otherwise = do
      u3 <- consume
      u4 <- consume
      let b = (e + alpha) / e
          p = b * u3
          x = if (p <= 1.0) then pow p (1.0 / alpha) else negate log ( ( b - p ) / alpha )
          ret = x * beta
      if (p > 1.0)
        then if (u4 <= pow x (alpha - 1.0))
                then pure ret
                else rgamma alpha beta
        else if (u4 <= exp (-x))
                then pure ret
                else rgamma alpha beta






---
--- Helpers
---

tickToRadian :: Tick -> Radian
tickToRadian time = 2.0 * pi * time

posOnCircle :: Circle -> Tick -> Position
posOnCircle circle time = 
  let rotation = tickToRadian $ time * circle.rotationsPerTick
  in { x: sin rotation * circle.radius + circle.center.x
      , y: cos rotation * circle.radius + circle.center.y }

randStarIdx :: StarMakerT RandProvider Int
randStarIdx = do
  {starCount} <- ask
  lift $ (\a -> round $ a * (toNumber starCount - 1.0)) <$> consume




---
--- StarMaker
---

--- StarMaker is simply an environment wrapper that provides the config record StarMakerConfig
--- to functions related to Galaxy generation.

type StarMakerConfig = {
  alpha :: Number,
  beta :: Number,
  scale :: Number,
  playerCount :: Int,
  orbits :: List Circle,
  starCount :: Int,
  wormholeCount :: Int
}

type StarMakerT :: forall k. (k -> Type) -> k -> Type
type StarMakerT m = ReaderT StarMakerConfig m
type StarMaker = StarMakerT Identity

runStarMaker :: StarMakerConfig -> Effect Galaxy
runStarMaker config = do
  manyUniforms <- replicateM 5000 random
  let uniforms = cycle manyUniforms
  pure $ unwrap $ (flip evalStateT uniforms) $ (flip runReaderT config) mkGalaxy

-- A star’s position is a random distance (betavariate distribution) away from an anchor point along a randomly chosen orbit (uniform) at a random time (uniform).
-- The theta of the star about its anchor is the same as the theta of the anchor about its orbit.
-- We achieve this last condition by calculating the star and the anchor using the same rotation rate.
randStarPos :: StarMakerT RandProvider Position
randStarPos = do
  { orbits, scale } <- ask
  distance <- (_ * scale) <$> betavariate
  orbit <- lift $ choice orbits
  time <- lift consume
  let anchor = posOnCircle orbit time
  pure $ posOnCircle { center: anchor, radius: distance, rotationsPerTick: orbit.rotationsPerTick } time

-- Makes the first playerCount stars homeworlds such that starUid equals playerUid.
mkGalaxy :: StarMakerT RandProvider Galaxy
mkGalaxy = do
  {starCount, playerCount, wormholeCount} <- ask
  homeworlds <- traverse (\uid -> asHomeworld uid <$> mkStar uid) $ range 1 playerCount
  otherworlds <- traverse mkStar $ range (playerCount + 1) starCount
  wormholes <- traverse (const mkWormhole) $ range 1 wormholeCount
  pure $ { stars: homeworlds <> otherworlds
              , wormholes: wormholes }
  where mkStar uid = do
          resources <- (\a -> round $ a * 50.0) <$> lift consume
          pos <- randStarPos
          pure { uid: uid
                    , puid: Nothing
                    , name: "Star" <> show uid
                    , e: 0
                    , i: 0
                    , s: 0
                    , st: 0
                    , g: 0
                    , r: resources
                    , x: pos.x
                    , y: pos.y }
        asHomeworld puid star = star { e=10, i=5, s=2, st=50, r=50, puid=Just puid }
        mkWormhole :: StarMakerT RandProvider Wormhole
        mkWormhole = do
          uid1 <- randStarIdx
          uid2 <- retryUnlessM (\a -> a /= uid1) randStarIdx
          pure $ [uid1, uid2]





---
--- Config Validation
---

validateConfig :: StarMakerConfig -> ExceptT String Effect StarMakerConfig
validateConfig config = do
  when (isNaN config.alpha) $ throwError "Alpha must not be NaN."
  when (isNaN config.beta) $ throwError "Beta must not be NaN."
  when (config.alpha < 0.0) $ throwError "Alpha must be at least 0."
  when (config.beta < 0.0) $ throwError "Beta must be at least 0."
  when (config.beta > 1.0) $ throwError "Beta must be at most 1."
  when (config.playerCount < 2) $ throwError "PlayerCount must be at least 2."
  when (config.starCount < 2) $ throwError "StarCount must be at least 2."
  when (config.wormholeCount < 0) $ throwError "WormholeCount must be at least 0."
  when (config.playerCount > config.starCount) $ throwError "StarCount must be greater than PlayerCount."
  when (config.wormholeCount > config.starCount) $ throwError "StarCount must be greater than WormholeCount."
  _ <- traverse validateCircle config.orbits
  pure config

validateCircle :: forall m. Monad m => Circle -> ExceptT String m Unit
validateCircle circle = do
  when (isNaN circle.center.x) $ throwError "Circle x value must not be NaN."
  when (isNaN circle.center.y) $ throwError "Circle y value must not be NaN."
  when (circle.radius < 0.0) $ throwError "Circle radius must be at least 0."
  when (circle.rotationsPerTick < 0.0) $ throwError "Circle RotationsPerTick must be at least 0."






---
--- Generic Utitlies
---

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust val = unsafePartial $ fromJust val

retryUnlessM :: forall a m. Monad m => (a -> Boolean) -> m a -> m a
retryUnlessM pred action = do
  res <- action
  if (pred res) then (pure res) else (retryUnlessM pred action)






---
--- Ugly UI Shit
---

updateStateByLabel :: String -> Number -> ConfigInputsState -> ConfigInputsState
updateStateByLabel label newVal state = case label of
  "alpha" -> state { config = set (Proxy :: Proxy "alpha") newVal  state.config }
  "beta" -> state { config = set (Proxy :: Proxy "beta") newVal  state.config }
  "scale" -> state { config = set (Proxy :: Proxy "scale") newVal  state.config }
  "playerCount" -> state { config = state.config { playerCount = round newVal } }
  "starCount" -> state { config = state.config { starCount = round newVal } }
  "wormholeCount" -> state { config = state.config { wormholeCount = round newVal } }
  otherwise -> state -- shouldn't happen
  
  
type ConfigInputsState = 
  { config :: StarMakerConfig
  , galaxyJson :: String
  }

type ConfigInputsSlots = 
  ( slider :: forall query. H.Slot query SliderAction Int
  , display :: forall query. H.Slot query Void Int
  )
  
_slider = Proxy :: Proxy "slider"
_display = Proxy :: Proxy "display"

data ConfigAction = HandleSliderChange SliderAction

configInputs :: forall query output m. MonadEffect m => H.Component query ConfigInputsState output m
configInputs = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
  where initialState :: ConfigInputsState -> ConfigInputsState
        initialState = identity
        
        handleAction :: forall a output m. MonadEffect m => ConfigAction -> H.HalogenM ConfigInputsState ConfigAction ConfigInputsSlots output m Unit
        handleAction action = do
          case action of
            HandleSliderChange sliderAction ->
              case sliderAction of
                Slide label newVal -> do
                  state <- H.modify $ updateStateByLabel label newVal
                  eitherGalaxy <- H.liftEffect $ runExceptT $ recalcGalaxy state.config
                  case eitherGalaxy of
                    Left err -> H.modify_ $ set (Proxy :: Proxy "galaxyJson") err
                    Right galaxy -> H.modify_ $ set (Proxy :: Proxy "galaxyJson") $ JSON.writeJSON galaxy
        
        recalcGalaxy :: StarMakerConfig -> ExceptT String Effect Galaxy
        recalcGalaxy config = do
          _ <- (validateConfig config :: ExceptT String Effect StarMakerConfig)
          lift $ runStarMaker config
        
        render :: forall m. MonadEffect m => ConfigInputsState -> H.ComponentHTML ConfigAction ConfigInputsSlots m
        render state =
          HH.div_
            [ HH.div_
                [ HH.slot _slider 0 slider { label: "alpha",  min: 0.0, max: 1.0, step: 0.001, value: state.config.alpha  } HandleSliderChange
                , HH.slot _slider 1 slider { label: "beta",   min: 0.0, max: 1.0, step: 0.001, value: state.config.beta } HandleSliderChange
                , HH.slot _slider 2 slider { label: "scale",   min: 0.0, max: 20.0, step: 0.01, value: state.config.scale } HandleSliderChange
                , HH.slot _slider 3 slider { label: "playerCount",   min: 2.0, max: 50.0, step: 1.0, value: toNumber state.config.playerCount } HandleSliderChange
                , HH.slot _slider 4 slider { label: "starCount",   min: 2.0, max: 300.0, step: 1.0, value: toNumber state.config.starCount } HandleSliderChange
                , HH.slot _slider 5 slider { label: "wormholeCount",   min: 0.0, max: 300.0, step: 1.0, value: toNumber state.config.wormholeCount } HandleSliderChange
                ]
            , HH.slot_ _display 0 jsonDisplay state.galaxyJson
            ]




data JsonDisplayAction = Receive String
type JsonDisplayState = String

jsonDisplay :: forall query output m. H.Component query JsonDisplayState output m
jsonDisplay = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
  }
  where initialState :: JsonDisplayState -> JsonDisplayState
        initialState = identity
        
        handleAction :: forall a output m. JsonDisplayAction -> H.HalogenM JsonDisplayState JsonDisplayAction () output m Unit
        handleAction = case _ of
          Receive galaxyJson -> H.put galaxyJson
        
        render :: forall m. String -> H.ComponentHTML JsonDisplayAction () m
        render galaxyJson =
          HH.div
            [ HP.style "width: 700px; height: 500px; overflow: scroll;" ]
            [ HH.text galaxyJson ]



type SliderState =
  { label :: String
  , min :: Number
  , max :: Number
  , step :: Number
  , value :: Number
  }

data SliderAction = Slide String Number

slider :: forall query m. MonadEffect m => H.Component query SliderState SliderAction m
slider = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where initialState :: SliderState -> SliderState
        initialState = identity
        
        handleAction :: forall m. MonadEffect m => SliderAction -> H.HalogenM SliderState SliderAction () SliderAction m Unit
        handleAction = case _ of
          Slide label newVal -> do
            _ <- H.liftEffect $ Console.log "testing"
            label <- gets _.label
            H.raise $ Slide label newVal
            H.modify_ \st -> st { value=newVal }
        
        render :: forall m. SliderState -> H.ComponentHTML SliderAction () m
        render state =
          HH.div_
            [ HH.text $ state.label
            , HH.input
                [ HP.value $ show state.value
                , HP.type_ HP.InputRange
                , HP.min state.min
                , HP.max state.max
                , HP.step $ Step state.step
                , HE.onValueInput \newVal -> Slide state.label $ unsafeFromJust $ Number.fromString $ newVal]
            , HH.text $ show state.value
            ]


defaultConfig :: StarMakerConfig
defaultConfig = { alpha: 0.3
                , beta: 0.6
                , scale: 3.0
                , playerCount: 8
                , orbits: { center: {x: 0.0, y: 0.0}, radius: 10.0, rotationsPerTick: 10.0 } : nil
                , starCount: 100
                , wormholeCount: 5 }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI configInputs {config: defaultConfig, galaxyJson: "Fiddle with slides to begin generation"} body

-- main :: Effect Unit
-- main = do
--   let config =  { alpha: 0.3
--                 , beta: 0.6
--                 , scale: 3.0
--                 , playerCount: 8
--                 , orbits: { center: {x: 0.0, y: 0.0}, radius: 10.0, rotationsPerTick: 10.0 } : nil
--                 , starCount: 100
--                 , wormholeCount: 5 }
--   validConfig <- runExceptT (validateConfig config)
--   case validConfig of
--     Left err -> Console.log err
--     Right (conf :: StarMakerConfig) -> do
--       (galaxy :: Galaxy) <- runStarMaker conf
--       Console.log $ JSON.writeJSON galaxy
