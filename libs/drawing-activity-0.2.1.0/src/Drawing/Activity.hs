
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE CPP                        #-}

module Drawing.Activity
    (
    -- * Main pure entry points
      drawingOf, activityOf
    -- * Main entry point with IO effects
    , activityOfIO
    -- * Update
    , State
    , UpdateM, Sink
    , module Control.Monad.State.Class
    , deferIO, deferIO_, deferSub
    -- * View
    , View, ViewProp, drawingView
    , Drawing
    -- ** Events
    , onKeyDown, onKeyUp, onKeyDown', onKeyUp'
    , onMouseDown, onMouseUp, onMouseMove
    -- * Low level
    , JSM, drawingOf', activityOf', activityOfIO'
    )
where
import           Drawing.Activity.StableNamed
import           Drawing.Activity.Internal.Event

import           Drawing hiding (text)
import           Drawing.Render (unitSize, renderSvgTree, SvgTree(..))
import           Drawing.Vector

import           Miso hiding (View, onKeyDown, onKeyUp, onMouseDown, onMouseUp)
import qualified Miso as Miso

import           Data.List (sort)
import           Data.Maybe (isJust)

import           Control.Monad (when, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           GHC.Generics

import           System.IO (hFlush, stdout)

import Language.Javascript.JSaddle.RunWarp (run)

-- ---------------------------------------------------------------
-- Main pure entry points
-- ---------------------------------------------------------------

-- | A simple drawing program.
drawingOf' :: Double             -- ^ The width (in drawing units) of the view.
          -> Double             -- ^ The height (in drawing units) of the view.
          -> Drawing
          -> JSM ()
drawingOf' width height draw =
    activityOf' width height () (const $ pure ()) (const $ drawingView [] draw)

drawingOf :: Int               -- ^ Port
          -> Double             -- ^ The width (in drawing units) of the view.
          -> Double             -- ^ The height (in drawing units) of the view.
          -> Drawing
          -> IO ()
drawingOf port width height draw =
    activityOf port width height () (const $ pure ()) (const $ drawingView [] draw)

{-- TODO:
type Time = Double

-- | A program which shows an animation, with a picture for each time given by the parameter.
animationOf :: Double                   -- ^ The width (in drawing units) of the view.
            -> Double                   -- ^ The height (in drawing units) of the view.
            -> Int                      -- ^ Interval in millis
            -> (Time -> Drawing)        -- ^ Function to produce the next frame of animation.
                                        --   It is passed the time in seconds since the program started.
            -> JSM ()
animationOf width height view =
    activityOfIO width height 0.0 update view'
    where
        update (TimePassing t _) = const t
        update _ = id
        view' t = ([], view t)

splitTime :: Time -> (Time -> model -> model) -> Time -> model -> model
splitTime maxdt update dt state =
    if maxdt < dt then splitTime maxdt update (dt - maxdt) (update maxdt state)
    else update dt state
--}

-- | An interactive program that responds to Events.
-- Activities can interact with the user, change over time, and remember information about the past.
#ifdef TEST_STABLENAMED
activityOf' :: Eq model =>
#else
activityOf' ::
#endif
              Double             -- ^ The width (in drawing units) of the view.
           -> Double             -- ^ The height (in drawing units) of the view.
           -> model              -- ^ The initial state of the activity.
           -> (action -> State model ())
                        -- ^ The event handling function, which updates the state given an event.
           -> (model -> View action)
                        -- ^ The visualization function, which converts the state into a picture to display.
           -> JSM ()
activityOf' width height state0 update view =
    activityOfIO' width height state0 (modify . execState . update) view

activityOf :: Int                -- ^ Port
           -> Double             -- ^ The width (in drawing units) of the view.
           -> Double             -- ^ The height (in drawing units) of the view.
           -> model              -- ^ The initial state of the activity.
           -> (action -> State model ())
                        -- ^ The event handling function, which updates the state given an event.
           -> (model -> View action)
                        -- ^ The visualization function, which converts the state into a picture to display.
           -> IO ()
activityOf port width height state0 update view =
    activityOfIO port width height state0 (modify . execState . update) view

-- ---------------------------------------------------------------
-- Main entry points (with IO effects)
-- ---------------------------------------------------------------

type UpdateM model action = StateT model (Writer [Sink action -> IO ()])

data View action = View Drawing [ViewProp action]
    deriving Functor

drawingView :: [ViewProp action] -> Drawing -> View action
drawingView = flip View

-- | An interactive program that responds to Events.
-- Activities can interact with the user, change over time, and remember information about the past.
#ifdef TEST_STABLENAMED
activityOfIO' :: Eq model =>
#else
activityOfIO' ::
#endif
              Double    -- ^ The width (in drawing units) of the view.
           -> Double    -- ^ The height (in drawing units) of the view.
           -> model     -- ^ The initial state of the activity.
           -> (action -> UpdateM model action ())
                        -- ^ The event handling function, which updates the state given an event.
           -> (model -> View action)
                        -- ^ The visualization function, which converts the state into a picture to display.
           -> JSM ()
activityOfIO' width height state0 update view = do
  let env = Env unitSize
                (round $ width * unitSize / 2) (round $ height * unitSize / 2)
                update view
  let
    initialAction = MNoOp
    model      = initModel env state0
    update     = fromTransition . updateModel env
    view       = viewModel env
    events     = defaultEvents
    -- Aquesta línia era la que provocava els conflictes:
    -- 'events = M.insert "mousemove" False defaultEvents'
    mountPoint = Nothing
    subs       = [ mouseSub MMouseMov ]
    logLevel   = Off
  startApp App{..}

activityOfIO :: Int              -- ^ Port
           -> Double             -- ^ The width (in drawing units) of the view.
           -> Double             -- ^ The height (in drawing units) of the view.
           -> model              -- ^ The initial state of the activity.
           -> (action -> UpdateM model action ())
                        -- ^ The event handling function, which updates the state given an event.
           -> (model -> View action)
                        -- ^ The visualization function, which converts the state into a picture to display.
           -> IO ()
activityOfIO port width height state0 update view = do
  putStrLn $ "Open URL: http://localhost:"<>show port <>"/"
  run port $
    activityOfIO' width height state0 update view


-- | Schedule a single IO action for later execution.
deferIO :: IO action -> UpdateM model action ()
deferIO io = deferSub $ \sink -> io >>= sink

-- | Schedule a single IO action for later execution.
deferIO_ :: IO () -> UpdateM model action ()
deferIO_ io = deferSub $ const io

-- | Schedule a single IO action for later execution.
deferSub:: (Sink action -> IO ()) -> UpdateM model action ()
deferSub sub = tell [ sub ]


-- ---------------------------------------------------------------
-- Internal (implementation)

data Env model action = Env
        { eUnitSize :: !Double
        , eViewXMax :: !Int -- in pixels
        , eViewYMax :: !Int -- in pixels
        , eInput :: !(action -> UpdateM model action ())
        , eVisual :: !(model -> View action)
        }

---------------------------------
-- Initialize the model

data Model model action = Model
  { mState :: !(StableNamed model)
  -- View state:
  , mMouseIsIn :: !(Maybe (Int, Int)) -- client coords of the top-left corner
  , mViewState :: !(StableNamed (View' action))
  }
  deriving (Eq)

initModel :: Env model action -> model -> Model model action
initModel env state0 =
  let sstate0 = mkStableNamed state0
  in Model
    { mState = sstate0
    , mMouseIsIn = Nothing
    , mViewState = mkStableNamed $ preViewModel env sstate0
    }

---------------------------------
-- Update the model

updateModel :: Env model action -> Msg action -> Transition (Msg action) (Model model action) ()
updateModel env msg = do
#ifdef PRINT_MESSAGES
    scheduleIO_ $ liftIO $ do
        putStrLn $ show msg
        hFlush stdout
#endif
    oldstate <- gets mState
    update1 env msg
    update2 env oldstate

update1 :: Env model action -> Msg action -> Transition (Msg action) (Model model action) ()
update1 _ MNoOp =
    pure ()

update1 _ (MMouseIn mbinfo) = do
    let mkNodeClient info = client info ^-^ offset info
    modify $ \ model -> model{ mMouseIsIn = mkNodeClient <$> mbinfo }

update1 env (MMouseDownUp info f) = do
    let p = offsetToPoint env $ offset info
    handleAppEvent env $ f p

update1 env (MMouseMov client) = do
    Model{..} <- get
    case mMouseIsIn of
        Just svgClient -> do
            let p = offsetToPoint env $ client ^-^ svgClient
                (_, _, onmoves) = unStableNamed mViewState
            forM_ onmoves $ \f ->
                handleAppEvent env $ f p
            pure ()
        Nothing -> pure ()

update1 env (MAction act) =
    handleAppEvent env act

offsetToPoint :: Env model action -> (Int, Int) -> Point
offsetToPoint env (offsetX, offsetY) =
    let
        x = fromIntegral (offsetX - eViewXMax env) / eUnitSize env
        y = fromIntegral (eViewYMax env - offsetY) / eUnitSize env
    in (x, y)

handleAppEvent :: Env model action -> action -> Transition (Msg action) (Model model action) ()
handleAppEvent env ev = do
    model <- get
    let (state', subs) = runWriter $ execStateT (eInput env ev) $ unStableNamed $ mState model
    put $ model{ mState = mkStableNamed state' }
    mapM_ sched subs

sched :: (Sink action -> IO ()) -> Transition (Msg action) (Model model action) ()
sched sub = scheduleSub $ \sink -> liftIO $ sub $ \a -> sink (MAction a)

update2 :: Env model action -> StableNamed model -> Transition (Msg action) (Model model action) ()
update2 env oldstate = do
    model <- get
    let newstate = mState model
    when (oldstate /= newstate) $ do
        let viewmodel = preViewModel env newstate
        put $ model{ mViewState = mkStableNamed viewmodel }

type View' action = (Drawing, [Attribute (Msg action)], [Point -> action])

preViewModel :: Env model action -> StableNamed model -> (Drawing, [Attribute (Msg action)], [Point -> action])
preViewModel env state =
    let View draw drawprops = eVisual env $ unStableNamed state
        (attrs, onmoves) = part drawprops
    in (draw, attrs, onmoves)
    where
        part [] = ([], [])
        part (ViewProp attr : props) =
            let (attrs, onmoves) = part props
            in (attr : attrs, onmoves)
        part (PropOnMouseMove onmove : props) =
            let (attrs, onmoves) = part props
            in (attrs, onmove : onmoves)


---------------------------------
-- Rendering the view

viewModel :: Env model action -> Model model action -> Miso.View (Msg action)
viewModel env Model{..} =
  let (draw, attrs, _) = unStableNamed mViewState
  in div_ [ class_ "container-fluid" ]
    [ h3_ []
        [ text "Drawing.Activity" ]
    , div_ [ id_ "canvas", class_ "row" ]
        [ renderSvgView env draw attrs ]
    ]

renderSvgView :: Env model action -> Drawing -> [Attribute (Msg action)] -> Miso.View (Msg action)
renderSvgView env@Env{..} draw drawprops =
    let SvgTree "svg" attrs children = renderSvgTree eViewXMax eViewYMax draw
        svgattrs = (uncurry prop <$> attrs) <> svgEvents <> drawprops
    in elem "svg" svgattrs (svgTreeToView <$> children)
  where
    svgEvents :: [Attribute (Msg action)]
    svgEvents =
        [ prop "tabindex" (0::Int)
        , on "mouseover" mouseInfoDecoder $ MMouseIn . Just
        , on "mouseout"  mouseInfoDecoder $ const $ MMouseIn Nothing
        ]

    elem name as cs = node SVG name Nothing as cs
    svgTreeToView :: SvgTree -> Miso.View (Msg action)
    svgTreeToView (SvgTree name attrs children) =
        elem name (uncurry prop <$> attrs) (svgTreeToView <$> children)
    svgTreeToView (SvgText txt) =
        text txt

