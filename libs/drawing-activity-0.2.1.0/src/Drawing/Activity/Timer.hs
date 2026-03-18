
{-# LANGUAGE OverloadedStrings          #-}

module Drawing.Activity.Timer
    ( view
    , Timer
    , newTimer
    , cancelTimer
    )
where
import           Miso hiding (view, forkJSM)
import           Miso.String

import qualified Language.Javascript.JSaddle as JSM

import qualified Control.Concurrent as Conc
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class (liftIO)


view :: Double -> (Double -> a) -> View a
view timesBySecond timesBySecondChange =
    div_ []
            [ text "Time events by second: "
            , input_ [ type_ "range", id_ "refr-range"
                     , min_ (ms rangeStop), max_ (ms rangeMax)
                     , value_ (ms $ timesBySecondToRange timesBySecond)
                     , onInput (timesBySecondChange . rangeToTimesBySecond . fromMisoString)
                     ]
            , text (ms $ show $ roundN 2 timesBySecond)
            ]

-- number of times by second is 2^rangeValue
rangeStop, rangeMin, rangeMax :: Int
rangeStop = rangeMin - 1
rangeMin = -2
rangeMax = 7

rangeToTimesBySecond :: Int -> Double
rangeToTimesBySecond r =
    if r < rangeMin then 0.0
    else 2.0 ^^ r

timesBySecondToRange :: Double -> Int
timesBySecondToRange f =
    if f < (2.0 ^^ rangeMin) then rangeStop
    else round (log f / log 2.0)

roundN :: Int -> Double -> Double
roundN n x =
    let f = 10 ^ n
    in  fromIntegral (round (x * f)) / f

---------------------------------
-- Timer management

newtype Timer = Timer (Conc.MVar TimerState)
    deriving (Eq)

type TimerState = Maybe Conc.ThreadId

-- interval in millis
newTimer :: Int -> JSM () -> JSM Timer
newTimer interval action = do
    mvar <- liftIO Conc.newEmptyMVar
    let loop = do
            liftIO $ Conc.threadDelay (1000 * interval) -- micros
            self <- liftIO Conc.myThreadId
            state <- liftIO $ Conc.readMVar mvar
            if state == Just self then do
                maskJSM_ action
                loop
            else
                pure ()
    tid <- forkJSM loop
    liftIO $ Conc.putMVar mvar $ Just tid
    pure $ Timer mvar

cancelTimer :: Timer -> JSM ()
cancelTimer (Timer mvar) = do
    state <- liftIO $ Conc.takeMVar mvar
    case state of
        Just tid -> liftIO $ Conc.killThread tid
        Nothing ->  pure ()
    liftIO $ Conc.putMVar mvar Nothing

------------------------------------
-- Extended concurrency to JSM

-- | Run given `JSM` action asynchronously, in a separate thread.
forkJSM :: JSM () -> JSM Conc.ThreadId
forkJSM a = do
  ctx <- JSM.askJSM
  liftIO (Conc.forkIO (JSM.runJSM a ctx))

maskJSM_ :: JSM a -> JSM a
maskJSM_ jsm = do
    ctx <- JSM.askJSM
    liftIO $ Exc.mask_ $ JSM.runJSM jsm ctx

