module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, code)
import DOM.Event.Types (Event, EventType(EventType), EventTarget, readEventTarget, KeyboardEvent)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (load, keydown, keyup)
import DOM.HTML.HTMLCanvasElement (setHeight, setWidth)
import DOM.HTML.Types (readHTMLCanvasElement, htmlDocumentToDocument, htmlCanvasElementToHTMLElement, HTMLCanvasElement, htmlElementToElement, htmlElementToNode)
import DOM.HTML.Window (document, requestAnimationFrame)
import DOM.Node.Document (createElement)
import DOM.Node.Node (appendChild)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId), elementToNode, Document, documentToNonElementParentNode)
import Data.Either (Either(..), either)
import Data.Foreign (toForeign)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromJust, fromMaybe)
import Data.Monoid (mempty)
import Data.Set (Set, insert, delete, member)
import Graphics.Canvas (CanvasElement, getContext2D, Context2D, CANVAS, setFillStyle, fillRect, beginPath, arc, fill)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- Referencing:
-- https://robots.thoughtbot.com/pong-clone-in-javascript

canvasWidth = 400
canvasHeight = 600

toCanvasElement :: Element -> Maybe HTMLCanvasElement
toCanvasElement = either (const Nothing) Just <<< runExcept <<< readHTMLCanvasElement <<< toForeign

toEventTarget :: forall a. a -> Maybe EventTarget
toEventTarget = either (const Nothing) Just <<< runExcept <<< readEventTarget <<< toForeign

toKeyboardEvent :: forall a. Event -> Maybe KeyboardEvent
toKeyboardEvent = either (const Nothing) Just <<< runExcept <<< eventToKeyboardEvent



createCanvasElement :: Eff _ HTMLCanvasElement
createCanvasElement = do
  document <- htmlDocumentToDocument <$> (document =<< window)
  bodyEl :: Maybe Element <- getElementById (ElementId "body") $ documentToNonElementParentNode document
  let body' = unsafePartial fromJust bodyEl
  canvasEl :: Maybe HTMLCanvasElement <- toCanvasElement <$> (createElement "canvas" document)
  let canvasEl' = unsafePartial fromJust canvasEl
  setWidth canvasWidth canvasEl'
  setHeight canvasHeight canvasEl'
  _ <- appendChild
      (elementToNode $ htmlElementToElement $ htmlCanvasElementToHTMLElement canvasEl')
      (elementToNode body')
  pure canvasEl'

interactWall :: Ball -> Ball
interactWall ball =
  if ball.x - ball.radius < 0.0 -- if hitting left wall
    then ball { x = ball.x + ball.radius, dx = ball.dx * -1.0 }
    else
      if ball.x + ball.radius > toNumber canvasWidth -- if hitting the right wall
        then ball { x = toNumber canvasWidth - ball.radius, dx = ball.dx * -1.0 }
        else ball

interactGoal :: Ball -> Ball
interactGoal ball =
  if ball.y < 0.0 || ball.y > toNumber canvasHeight -- if a point was scored
    then ball { dx = 0.0, dy = 3.0, x = 200.0, y = 300.0 }
    else ball

topSq :: forall r. { y :: Int, h :: Int | r} -> Int
topSq s = s.y + s.h

botSq :: forall r. { y :: Int | r} -> Int
botSq s = s.y

topCir :: forall r. { x :: Int, r :: Int | r} -> Int
topCir c = c.x + c.r

botCir :: forall r. { x :: Int, r :: Int | r} -> Int
botCir c = c.x - c.r

interactPaddle :: Paddle -> Ball -> Ball
interactPaddle paddle ball =
  -- var top_x = this.x - 5;
  -- var top_y = this.y - 5;
  -- var bottom_x = this.x + 5;
  -- var bottom_y = this.y + 5;
  -- (top_y < (paddle1.y + paddle1.height)
  --   && bottom_y > paddle1.y
  --   && top_x < (paddle1.x + paddle1.width)
  --   && bottom_x > paddle1.x)
  -- (top_y < (paddle2.y + paddle2.height)
  --   && bottom_y > paddle2.y
  --   && top_x < (paddle2.x + paddle2.width)
  --   && bottom_x > paddle2.x)
  if intersects ball paddle
    then
      let newDy = ball.dy * -1.0
      in ball { dy = newDy, dx = ball.dx + (paddle.dx / 2.0), y = ball.y + newDy }
    else ball

intersects :: Ball -> Paddle -> Boolean
intersects ball paddle =
  if ball.y - ball.radius < paddle.y + paddle.h
     && ball.y + ball.radius > paddle.y
     && ball.x - ball.radius < paddle.x + paddle.w
     && ball.x + ball.radius > paddle.x
    then true
    else false

applySpeed :: Ball -> Ball
applySpeed ball = ball { x = ball.x + ball.dx, y = ball.y + ball.dy }

--data Direction = Up | Down
--dir :: forall r. { dx :: Int, dy :: Int | r } -> Direction
--dir a = case a.dx of
--  dx | dx > 0 -> Up
--  dx | dx < 0 -> Down

movePaddleP1 :: Inputs -> Paddle -> Paddle
movePaddleP1 inputs paddle =
  if "ArrowRight" `member` inputs.keys
    then movePaddle 4.0 paddle
    else
      if "ArrowLeft" `member` inputs.keys
        then movePaddle (-4.0) paddle
        else paddle

movePaddleP2 :: Ball -> Paddle -> Paddle
movePaddleP2 ball paddle =
  let
    diffToBall = -((paddle.x + paddle.w / 2.0) - ball.x)
    -- max speed left
    -- max speed right
    diffToBall' =
      if diffToBall < -3.0
        then -3.0
        else
          if diffToBall > 3.0
            then 3.0
            else diffToBall
  in movePaddle diffToBall' paddle

movePaddle :: Number -> Paddle -> Paddle
movePaddle x paddle =
  let
    newX = paddle.x + x
  in
    if newX < 0.0
    then paddle { x = 0.0, dx = 0.0 }
    else
      if newX + paddle.w > toNumber canvasWidth
        then paddle { x = toNumber canvasWidth - paddle.w, dx = 0.0 }
        else paddle { x = newX, dx = x }

updateGameState :: forall eff. Inputs -> GameState -> GameState
updateGameState inputs state =
  state
    { ball = nextBall
    , paddle_p1 = nextPaddleP1
    , paddle_p2 = nextPaddleP2
    }
  where
    nextPaddleP1 = movePaddleP1 inputs state.paddle_p1
    nextPaddleP2 = movePaddleP2 state.ball state.paddle_p2
    nextBall =
      ( interactGoal
        <<< (interactPaddle state.paddle_p1)
        <<< (interactPaddle state.paddle_p2)
        <<< interactWall
        <<< applySpeed
      )
      state.ball

type Paddle =
  { x :: Number, y :: Number
  , w :: Number, h :: Number
  , dx :: Number, dy :: Number
  }

type Ball =
  { x :: Number, y :: Number
  , dx :: Number, dy :: Number
  , radius :: Number
  }

type GameState =
  { paddle_p1 :: Paddle
  , paddle_p2 :: Paddle
  , ball :: Ball
  }

initialGameState :: GameState
initialGameState =
  { paddle_p1:
    { x: 175.0, y: 580.0
    , w: 50.0, h: 10.0
    , dx: 0.0, dy: 0.0
    }
  , paddle_p2:
    { x: 175.0, y: 10.0
    , w: 50.0, h: 10.0
    , dx: 0.0, dy: 0.0
    }
  , ball:
    { x: 200.0, y: 300.0
    , dx: 0.0, dy: 3.0
    , radius: 5.0
    }
  }

renderPaddle :: Context2D -> Paddle -> Eff _ Unit
renderPaddle context2D paddle = do
  _ <- setFillStyle "#0000FF" context2D
  _ <- fillRect context2D
    { x: paddle.x, y: paddle.y
    , w: paddle.w, h: paddle.h
    }
  pure unit

renderBall :: Context2D -> Ball -> Eff _ Unit
renderBall context2D ball = do
  _ <- setFillStyle "#000000" context2D
  _ <- beginPath context2D
  _ <- arc context2D
    { x: ball.x, y: ball.y
    , r: ball.radius
    , start: 0.0, end: 2.0 * 3.1415629
    }
  _ <- setFillStyle "#000000" context2D
  _ <- fill context2D
  pure unit

renderGameState :: forall eff. Context2D -> GameState -> Eff (canvas :: CANVAS, dom :: DOM | eff) Unit
renderGameState context2D state = do
  -- Render Background
  _ <- setFillStyle "#FF00FF" context2D
  _ <- fillRect context2D
    { x: 0.0, y: 0.0
    , w: toNumber canvasWidth, h: toNumber canvasHeight
    }
  renderPaddle context2D state.paddle_p1
  renderPaddle context2D state.paddle_p2
  renderBall context2D state.ball
  pure unit

runGameLoop :: forall eff. Context2D -> GameState -> Ref Inputs
  -> Eff (canvas :: CANVAS, dom :: DOM, ref :: REF | eff) Unit
runGameLoop context2D state inputs = do
  inputs' <- readRef inputs
  let state' = updateGameState inputs' state
  renderGameState context2D state'
  _ <- window >>= requestAnimationFrame (runGameLoop context2D state' inputs)
  pure unit

type Inputs = { keys :: Set String }

onLoad :: forall eff. Event -> Eff _ Unit
onLoad _ = do
  -- Create canvas element to work with.
  canvasEl <- createCanvasElement
  -- Get a 2D context from it. All canvas commands operate on a context, not the canvas element.
  let canvas :: CanvasElement
      canvas = unsafeCoerce canvasEl
  context2D <- getContext2D canvas
  -- Listen for external inputs
  windowEt <- window <#> unsafePartial fromJust <<< toEventTarget
  inputsRef :: Ref Inputs <- newRef { keys: mempty }
  let
    onKeydown :: Event -> Eff _ Unit
    onKeydown ev =
      let
        f :: KeyboardEvent -> Inputs -> Inputs
        f kEv i = i { keys = insert (code kEv) i.keys }
      in maybe (pure unit) (modifyRef inputsRef <<< f) $ toKeyboardEvent ev
    onKeyup :: Event -> Eff _ Unit
    onKeyup ev =
      let
        f :: KeyboardEvent -> Inputs -> Inputs
        f kEv i = i { keys = delete (code kEv) i.keys }
      in maybe (pure unit) (modifyRef inputsRef <<< f) $ toKeyboardEvent ev
  addEventListener keydown (eventListener onKeydown) false windowEt
  addEventListener keyup (eventListener onKeyup) false windowEt
  runGameLoop context2D initialGameState inputsRef

main :: Eff (canvas :: CANVAS, console :: CONSOLE, dom :: DOM, ref :: REF) Unit
main = do
  -- Start animation when the document is loaded.
  --documentEt <- window >>= document <#> unsafePartial fromJust <<< toEventTarget <<< htmlDocumentToDocument
  --addEventListener (EventType "ready") (eventListener onLoad) false documentEt
  windowEt <- window <#> unsafePartial fromJust <<< toEventTarget
  addEventListener load (eventListener onLoad) false windowEt
  pure unit
