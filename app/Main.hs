{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import Foreign.C.Types
import Control.Monad(when)


data State=State{ curBrick::V2 CInt,
                  gameRenderer::Renderer,
                  isQuit::Bool,
                  otherBricks::[[V2 CInt]],
                  timeSteps::CInt,
                  isRefresh::Bool,
                  score::CInt,
                  numberOfLines::CInt,
                  numberOfColumns::CInt}


makeBrick :: V2 CInt -> Rectangle CInt
makeBrick pt= Rectangle (P pt) (V2 38 38)

placeOccupied :: V2 CInt -> [[V2 CInt]] -> Bool
placeOccupied _ [] = False
placeOccupied v (w:ws)=let placeOccupied' _ []=False
                           placeOccupied' v' (w':ws')=(v' == w') || placeOccupied' v' ws'
                       in placeOccupied' v w || placeOccupied v ws

isLessColumn :: V2 CInt -> V2 CInt -> Bool
isLessColumn (V2 x1 _) (V2 x2 _)=x1<x2

isBelongToLine :: V2 CInt -> [V2 CInt] -> Bool
isBelongToLine _ []=False
isBelongToLine (V2 _ y1) (V2 _ y2:_)=y1 == y2

insertBrickToLine:: V2 CInt -> [V2 CInt] -> [V2 CInt]
insertBrickToLine pt []=[pt]
insertBrickToLine pt (v:vs)=if isLessColumn v pt then v:insertBrickToLine pt vs else pt:v:vs 

insertBrick :: V2 CInt -> [[V2 CInt]] -> [[V2 CInt]]
insertBrick pt []=[[pt]]
insertBrick pt (l:ls)=if isBelongToLine pt l then insertBrickToLine pt l: ls else l:insertBrick pt ls

removeLine:: CInt -> [[V2 CInt]] -> [[V2 CInt]]
removeLine _ []=[]
removeLine y (l:ls)=let isLine _ []= False
                        isLine y' (V2 _ y'':_)=y'==y''
                    in if isLine y l then ls else l:removeLine y ls
moveLowerLines:: CInt -> [[V2 CInt]] -> [[V2 CInt]]
moveLowerLines _ []=[]
moveLowerLines y (l:ls)=let isLower _ []=False
                            isLower y' (V2 _ y'':_)=y''<y'
                            incrementLine []=[]
                            incrementLine (V2 x''' y''':ls')=V2 x''' (y'''+1):incrementLine ls'
                        in if isLower y l then incrementLine l:moveLowerLines y ls else l:moveLowerLines y ls
maxNumberColumn::[[V2 CInt]] -> (Int,CInt)
maxNumberColumn [] =(0,-1)
maxNumberColumn ls=let lineNumber []          =(-1)
                       lineNumber ((V2 _ y''):_)=y''
                       maxNumberColumn' [] accu=accu
                       maxNumberColumn' (l':ls') (accua, accul)=if length l' > accua 
                                                                  then maxNumberColumn' ls' (length l',lineNumber l')
                                                                  else maxNumberColumn' ls' (accua,accul)
                       (nc,y)=maxNumberColumn' ls (0,-1)
                   in  (nc,y)
removeLines:: State -> State
removeLines state=let pts=otherBricks state
                      (nc,y)=maxNumberColumn pts
                      pts'=removeLine y pts
                      pts''=moveLowerLines y pts'
                      score'=score state
                  in if nc ==  fromIntegral (numberOfColumns state)
                       then removeLines $ state {otherBricks=pts'',isRefresh=True,score=score'+1}
                       else state
moveLeft :: State -> State
moveLeft state= let (V2 x y)=curBrick state;
                    x'=x-1
                    pt'=V2 x' y
                    pts=otherBricks state
                in if x' < 1 || placeOccupied pt' pts
                    then state
                    else state {curBrick=pt',isRefresh=True}

moveRight :: State -> State
moveRight state= let (V2 x y)=curBrick state;
                     x'=x+1
                     pt'=V2 x' y
                     pts=otherBricks state
                 in if x' > numberOfColumns state || placeOccupied pt' pts
                      then state
                      else state {curBrick=pt',isRefresh=True}

endGame :: State -> State
endGame state=state {isQuit=True}

changeState::State -> Event -> State
changeState state myEvent=case eventPayload myEvent of
  (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeLeft _) )) -> moveLeft state
  (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeRight _) )) -> moveRight state
  (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeDown _) )) -> updateModel state
  (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeQ _) )) -> endGame state
  (WindowClosedEvent _) -> endGame state
  _ -> state

updateModel:: State -> State
updateModel state'= let state=removeLines state'
                        (V2 x y)=curBrick state
                        pts=otherBricks state
                        pt'= V2 x (y+1)
                        pts'=insertBrick (V2 x y) pts
                        refr'=True
                        xc=(numberOfColumns state `div` 2)+1
                    in if y > numberOfLines state
                          then state{curBrick= V2 xc 1,otherBricks=pts',isRefresh=refr'}
                          else
                            if placeOccupied pt' pts
                              then
                                if y==1
                                  then state{isQuit=True,isRefresh=refr'}
                                  else state{curBrick=V2 xc 1,otherBricks=pts',isRefresh=refr'}
                              else state{curBrick=pt',isRefresh=refr'}

processEvents:: State -> [Event] -> State
processEvents=foldl changeState


updateCounter::State -> State
updateCounter state = let cnt=timeSteps state
                      in  if cnt == 49  
                            then state {timeSteps = 0}
                            else state {timeSteps = cnt+1}
renderGame::State -> IO ()
renderGame state=do
  let r=gameRenderer state
      drawBrick (V2 ix iy) = fillRect r (Just $ makeBrick (V2 (1+((ix-1)*40)) (1+((iy-1)*40))))
      drawLineBricks [] = return () :: IO ()
      drawLineBricks (pt':pts')= do drawBrick pt'
                                    drawLineBricks pts' 
      drawBricks [] = return () :: IO ()
      drawBricks (pt':pts')= do drawLineBricks pt'
                                drawBricks pts' 
  when (isRefresh state) $ do
    rendererDrawColor r $= V4 0 0 0 128
    clear r
    rendererDrawColor r $= V4 255 255 0 128
    drawBrick $ curBrick state
    drawBricks $ otherBricks state
    present r

mainLoop :: State -> IO ()  
mainLoop state=do
  events <- pollEvents
  let state'=processEvents state events
      cnt=timeSteps state
      state''=if cnt == 0 then updateModel state' else state'
      state'''=state''{isRefresh=False}
  renderGame state''
  delay 10
  if isQuit state' then return () else mainLoop $ updateCounter state'''


main:: IO ()
main=do
  let nol=14 -- number of lines
      noc=9 -- number of column
  initialize (InitVideo,InitEvents)
  mywindow <- createWindow  "Little" defaultWindow {windowInitialSize=V2 (noc*40) (nol*40+40)} -- (defaultWindow { windowGraphicsContext=OpenGLContext defaultOpenGL})
  r <- createRenderer mywindow (-1) defaultRenderer -- (RendererConfig AcceleratedRenderer True)
  -- imgBrick <- loadBMP "brick.bmp"
  -- texture <- createTextureFromSurface r imgBrick
  -- copy r texture Nothing Nothing 
  mainLoop (State (V2 5 1) r False [] 0 True 0 nol noc)
