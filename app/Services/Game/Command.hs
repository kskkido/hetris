module Services.Game.Command
  ( handleEventKey
  ) where

import Graphics.Gloss.Interface.Pure.Game
  ( Event(..)
  , Key(..)
  , KeyState(..)
  )

import Models.Piece
  ( Piece
  , moveL
  , moveR
  , rotate
  )

import Models.Matrix
  ( Matrix(..)
  , hardDrop
  , command
  , commandable
  )

import Models.Game
  ( Game
  , put
  )

handleEventKey :: Event -> Game -> Game
handleEventKey (EventKey (Char 'w') Down _ _) = put $ command rotate
handleEventKey (EventKey (Char 'a') Down _ _) = put $ command moveL
handleEventKey (EventKey (Char 'd') Down _ _) = put $ command moveR
handleEventKey (EventKey (Char 's') Down _ _) = put hardDrop
handleEventKey _ = id
