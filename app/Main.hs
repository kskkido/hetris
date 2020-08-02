module Main where

import Graphics.Gloss
  ( Display(..)
  , white
  )

import Graphics.Gloss.Interface.Pure.Game
  ( play
  )

import Models.Game
  ( create
  )

import Services.Game.Command
  ( handleEventKey
  )

import Services.Game.Frame
  ( next
  )

import Views.Game
  ( render
  )

main :: IO ()
main = play
       (InWindow "hetris" (400, 800) (200,600))
       white
       6
       (create 10 20)
       (render (20,20))
       handleEventKey
       next
