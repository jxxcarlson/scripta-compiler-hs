{-# LANGUAGE OverloadedStrings #-}


module Parser.Meta (Meta, dummy) where

import Prelude hiding(id)


data Meta =
    Meta { begin :: Int, end :: Int, index :: Int, id :: String }


dummy =
   Meta { begin = 0, end = 0, index = 0, id = "dummyId" }

