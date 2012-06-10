module Util
    ( readMaybe
    ) where

-- Taken from:
-- http://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe/8080084#8080084
readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing
