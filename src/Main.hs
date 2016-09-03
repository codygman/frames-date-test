{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators,
             ViewPatterns #-}

module Main where


import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Proxy (Proxy(..))
import Lens.Micro
import Lens.Micro.Extras
import Frames
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P

-- A few other imports will be used for highly customized parsing [[Better Types][later]].

import Frames.CSV (colQ)

tableTypes "User" "user.csv"

userStream :: Producer User IO ()
userStream = readTableOpt userParser "user.csv"

loadUsers :: IO (Frame User)
loadUsers = inCoreAoS userStream

printUsers = mapM_ print . F.toList =<< loadUsers


main :: IO ()
main = do
  putStrLn "hello world"
