-- Util.hs
module Util where

import System.IO.Unsafe

inputRaw :: String -> String
inputRaw fileName = do
  let contents = unsafePerformIO $ readFile fileName
  take (length contents - 1) contents
