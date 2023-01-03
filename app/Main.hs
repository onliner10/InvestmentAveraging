module Main (main) where
import Adapter (getData)

main :: IO ()
main = do
    parseResult <- getData
    case parseResult of
      Left err ->
          print err
      Right x ->
          print x
