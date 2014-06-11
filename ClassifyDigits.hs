import           Data.Ord
import           Data.Vector ((!))
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Csv
import qualified Data.ByteString.Lazy as L

data LabelPixels =
  LP { lpLabel :: Int, lpPixels :: Vector Int }

slurpFile :: FilePath -> IO (Vector LabelPixels)
slurpFile fp =
  do bytes <- L.readFile fp
     case decode HasHeader bytes of
       Left err -> error err
       Right rows -> return (V.map makeRow rows)
  where makeRow row = LP (row ! 0) (V.drop 1 row)

distance :: Vector Int -> Vector Int -> Double
distance p1 p2 =
  sqrt (fromIntegral (V.sum (V.zipWith (\a b -> let d = a - b
                                                in d * d)
                                       p1 p2)))

classify :: Vector LabelPixels -> Vector Int -> Int
classify trainingSet pixels =
  fst (V.minimumBy (comparing snd)
                   (V.map (\x -> (lpLabel x,distance pixels (lpPixels x)))
                          trainingSet))

main :: IO ()
main =
  do putStrLn "Start ..."
     trainingSet <- slurpFile "trainingsample.csv"
     validationSample <- slurpFile "validationsample.csv"
     let numCorrect =
           V.sum (V.map (\p -> if classify trainingSet (lpPixels p) == lpLabel p
                                  then 1
                                  else 0)
                        validationSample)
         percent = numCorrect / fromIntegral (V.length validationSample) * 100
     putStrLn ("Percentage correct: " ++ show percent)
