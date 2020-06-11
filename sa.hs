import Data.Foldable
import System.Random
import Control.Monad
import Data.Sequence hiding (replicateM,length)
import System.IO.Unsafe
import System.IO
import Data.Char
import System.Environment

showFunctionsID :: IO ()
showFunctionsID = do putStrLn "0 DeJong, 1 Elipsoid, 2 Schwefel, 3 Rastrigin"

rastrigin :: Int -> [Double] -> Double
rastrigin 0 arr = ((fromIntegral 10) * (fromIntegral (length arr))) + ((arr !! 0) ^^ 2) - (fromIntegral 10) * (cos ((fromIntegral 2) * pi * (arr !! 0))) 
rastrigin n arr = ((arr !! n) ^^ 2) - (fromIntegral 10) * (cos ((fromIntegral 2) * pi * (arr !! n))) + rastrigin (n-1) arr

schwefel :: Int -> [Double] -> Double
schwefel 0 arr = (-1) * (arr !! 0) * sin (sqrt (abs (arr !! 0)))
schwefel n arr = ((-1) * (arr !! n) * sin (sqrt (abs (arr !! n)))) + schwefel (n-1) arr

elipsoid :: Int -> [Double] -> Double
elipsoid 0 arr = (arr !! 0) ^^ 2
elipsoid n arr = ((fromIntegral (n+1)) * ((arr !!n) ^^ 2) ) + elipsoid (n-1) arr

deJong :: Int -> [Double] -> Double
deJong 0 arr = (arr !! 0 ) ^^ 2   
deJong n arr = ((arr !! n) ^^ 2)  + deJong (n-1) arr 

selectFunc :: Int -> Int -> [Double] -> Double
selectFunc 0 dim arr = deJong dim arr
selectFunc 1 dim arr = elipsoid dim arr
selectFunc 2 dim arr = schwefel dim arr
selectFunc 3 dim arr = rastrigin dim arr

bitArrayToNumber :: Int ->[Bool] -> Int
bitArrayToNumber  0 arr = fromEnum (arr !! 0) * 2 ^ 0 
bitArrayToNumber n arr = fromEnum (arr !! n) * (2 ^ n) + bitArrayToNumber (n-1) arr 

bitGenToDoubleArray :: Int -> [[Bool]] -> [Double]
bitGenToDoubleArray 0 arr = bigNumberToInterval (bitArrayToNumber 31 (arr !! 0)) : []
bitGenToDoubleArray n arr = bigNumberToInterval (bitArrayToNumber 31 (arr !! n)) : bitGenToDoubleArray (n - 1) arr 

generateGeneration :: Int -> IO [[Bool]]
generateGeneration n = replicateM n (generateRandomBitArray 32)   

bigNumberToInterval :: Int -> Double
bigNumberToInterval nr = (flt / 2^32) * 10.24 - 5.12 
      where flt = fromIntegral nr :: Double

generateRandomBitArray :: Int -> IO [Bool]
generateRandomBitArray n = replicateM n randomIO

selectVecin :: Int -> Int -> [[Bool]] -> [[Bool]]
selectVecin i j arr = toList (update i (replaceNth j (arr !! i)) $ fromList arr ) 

replaceNth :: Int -> [Bool] -> [Bool]
replaceNth _  [] = []
replaceNth n arr = toList (update n (not ( arr !! n ) ) $ fromList arr)


simmulatedGen :: Int -> [[Bool]] -> [[Bool]] ->Int -> Double -> Int -> [[Bool]]
simmulatedGen functie seed minim _ _ 0 = minim
simmulatedGen functie seed minim dimensiuni t iteratii =
       let i = unsafePerformIO $ randomRIO (0,dimensiuni - 1 :: Int)
           j = unsafePerformIO $ randomRIO (0,31:: Int)
           vecin = selectVecin i j seed
           valoareVecin = selectFunc functie dimensiuni (bitGenToDoubleArray dimensiuni vecin)
           valoareMinim = selectFunc functie dimensiuni (bitGenToDoubleArray dimensiuni minim)
           percentage = unsafePerformIO $ randomIO :: Double in 
    if valoareVecin < valoareMinim then simmulatedGen functie seed vecin dimensiuni t (iteratii - 1) else if t > percentage then simmulatedGen functie seed vecin dimensiuni t (iteratii - 1) else
                                            simmulatedGen functie seed minim dimensiuni t (iteratii - 1)


simulatedAnnealing ::Int -> [[Bool]] -> Int -> Double -> Double -> Double -> Int -> [Double]
simulatedAnnealing functie minim dimensiuni t tMin alpha iteratii =
 let candidat = simmulatedGen functie minim minim dimensiuni t iteratii
     valCandidat = selectFunc functie dimensiuni (bitGenToDoubleArray dimensiuni candidat) in 
 if t < tMin then bitGenToDoubleArray dimensiuni minim else
            simulatedAnnealing functie candidat dimensiuni (t * alpha) tMin alpha iteratii

bucla ::Int -> [[[Bool]]] -> Int -> Int -> Double -> Double -> Double -> Int -> [[Double]]
bucla functie arr 0 dimensiuni t tMin alpha iteratii = simulatedAnnealing functie (arr !! 0) dimensiuni t tMin alpha iteratii : []
bucla functie arr n dimensiuni t tMin alpha iteratii = simulatedAnnealing functie (arr !! n) dimensiuni t tMin alpha iteratii : bucla functie arr (n-1) dimensiuni t tMin alpha iteratii

createArr :: Int ->Int ->IO [[[Bool]]]
createArr n dimensiuni= replicateM n (generateGeneration dimensiuni)

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs

printArr :: String ->Int ->[Double] -> IO ()
printArr file 0 arr = do appendFile file $ show (arr !! 0)
                         appendFile file "\n"
printArr file dim arr = do appendFile file $ show (arr !! dim)
                           appendFile file ","
                           printArr file (dim-1) arr


printResult :: String->Int-> Int ->Int -> [[Double]] -> IO ()
printResult file dimensiuni 0 functie indivizi = do appendFile file $ show (selectFunc functie dimensiuni (indivizi !! 0))
                                                    appendFile file ", ,"
                                                    printArr file dimensiuni (indivizi !! 0)
printResult file dimensiuni length functie indivizi = do appendFile file $ show (selectFunc functie dimensiuni (indivizi !! length))
                                                         appendFile file ", ,"
                                                         printArr file dimensiuni (indivizi !! length)
                                                         printResult file dimensiuni (length-1) functie indivizi
 

main = do 
let dimensiuni = 5
let t = 1000 :: Double
let tMin = 0.001 :: Double
let alpha = 0.9 :: Double
let iteratii = 120
let repetari = 2
let file = "PFresults1110.csv"
showFunctionsID
functie <- getIntArg
print functie
if functie == 0 then
  appendFile file "Functia DeJonj cu " 
  else
      if functie == 1 then                
      appendFile file "Functia Elipsoid cu " 
      else
         if(functie==2) then
         appendFile file "Functia Schwefel cu " 
         else
             appendFile file "Functia Rastrigin cu "
appendFile file $ show dimensiuni
appendFile file " dimensiuni \n"         
arr <- createArr repetari dimensiuni
let minime = bucla functie arr (repetari-1) (dimensiuni-1) t tMin alpha iteratii
printResult file (dimensiuni-1) (repetari-1) functie minime
return ()
