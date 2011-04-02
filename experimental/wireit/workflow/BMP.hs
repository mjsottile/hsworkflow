{-# OPTIONS_GHC -fglasgow-exts #-}
{- 
    The BMP module is a hack to allow the reading and writing of
    a particular type of bitmap file.
    
    The saveBitmap function relies upon being given a valid BMP header
    that is usually obtained via a previous loadBitmap call.

    A better implementation would allow this header to be generated.

    This code is based upon the bitmap loader in:
        FunGEn - Functional GameEngine
        http://www.cin.ufpe.br/~haskell/fungen
        Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

-}

module BMP where -- (loadBitmap,saveBitmap,parseBitmap,formatBitmap) where

import IO
import System.IO
import Data.Word
import Data.Int
import Data.Array
import Data.Array.IO
import System.Directory
import System.FilePath
import Control.Monad.State

type BMPubyte = Word8
type BMPdouble = Double
type BMPsizei = Int32
type BMPHeader = String
type PixelDescriptor = [Color4]
data Color4 = Color4 !BMPubyte !BMPubyte !BMPubyte !BMPubyte
type ColorList3 = [(BMPubyte, BMPubyte, BMPubyte)]         -- color in RGB format
type Bitmap = (BMPsizei, BMPsizei, BMPHeader, PixelDescriptor)   -- width, height and data of bitmap
type BmpList = [(BMPubyte, BMPubyte, BMPubyte, BMPubyte)]

binAux :: String
binAux = "000000000000000000000000"

parseBitmap :: (MonadIO m) => String -> Maybe ColorList3 -> m Bitmap
parseBitmap bmString invList = do
                    let bmMagic = getI2 $ bmString
                    let bmSize = getI4 $ dropBMPsizei 2 bmString
                    let bmOffset = getI4 $ dropBMPsizei 10 bmString
                    let (bmW,bmH) = getWH (dropBMPsizei 18 bmString)
                    liftIO $ putStrLn $ "bmW=" ++ (show bmW)
                    liftIO $ putStrLn $ "bmH=" ++ (show bmH)
                    liftIO $ putStrLn $ "bmMagic=" ++ (show bmMagic)
                    liftIO $ putStrLn $ "bmSize=" ++ (show bmSize)
                    liftIO $ putStrLn $ "bmOffset=" ++ (show bmOffset)
                    let content = (dropBMPsizei 54 bmString)
                    liftIO $ putStrLn $ "content len=" ++ (show (length content))
                    let bmData = getBmData content (bmW,bmH) invList
                    return (bmW,bmH,(take 54 bmString),bmData)

loadBitmap :: FilePath -> Maybe ColorList3 -> IO Bitmap
loadBitmap bmName invList = do
                    bmFile <- openBinaryFile bmName (ReadMode)
                    bmString <- hGetContents bmFile
                    result <- parseBitmap bmString invList
                    hClose bmFile
                    return result



-- saves a bitmap to a string
formatBitmap :: (MonadIO m) => Bitmap -> m String
formatBitmap (bmW,bmH,bmHeader,bmData) = do
                    let bmString = putBmData bmData
                    return $ bmHeader ++ bmString

-- saves a bitmap from a file
saveBitmap :: FilePath -> Bitmap -> IO ()
saveBitmap bmName (bmW,bmH,bmHeader,bmData) = do
                    putStrLn $ "saveBitmap " ++ bmName
                    bmFile <- openBinaryFile bmName (WriteMode)
                    let bmString = putBmData bmData
                    hPutStr bmFile $ bmHeader ++ bmString
                    hClose bmFile
                    return ()

getBmData :: String -> (BMPsizei,BMPsizei) -> Maybe ColorList3 -> PixelDescriptor
getBmData bmString (bmW,bmH) invList =
        let
            colorList = makeColorList bmString (bmW,bmH)
            bmData = [Color4 r g b a | (r,g,b,a) <- addInvisiblity colorList invList]
        in
            bmData  


putBmData :: PixelDescriptor -> String
putBmData [] = []
putBmData ( (Color4 r g b a):c4s ) = (chr2 b):(chr2 g):(chr2 r):(putBmData c4s)


addInvisiblity :: ColorList3 -> Maybe ColorList3 -> BmpList
addInvisiblity [] _ = []
addInvisiblity l Nothing = map (\(r,g,b) -> (r,g,b,255)) l
addInvisiblity ((r,g,b):as) i@(Just invList) | (r,g,b) `elem` invList = ((r,g,b,0):(addInvisiblity as i))
                                             | otherwise = ((r,g,b,255):(addInvisiblity as i))
                                             
makeColorList :: String -> (BMPsizei,BMPsizei) -> [(BMPubyte, BMPubyte, BMPubyte)]
makeColorList bmString (bmW,bmH) = makeColorListAux (bmW `mod` 4) bmString (bmW*bmH) (bmW,bmW)
                        
makeColorListAux :: BMPsizei -> String -> BMPsizei -> (BMPsizei,BMPsizei) -> [(BMPubyte, BMPubyte, BMPubyte)]
makeColorListAux _ _ 0 _ = []
makeColorListAux x bmString totVert (0,bmW) = makeColorListAux x (dropBMPsizei x bmString) totVert (bmW,bmW)
makeColorListAux x (b:g:r:bmString) totVert (n,bmW) = (ord2 r,ord2 g,ord2 b): (makeColorListAux x bmString (totVert - 1) (n - 1,bmW))
makeColorListAux _ z _ zz = error $ "Fun_Loader.makeColorListAux error: strange bitmap file. z=" ++
                                        (show (length z)) ++ "/" ++ (show (zz))


toDecimal :: String -> BMPsizei
toDecimal a = toDecimalAux (reverse a) 32

toDecimalAux :: String -> BMPsizei -> BMPsizei
toDecimalAux [] _ = 0
toDecimalAux _ 0 = 0
toDecimalAux (a:as) n
                | a == '0' = toDecimalAux as (n-1)
                | otherwise = pow2 (32 - n) + toDecimalAux as (n-1)

toBinary :: Int -> String
toBinary n
        | n < 2 = show n
        | otherwise = toBinary (n `div` 2) ++ (show (n `mod` 2))
        
ord2 :: Char -> BMPubyte
ord2 a = (toEnum.fromEnum) a
        
chr2 :: BMPubyte -> Char
chr2 a = toEnum $ fromEnum $ fromIntegral $ a

pow2 :: BMPsizei -> BMPsizei
pow2 0 = 1
pow2 n = 2 * pow2(n-1)


shiftLeft :: String -> Int -> String
shiftLeft a 0 = a
shiftLeft (_:as) n = shiftLeft(as ++ "0") (n-1)
shiftLeft _ _ = []

make0 :: Int -> String
make0 0 = []
make0 n = '0':(make0 (n-1))


dropBMPsizei                :: BMPsizei -> [a] -> [a]
dropBMPsizei 0 xs            = xs
dropBMPsizei _ []            = []
dropBMPsizei n (_:xs) | n>0  = dropBMPsizei (n-1) xs
dropBMPsizei _ _ = error "Fun_Aux.dropBMPsizei error: negative argument"

getWH :: String -> (BMPsizei,BMPsizei)
getWH (a:b:c:d:e:f:g:h:_) =
        ( (op (bin a) 0) + (op (bin b) 8) + (op (bin c) 16) + (op (bin d) 24),
                 (op (bin e) 0) + (op (bin f) 8) + (op (bin g) 16) + (op (bin h) 24))
                 where bin x = toBinary(fromEnum x)
                       op x n = toDecimal(shiftLeft(binAux ++ (make0 (8 - (length x)) ++ x)) n)

getI2 :: String -> BMPsizei
getI2 (a:b:_) =
        (op (bin a) 0) + (op (bin b) 8)
                 where bin x = toBinary(fromEnum x)
                       op x n = toDecimal(shiftLeft(binAux ++ (make0 (8 - (length x)) ++ x)) n)


getI4 :: String -> BMPsizei
getI4 (a:b:c:d:_) =
            (op (bin a) 0) + (op (bin b) 8) + (op (bin c) 16) + (op (bin d) 24)
                 where bin x = toBinary(fromEnum x)
                       op x n = toDecimal(shiftLeft(binAux ++ (make0 (8 - (length x)) ++ x)) n)


printASCIIArtRow :: [Color4] -> IO ()
printASCIIArtRow [] =
    do
        return ()

printASCIIArtRow ( (Color4 r g b a):ds ) =
    do
        let sum = (fromIntegral b) --  + (fromIntegral g) + (fromIntegral b)
        putStr $ take 4 $ " " ++ (show sum) ++ "     "
        printASCIIArtRow ds

printASCIIArt :: Int -> Int -> [Color4] -> IO ()
printASCIIArt _ 0 _ =
    do
        putStrLn ""
        return ()

printASCIIArt w h d =
    do
        printASCIIArtRow $ take w d
        putStrLn ""
        printASCIIArt w (h-1) (drop w d)


fixname path =
    let
        lname = dropExtension( path )
    in
        lname ++ ("NEW.bmp")

xmain =
    do
        let fname = "./data/images/i2.bmp"
        putStrLn $ "Start program. Opening file: " ++ fname
        (bmW,bmH,bmHeader,bmData) <- loadBitmap fname Nothing
        putStrLn $ "Width=" ++ (show bmW)
        putStrLn $ "Height=" ++ (show bmH)
        putStrLn $ "Length=" ++ (show (length bmData))
        printASCIIArt (fromIntegral bmW) (fromIntegral bmH) bmData
        let newBM = (bmW,bmW,bmHeader,bmData)
        saveBitmap (fixname fname) newBM
        putStrLn "All done"
