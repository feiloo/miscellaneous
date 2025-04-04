{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Lib

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Char(ord)
import Data.Word
import Data.Binary.Get
import Data.List
import Data.List.Split
import qualified Codec.Compression.GZip as GZip
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPutStr)
import System.Random

max_readname_len :: Integer
max_readname_len = 68

-- @A00684:330:HJW2GDRX3:1:1101:3586:1000 1:N:0:TGTAACCACT+TGGAGCGATT

--splitByAt :: B.ByteString -> [B.ByteString]
-- splitByAt = B.split (Data.Word8 '@')

data ReadBlock = ReadBlockBuild {
         seqname :: B.ByteString
        , base_sequence :: B.ByteString
        , seqname2 :: Maybe B.ByteString
        , qual :: B.ByteString
        } deriving (Show)

splitToReads :: B.ByteString -> [B.ByteString]
splitToReads = B.split (fromIntegral.ord $ '@')

-- symbol = fromIntegral.ord $ '@'
symbol = (C.pack ['a'])

dofn1 = do
    withFile "example.txt.gz" ReadMode $ \handle -> do
        compressedContent <- B.hGetContents handle
        let decompressedContent = GZip.decompress compressedContent
        let splitV = splitToReads decompressedContent
        mapM_ B.putStr splitV

dofn2 = do 
        let sym = (C.pack ['a'])
        let readBlock = ReadBlockBuild {
            seqname = sym, 
            base_sequence = sym, 
            seqname2 = Just $ sym, 
            qual = sym
        }
        putStr (show readBlock)

readStrToRead :: B.ByteString -> Maybe ReadBlock
readStrToRead readStr = 
        let fields = B.split (fromIntegral.ord $ '\n') readStr in
        case fields of
        (a:b:c:d) ->
            Just ReadBlockBuild { 
                seqname = a,
                base_sequence = b,
                seqname2 = Just $ c,
                qual = d !! 0
            }
        _ -> Nothing

pSeqname sn =
        case sn of 
                Just name -> name
                _ -> C.pack []

printRead :: Maybe ReadBlock -> [Char]
printRead readb =
        case readb of 
                Just read ->
                        "@" ++ (C.unpack (seqname read)) ++ "\n" ++ (C.unpack (base_sequence read)) ++ "\n" ++ (C.unpack (pSeqname (seqname2 read))) ++ "\n" ++ (C.unpack (qual read))
                _ -> []



dofn3 = do
    withFile "example.txt.gz" ReadMode $ \handle -> do
        compressedContent <- B.hGetContents handle
        let decompressedContent = GZip.decompress compressedContent
        let splitV = splitToReads decompressedContent
        let read1 = ReadBlockBuild { 
            seqname = symbol, 
            base_sequence = symbol, 
            seqname2 = Just $ (symbol),
            qual = symbol
        }
        --putStr read1
        withFile "out.txt" WriteMode $ \fHandle -> do
            (hPutStr fHandle) (show read1)

top::Integer
top = 100

bottom::Integer
bottom=0

dofn4 = do
    withFile "example.txt.gz" ReadMode $ \handle -> do
        compressedContent <- B.hGetContents handle
        let decompressedContent = GZip.decompress compressedContent
        let splitV = splitToReads decompressedContent
        --putStr read1
        let read1 = map readStrToRead splitV
        let rngen = mkStdGen 1
        let mr =  (map (\(a,b) -> b) (filter (\(a, b) -> a < 10) (zip (randomRs (bottom, top) rngen) read1)))
        let shows = intercalate "\n" (map show mr)
        withFile "out.txt" WriteMode $ \fHandle -> do
            (hPutStr fHandle) shows

dofn5 = do
        content <- getContents
        let splitV = splitToReads (C.pack content)
        --putStr read1
        let read1 = map readStrToRead splitV
        let rngen = mkStdGen 1
        let mr =  (map (\(a,b) -> b) (filter (\(a, b) -> a < 25) (zip (randomRs (bottom, top) rngen) read1)))
        let shows = intercalate "\n" (map printRead mr)
        putStr shows
        --withFile "out.txt" WriteMode $ \fHandle -> do
        --    (hPutStr fHandle) shows

        
readblocks :: B.ByteString -> [B.ByteString]
readblocks {-# UNBOX #-} !content = 
        if B.length content < 28
        then []
        else
        let blocksize_bytes = fromIntegral (runGet ( getWord16le ) (do B.take 2 ( B.drop 16 content))) in
        B.take (blocksize_bytes + 1) content : (readblocks (B.drop (blocksize_bytes + 1) content))


dofn6 = do
    withFile "example.txt.gz" ReadMode $ \handle -> do
    --withFile "two.gz" ReadMode $ \handle -> do
        compressedContent <- B.hGetContents handle
        --let blocks = filter (\a -> a /= []) (readblocks compressedContent)
        let blocks =  (readblocks compressedContent)
        --B.putStr (C.unwords blocks)
        B.putStr (B.intercalate B.empty blocks)
        --let decompressedContent = GZip.decompress (head blocks)
        -- putStrg (printBlock block)
        --putStr (show blocks)
        --putStr (show decompressedContent)
dofn7 = do
    withFile "test.gz" ReadMode $ \handle -> do
    --withFile "two.gz" ReadMode $ \handle -> do
        compressedContent <- B.hGetContents handle
        --let blocks = filter (\a -> a /= []) (readblocks compressedContent)
        let blocks = map GZip.decompress (readblocks compressedContent)
        putStr (C.unpack (C.unlines blocks))
        --B.putStr (C.unwords blocks)
        --B.putStr (B.intercalate B.empty blocks)


main :: IO ()
main = dofn7
