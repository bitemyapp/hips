{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Char8 (hGet, hPut)
import System.IO
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Error
import Data.Bits
import Control.Applicative
import Data.Functor.Syntax ((.:))
import System.Directory
import System.Environment
import System.Exit

data InOut = InOut { hIn ∷ Handle
                   , hOut ∷ Handle }
type Patcher = ReaderT InOut (ErrorT String IO)

bsToInt ∷ ByteString → Int
bsToInt = S.foldl (\acc x → acc `shiftL` 8 + fromIntegral x) 0

get ∷ Int → Patcher ByteString
get l = do
    h ← asks hIn
    liftIO $ hGet h l

getInt ∷ Int → Patcher Int
getInt = bsToInt .: get

put ∷ Int → ByteString → Patcher ()
put o s = do
    h ← asks hOut
    liftIO $ do
        hSeek h AbsoluteSeek $ toInteger o
        hPut h s

patchLoop ∷ Patcher ()
patchLoop = do
    tag ← get 3
    unless (tag == "EOF") $ do
        s ← getInt 2
        v ← if s /= 0 then get s else do
            r ← getInt 2
            S8.replicate r . S8.head <$> get 1
        put (bsToInt tag) v
        patchLoop

patch ∷ Patcher ()
patch = do
    header ← get 5
    unless (header == "PATCH") $ throwError "bad header"
    patchLoop

failWith ∷ IO α → IO β
failWith a = a >> (exitWith =<< exitFailure)

printHelp ∷ IO ()
printHelp = do
    exe ← getProgName
    putStrLn $ "usage: " ++ exe ++ " patchfile datafile newfile"

main ∷ IO ()
main = do
    args ← getArgs
    unless (length args == 3) $ failWith printHelp
    let [patchFile, dataFile, newFile] = args
    hPatch ← openBinaryFile patchFile ReadMode
    copyFile dataFile newFile
    hFile ← openBinaryFile newFile ReadWriteMode
    result ← runErrorT $ runReaderT patch InOut { hIn  = hPatch, hOut = hFile }
    mapM_ hClose [hPatch, hFile]
    case result of
        Left e → failWith $ putStrLn e
        _ → putStrLn "done"
