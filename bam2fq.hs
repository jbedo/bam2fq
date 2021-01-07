{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bio.Bam
import Bio.Bam.Rec (isReversed)
import Bio.Prelude (Qual, unQ, IOMode(..), Nucleotide(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Log
import Data.Bits
import Data.Function
import Data.Maybe
import Data.Word
import Options.Applicative
import Prelude hiding (Read)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Streaming.Char8 as SB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as V
import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)
import Streaming.With
import Streaming.Zip (gzip, bestSpeed)
import System.Environment

data Args = Args {
  bam :: FilePath
  ,fq1 :: FilePath
  ,fq2 :: FilePath
} deriving (Eq, Ord, Show)

args :: Parser Args
args =
  Args <$> argument str (metavar "BAM" <> help "input name sorted BAM file")
       <*> argument str (metavar "FASTQ1" <> help "output fastq path (read set 1)")
       <*> argument str (metavar "FASTQ2" <> help "output fastq path (read set 2)")
      

data Read = Read {
  name :: BS.ByteString
  ,qual :: V.Vector Qual
  ,seq :: Vector_Nucs_half Nucleotides
                 }
  deriving (Show)

type PairedRead = Of Read Read

fmtFastq :: MonadIO m => Bool -> Stream (Of PairedRead) m  () -> SB.ByteString m ()
fmtFastq second a = do
  n <- lift $ S.next a
  case n of
    Right ((Read n q s) :> (Read n' q' s'), a') -> do
      SB.string "@"
      SB.fromStrict n
      if second then
        SB.string "/2\n"
      else
        SB.string "/1\n"
      SB.string $ show $ if second then s' else s
      SB.string "\n+\n"
      (if second then q' else q)
        & V.toList
        & each
        & S.map unQ
        & S.map (toEnum . fromIntegral . (+0x21))
        & SB.pack
      SB.string "\n"
      fmtFastq second a'
    _ -> return ()

match :: MonadIO m => Stream (Of BamRec) m () -> Stream (Of PairedRead) m ()
match a = do
  n <- lift $ S.next a
  case n of
    Right (a, a') -> match' a a'
    _ -> return ()
  where
    match' a b = do
      n <- lift $ S.next b
      case n of
        Right (b, b') ->
          if b_qname a == b_qname b && isJust (b_qual a) && isJust (b_qual b) then do
            S.yield $ Read (b_qname a) (qual a) (seq a) :> Read (b_qname a) (qual a) (seq b)
            match b'
          else
            match' b b'
        _ -> return ()
    seq a =
      if isReversed a then
        rc $ b_seq a
      else
        b_seq a
    qual a =
      if isReversed a then
        V.reverse $ fromJust $ b_qual a
      else
        fromJust $ b_qual a
    rc = VG.map (\(Ns ns) -> Ns . revBits 4 0 $ ns) . VG.reverse
      where
        revBits 0 c _ = c
        revBits k c w = revBits (k-1) ((c `shiftL` 1) .|. (w .&. 1)) (w `shiftR` 1)

main = process =<< execParser (info (args <**> helper)
    (fullDesc <> progDesc "Unpacks a name-sorted BAM file into paired-end fastqs"
              <> header "bam2fq")
  )

process (Args path fq1 fq2) =
  withBinaryFile fq1 WriteMode $ \fq1 ->
    withBinaryFile fq2 WriteMode $ \fq2 ->
      withLogging_ (LoggingConf Warning Error Error 10 True) $
        decodeBamFiles [path] (\[(hdr, reads)] -> S.map unpackBam reads & match & S.copy
                                & fmtFastq False & gzip bestSpeed & SB.hPut fq1
                                & fmtFastq True & gzip bestSpeed & SB.hPut fq2)
