{-# LANGUAGE QuasiQuotes #-}

-- | A simple synthesizer
--
-- The 'runSynth' method requires GCC to be installed and the ALSA C library.

module Main where



import qualified Prelude as P
import Control.Arrow
import qualified Data.Char as Char

import Feldspar
import Feldspar.Software

import Feldspar.Synch.System
import Feldspar.Synch

import ALSA



--------------------------------------------------------------------------------
-- * Non-blocking keyboard reading
--------------------------------------------------------------------------------

getch_def = [cedecl|
// Non-blocking version of getchar(). Returns 0 when no character has been
// typed.
char getch()
{
    char c = 0;
    int i;
    ioctl(0, FIONREAD, &i);
    if(i) {c = getchar();}
    return c;
}
|]

getch :: Software (Data Word8)
getch = do
    addInclude "<stdio.h>"
    addInclude "<sys/ioctl.h>"
    addDefinition getch_def
    callFun "getch" []



--------------------------------------------------------------------------------
-- * Synthesizer
--------------------------------------------------------------------------------

-- | 0 means A, 3 means C, etc.
type Key = Data Word8

type Frequency = Data Double

claviature = "zsxdcvgbhnjm,"

-- | Map a character to the corresponding key
interpretChar :: Data Word8 -> Key
interpretChar = switch 0 table
  where
    cs    = P.map (P.fromIntegral . Char.ord) claviature
    table = P.zip cs (P.map value [3..])  -- C has key 3

-- | Map a key to the corresponding frequency. Frequency 0 maps to key 0.
interpretKey :: Key -> Frequency
interpretKey k = i2n (b2i (k>0) :: Data Word8) * 440 * interval k
  where
    b2i b = b ? 1 $ 0 -- TODO
    interval k = 2 ** (i2n k/12)

-- | Compute the step angle corresponding to the given wave frequency at the
-- given sample rate
stepAngle :: Frequency -> Data Double
stepAngle freq = 2*pi*freq/sampleRate

-- | Generate a sine wave of the given frequency at the give sample rate
genSine :: MonadComp m => Synch m Frequency (Data Double)
genSine = arr stepAngle >>> cycleStep 0 (2*pi) >>> arr sin

bufferLength = 25000  -- Sound device buffer length, 25ms
periodLength = 3000   -- Chunk length (approximate main loop period), 3ms
holdTime     = 100    -- 100 cycles = 300ms

synth :: ALSA -> PCM -> Data Length -> Synch Software () ()
synth alsa pcm n
    =   arrSource getch
    >>> holdPred (/=0) holdTime
    >>> arr interpretChar
    >>> arr interpretKey
    >>> chunk n (genSine >>> arr distort >>> arr quantize)
    >>> arrProg (writePCM alsa pcm)
  where
    distort x = sign * x * x
      where sign = x>=0 ? 1 $ (-1)  -- because `signum` doesn't work

synthMain :: Software ()
synthMain = do
    alsa@(ALSA {..}) <- importALSA
    pcm <- newPCM
    n   <- initPCM pcm Playback 1 bufferLength periodLength
    execSystem $ runSynch $ synth alsa pcm n

runSynth = runCompiled'
    defaultExtCompilerOpts {externalFlagsPost = ["-lm","-lasound"]}
    synthMain

main = icompile synthMain

