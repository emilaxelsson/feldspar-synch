{-# LANGUAGE QuasiQuotes #-}

-- | A simple synthesizer
--
-- The 'runSynth' method requires GCC to be installed and the ALSA C library.

module Main where



import qualified Prelude as P
import Control.Arrow
import qualified Data.Char as Char

import Feldspar.Run
import Feldspar.Validated

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

getch :: Run (Event (Data Word8))
getch = do
    addInclude "<stdio.h>"
    addInclude "<sys/ioctl.h>"
    addDefinition getch_def
    c <- callFun "getch" []
    return $ eventWhen (c/=0) c



--------------------------------------------------------------------------------
-- * Synthesizer
--------------------------------------------------------------------------------

bufferLength = 25000  -- Sound device buffer length, 25ms
periodLength = 3000   -- Chunk length (approximate main loop period), 3ms
holdTime     = 100    -- 100 cycles = 300ms

claviature = "zsxdcvgbhnjm,"

-- | Number of parallel channels
nPoly = P.length claviature

-- | 0 means A, 3 means C, etc.
type Key = Data Word8

type Frequency = Data Double

-- | Map a character to the corresponding key
interpretChar :: Data Word8 -> Key
interpretChar = switch 0 table
  where
    cs    = P.map (P.fromIntegral . Char.ord) claviature
    table = P.zip cs (P.map value [3..])  -- C has key 3

-- | Map a key to the corresponding frequency
interpretKey :: Key -> Frequency
interpretKey k = 440 * interval k
  where
    interval k = 2 ** (i2n k/12)

-- | Generate 'Frequency' events from 'Key' events. The number of 'Frequency'
-- events for each 'Key' event is given by 'holdTime'.
key :: MonadComp m => Synch m (Event Key) (Event Frequency)
key = holdEvent holdTime >>> arr (fmap interpretKey)

-- | Polyphonic version of 'key'. Each element in the output is tied to a
-- specific frequency. Each input event generates events on the channel given by
-- the input 'Key'.
keyPoly :: MonadComp m => Synch m (Event Key) [Event Frequency]
keyPoly = parSplit nPoly keyFilter (const key)
  where
    keyFilter i ke = ke >>= \k -> eventWhen (value (P.fromIntegral i + 3) == k) k

-- | Compute the step angle corresponding to the given wave frequency at the
-- specified sample rate
stepAngle :: Frequency -> Data Double
stepAngle freq = 2*π*freq/sampleRate

-- | Generate a sine wave of the given frequency at the specified sample rate
genSine :: MonadComp m => Synch m Frequency (Data Double)
genSine = arr stepAngle >>> cycleStep 0 (2*π) >>> arr sin >>> arr distort
  where
    distort x = signum x * x * x

-- | Event-controlled sine wave. The wave only advances when there's an event,
-- and the output is 0 when there is no event.
genSineE :: MonadComp m => Synch m (Event Frequency) (Data Double)
genSineE = liftEvent genSine >>> arr (\a -> fromValidated a 0)

-- | Polyphonic version of 'genSineE'. It runs a number of 'genSineE' networks
-- in parallel, and sums their output.
genSinePolyE :: MonadComp m => Synch m [Event Frequency] (Data Double)
genSinePolyE = parList nPoly (const genSineE) >>> arr (P.foldr (+) 0)

-- | Monophonic synth
synth :: ALSA -> PCM -> Data Length -> Synch Run () ()
synth alsa pcm n
    =   arrSource getch
    >>> arr (fmap interpretChar)
    >>> key
    >>> chunk n (genSineE >>> arr quantize)
    >>> arrProg (writePCM alsa pcm)

-- | Polyphonic synth
synthPoly :: ALSA -> PCM -> Data Length -> Synch Run () ()
synthPoly alsa pcm n
    =   arrSource getch
    >>> arr (fmap interpretChar)
    >>> keyPoly
    >>> chunk n (genSinePolyE >>> arr (*0.17) >>> arr quantize)
    >>> arrProg (writePCM alsa pcm)

synthMain :: Run ()
synthMain = do
    alsa@(ALSA {..}) <- importALSA
    pcm <- newPCM
    n   <- initPCM pcm Playback 1 bufferLength periodLength
    execSystem $ runSynch $ synth alsa pcm n

synthPolyMain :: Run ()
synthPolyMain = do
    alsa@(ALSA {..}) <- importALSA
    pcm <- newPCM
    n   <- initPCM pcm Playback 1 bufferLength periodLength
    execSystem $ runSynch $ synthPoly alsa pcm n

runSynth = runCompiled'
    defaultExtCompilerOpts {externalFlagsPost = ["-lm","-lasound"]}
    synthMain

runSynthPoly = runCompiled'
    defaultExtCompilerOpts {externalFlagsPost = ["-lm","-lasound"]}
    synthPolyMain

main = icompile synthMain

