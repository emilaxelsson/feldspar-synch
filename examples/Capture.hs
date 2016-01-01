{-# LANGUAGE QuasiQuotes #-}

-- | A simple program for copying sound from microphone to speakers

module Main where



import qualified Prelude ()
import Control.Arrow

import Feldspar
import Feldspar.IO

import Feldspar.Synch.System
import Feldspar.Synch

import Feldspar.SimpleVector

import ALSA



bufferLength = 25000  -- Sound device buffer length, 25s
periodLength = 3000   -- Chunk length (approximate main loop period), 3s

capture :: ALSA -> PCM -> PCM -> Data Length -> Synch () ()
capture alsa capt play n
    =   arrSource (readPCM alsa capt n)
    >>> arr (desugar . tweak . sugar)
    >>> arrProg (writePCM alsa play)
  where
    tweak :: Vector1 Int16 -> Vector1 Int16
    tweak = reverse

captureMain :: Program ()
captureMain = do
    addInclude "\"feldspar_c99.h\""
    addInclude "\"feldspar_array.h\""
    addInclude "<math.h>"
    alsa@(ALSA {..}) <- importALSA
    capt <- newPCM
    play <- newPCM
    m    <- initPCM capt Capture  1 bufferLength periodLength
    n    <- initPCM play Playback 1 bufferLength periodLength
    printf "%d %d\n" m n
    let n = 128  -- TODO n must currently be known statically because otherwise
                 --      we get an array declaration of unknown length.
    execSystem $ runSynch $ capture alsa capt play n

runCapture = runCompiled [] captureMain ["-lm","-lasound"]

main = icompile captureMain

