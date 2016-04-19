{-# LANGUAGE QuasiQuotes #-}

-- | A simple program for copying sound from microphone to speakers

module Main where



import qualified Prelude ()
import Control.Arrow

import Feldspar.Run

import Feldspar.Synch.System
import Feldspar.Synch

import Feldspar.Vector

import ALSA



bufferLength = 25000  -- Sound device buffer length, 25s
periodLength = 3000   -- Chunk length (approximate main loop period), 3s

capture :: ALSA -> PCM -> PCM -> Data Length -> Synch Run () ()
capture alsa capt play n
    =   arrSource (readPCM alsa capt n)
    >>> arrProg (fromPull . tweak . toPull)
    >>> arrProg (writePCM alsa play)
  where
    tweak :: Vector (Data Int16) -> Vector (Data Int16)
    tweak = reverse

captureMain :: Run ()
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

runCapture = runCompiled'
    defaultExtCompilerOpts {externalFlagsPost = ["-lm","-lasound"]}
    captureMain

main = icompile captureMain

