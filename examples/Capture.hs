{-# LANGUAGE QuasiQuotes #-}

-- | A simple program for copying sound from microphone to speakers

module Main where



import qualified Prelude ()
import Control.Arrow

import Feldspar.Run

import Feldspar.Synch.System
import Feldspar.Synch

import Feldspar.Data.Vector

import ALSA



bufferLength = 25000  -- Sound device buffer length, 25s
periodLength = 3000   -- Chunk length (approximate main loop period), 3s

capture :: ALSA -> PCM -> PCM -> Data Length -> Synch Run () ()
capture alsa capt play n
    =   arrSource (readPCM alsa capt n)
    >>> arrProg (manifestFresh . tweak . toPull)
    >>> arrProg (writePCM alsa play)
  where
    tweak :: Pull (Data Int16) -> Pull (Data Int16)
    tweak = reverse

captureMain :: Run ()
captureMain = do
    alsa@(ALSA {..}) <- importALSA
    capt <- newPCM
    play <- newPCM
    m    <- initPCM capt Capture  1 bufferLength periodLength
    n    <- initPCM play Playback 1 bufferLength periodLength
    execSystem $ runSynch $ capture alsa capt play n

runCapture = runCompiled'
    def
    def {externalFlagsPost = ["-lm","-lasound"]}
    captureMain

main = icompile captureMain

