{-# LANGUAGE QuasiQuotes #-}

-- | A simple binding to the ALSA C library

module ALSA
  ( PCM
  , StreamMode (..)
  , ALSA (..)
  , sampleRate
  , formatBits
  , importALSA
  , quantize
  , dequantize
  ) where



import Prelude ()

import Feldspar.Data.Vector
import Feldspar.Run



format_def = [cedecl|
// Sample format
static typename snd_pcm_format_t format = SND_PCM_FORMAT_S16;
|]

format_bits_def = [cedecl|
// Bits per sample
static typename uint32_t format_bits = 16;
|]

rate_def = [cedecl|
// Stream rate (Hz)
static unsigned int rate = 44100;
|]

set_hwparams_def = [cedecl|
static int set_hwparams(typename snd_pcm_t *handle,
                        typename snd_pcm_hw_params_t *params,
                        typename uint32_t channels,
                        typename uint32_t *buffer_time,
                        typename uint32_t *period_time,
                        typename snd_pcm_uframes_t *buffer_size,
                        typename snd_pcm_uframes_t *period_size)
{
    unsigned int rrate;
    typename snd_pcm_uframes_t size;
    int err, dir;
    /* choose all parameters */
    err = snd_pcm_hw_params_any(handle, params);
    if (err < 0) {
        printf("Broken configuration for playback: no configurations available: %s\n", snd_strerror(err));
        return err;
    }
    /* set hardware resampling */
    err = snd_pcm_hw_params_set_rate_resample(handle, params, 1);
    if (err < 0) {
        printf("Resampling setup failed for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* set the interleaved read/write format */
    err = snd_pcm_hw_params_set_access(handle, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    if (err < 0) {
        printf("Access type not available for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* set the sample format */
    err = snd_pcm_hw_params_set_format(handle, params, format);
    if (err < 0) {
        printf("Sample format not available for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* set the count of channels */
    err = snd_pcm_hw_params_set_channels(handle, params, channels);
    if (err < 0) {
        printf("Channels count (%i) not available for playbacks: %s\n", channels, snd_strerror(err));
        return err;
    }
    /* set the stream rate */
    rrate = rate;
    err = snd_pcm_hw_params_set_rate_near(handle, params, &rrate, 0);
    if (err < 0) {
        printf("Rate %iHz not available for playback: %s\n", rate, snd_strerror(err));
        return err;
    }
    if (rrate != rate) {
        printf("Rate doesn't match (requested %iHz, get %iHz)\n", rate, err);
        return -EINVAL;
    }
    /* set the buffer time */
    err = snd_pcm_hw_params_set_buffer_time_near(handle, params, buffer_time, &dir);
    if (err < 0) {
        printf("Unable to set buffer time %i for playback: %s\n", *buffer_time, snd_strerror(err));
        return err;
    }
    err = snd_pcm_hw_params_get_buffer_size(params, &size);
    if (err < 0) {
        printf("Unable to get buffer size for playback: %s\n", snd_strerror(err));
        return err;
    }
    *buffer_size = size;
    /* set the period time */
    err = snd_pcm_hw_params_set_period_time_near(handle, params, period_time, &dir);
    if (err < 0) {
        printf("Unable to set period time %i for playback: %s\n", *period_time, snd_strerror(err));
        return err;
    }
    err = snd_pcm_hw_params_get_period_size(params, &size, &dir);
    if (err < 0) {
        printf("Unable to get period size for playback: %s\n", snd_strerror(err));
        return err;
    }
    *period_size = size;
    /* write the parameters to device */
    err = snd_pcm_hw_params(handle, params);
    if (err < 0) {
        printf("Unable to set hw params for playback: %s\n", snd_strerror(err));
        return err;
    }
    return 0;
}
|]

set_swparams_def = [cedecl|
static int set_swparams(typename snd_pcm_t *handle,
                        typename snd_pcm_sw_params_t *swparams,
                        typename snd_pcm_uframes_t buffer_size,
                        typename snd_pcm_uframes_t period_size)
{
    int err;
    /* get the current swparams */
    err = snd_pcm_sw_params_current(handle, swparams);
    if (err < 0) {
        printf("Unable to determine current swparams for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* start the transfer when the buffer is almost full: */
    /* (buffer_size / avail_min) * avail_min */
    err = snd_pcm_sw_params_set_start_threshold(handle, swparams, buffer_size);
    if (err < 0) {
        printf("Unable to set start threshold mode for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* allow the transfer when at least period_size samples can be processed */
    /* or disable this mechanism when period event is enabled (aka interrupt like style processing) */
    err = snd_pcm_sw_params_set_avail_min(handle, swparams, period_size);
    if (err < 0) {
        printf("Unable to set avail min for playback: %s\n", snd_strerror(err));
        return err;
    }
    /* write the parameters to the playback device */
    err = snd_pcm_sw_params(handle, swparams);
    if (err < 0) {
        printf("Unable to set sw params for playback: %s\n", snd_strerror(err));
        return err;
    }
    return 0;
}
|]

initialize_pcm_def = [cedecl|
// Returns the period size (number of samples per channel in each chunk written
// to the sound card)
typename uint32_t initialize_pcm(typename snd_pcm_t **handle,
                                 typename snd_pcm_stream_t mode,
                                 typename uint32_t channels,
                                 typename uint32_t buffer_time,
                                 typename uint32_t period_time)
{
    typename snd_pcm_hw_params_t *hwparams;
    typename snd_pcm_sw_params_t *swparams;
    typename snd_pcm_uframes_t buffer_size;
    typename snd_pcm_uframes_t period_size;

    snd_pcm_open(handle, "default", mode, SND_PCM_NONBLOCK);
    snd_pcm_hw_params_alloca(&hwparams);
    snd_pcm_sw_params_alloca(&swparams);
    set_hwparams(*handle, hwparams, channels, &buffer_time, &period_time, &buffer_size, &period_size);
    set_swparams(*handle, swparams, buffer_size, period_size);

    return period_size;
}
|]

write_pcm_def = [cedecl|
// Returns how many samples were written
static int write_pcm(typename snd_pcm_t *handle, typename int16_t *samples, typename uint32_t length)
{
    snd_pcm_wait(handle, -1);
    snd_pcm_writei(handle, samples, length);
}
|]

read_pcm_def = [cedecl|
// Returns how many samples were read
static int read_pcm(typename snd_pcm_t *handle, typename int16_t *samples, typename uint32_t length)
{
    snd_pcm_wait(handle, -1);
    snd_pcm_readi(handle, samples, length);
}
|]

newtype PCM = PCM {unPCM :: Object}

data StreamMode = Playback | Capture
  deriving (Show)

data ALSA = ALSA
    { newPCM   :: Run PCM
    , initPCM  :: PCM -> StreamMode -> Data Word32 -> Data Word32 -> Data Word32 -> Run (Data Length)
    , writePCM :: PCM -> Manifest (Data Int16) -> Run ()
    , readPCM  :: PCM -> Data Length -> Run (Manifest (Data Int16))
    , closePCM :: PCM -> Run ()
    }

-- | Sample rate for sound device
sampleRate :: Num a => a
sampleRate = 44100
  -- Note: this constant is hard coded in the C code above

-- | Number of bits/sample for sound device
formatBits :: Num a => a
formatBits = 16
  -- Note: this constant is hard coded in the C code above

newPCM_ :: Run PCM
newPCM_ = fmap PCM $ newNamedObject "pcm" "snd_pcm_t" True

initPCM_
    :: PCM -> StreamMode -> Data Word32 -> Data Word32 -> Data Word32
    -> Run (Data Length)
initPCM_ pcm mode nChan bufTime perTime = callFun "initialize_pcm"
    [ addr $ objArg $ unPCM pcm
    , modeArg
    , valArg nChan
    , valArg bufTime
    , valArg perTime
    ]
  where
    modeArg = constArg "snd_pcm_stream_t" $ case mode of
        Playback -> "SND_PCM_STREAM_PLAYBACK"
        Capture  -> "SND_PCM_STREAM_CAPTURE"

writePCM_ :: PCM -> Manifest (Data Int16) -> Run ()
writePCM_ pcm samps = callProc "write_pcm"
    [ objArg $ unPCM pcm
    , iarrArg $ unManifest samps
    , valArg $ length samps
    ]

readPCM_ :: PCM -> Data Length -> Run (Manifest (Data Int16))
readPCM_ pcm len = do
    samps <- newArr len
    callProc "read_pcm"
        [ objArg $ unPCM pcm
        , arrArg samps
        , valArg len
        ]
    unsafeFreezeToManifest len samps

closePCM_ :: PCM -> Run ()
closePCM_ pcm = callProc "snd_pcm_close" [objArg $ unPCM pcm]

importALSA :: Run ALSA
importALSA = do
    addInclude "<alsa/asoundlib.h>"
    addInclude "<alloca.h>"
    addDefinition format_def
    addDefinition format_bits_def
    addDefinition rate_def
    addDefinition set_hwparams_def
    addDefinition set_swparams_def
    addDefinition initialize_pcm_def
    addDefinition write_pcm_def
    addDefinition read_pcm_def
    return $ ALSA
      { newPCM   = newPCM_
      , initPCM  = initPCM_
      , writePCM = writePCM_
      , readPCM  = readPCM_
      , closePCM = closePCM_
      }

-- | Convert a floating point value in the range [-1,1] to a 16-bit integer in
--- the range [minBound+1, maxBound]
quantize :: forall a . (RealFrac a, PrimType a) => Data a -> Data Int16
quantize a = round (a * i2n (maxBound :: Data Int16) :: Data a)

-- | Convert a floating point value in the range [-1,1] to a 16-bit integer in
--- the range [minBound+1, maxBound]
dequantize :: (Fractional a, PrimType a) => Data Int16 -> Data a
dequantize a = i2n a / i2n (maxBound :: Data Int16)

