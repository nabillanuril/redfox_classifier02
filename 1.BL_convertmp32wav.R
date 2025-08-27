library(av)
library(tidyverse)

setwd("F:/MSc Ecology & Data Science Research/1. Britishlib_red fox")

# List files with ".mp3" file extension
mp3_files <- list.files(pattern = "\\.mp3$", full.names = TRUE)
# Build output .wav paths
wav_files <- file.path(sub("\\.mp3$", ".wav", basename(mp3_files)))
# Convert each MP3 to WAV
for (i in seq_along(mp3_files)) {
  try(av_audio_convert(mp3_files[i], output = wav_files[i]))}

