library(warbleR)
library(tidyverse)
library(stringr)
library(tibble)
library(purrr)

# this script is for:
# 1. Loading all selection tables if they are already labelled
# 2. Automatically creating selection tables for Dartmoor noise
# 3. Generating weak labels from selection tables

# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# XC red fox strong  label ####
# list all selection tables and put the selection table name as wav sound.files inside df
file_paths <- list.files(
  path        = "1. Xenocanto_label",
  pattern     = "\\.Table.1.selections.txt$",
  full.names  = TRUE
)

sound_files <- basename(file_paths) %>%
  str_remove("\\.Table.*$") %>% paste0(".wav")

sound_paths <- file.path("1. Xenocanto_foreground_wav", sound_files) %>% dirname()

# read and adjust the selection table from Raven Pro to match the table format of warbleR
raven2warbleR <- function(filepath, soundfiles, soundpath, domain) {
  read.table(filepath,
             sep = "\t",
             header = TRUE,
             stringsAsFactors = FALSE) %>%
    # change df format into the one that fits warbleR
    rename(
      selec = Selection,
      start = Begin.Time..s.,
      end = End.Time..s.,
      bottom.freq = Low.Freq..Hz.,
      top.freq = High.Freq..Hz.,
      channel = Channel
    ) %>%
    mutate(
      sound.files = soundfiles,
      bottom.freq = bottom.freq / 1000,
      top.freq    = top.freq    / 1000,
      domain      = domain,
      path        = soundpath,
      label       = "strong"
    )
}

# combine selection table into one list
seltab_sp_xc <- mapply(raven2warbleR, file_paths, sound_files, 
                       sound_paths, "Xenocanto", SIMPLIFY = FALSE)

# merge bark and whine if separated by less than 0.02 seconds
# the function below merges bark with adjacent bark, and whine with adjacent whine,
# while keeping bark and whine separate
merge_bark_whine_bouts <- function(df, gap = 0.02) {
  wanted_types <- c("Bark", "Whine")
  # check if df in the list has bark and/or whine
  present_types <- intersect(unique(df$Call.Type), wanted_types)
  if (length(present_types) == 0) return(as.data.frame(df))
  merged <- df %>%
    filter(Call.Type %in% wanted_types) %>%
    arrange(Call.Type, start) %>%
    group_by(Call.Type) %>%
    mutate(
      time_gap = start - lag(end, default = first(start)),
      group = cumsum(time_gap > gap | is.na(time_gap))
    ) %>%
    group_by(Call.Type, group) %>%
    summarise(
      selec = first(selec),
      View = first(View),
      channel = first(channel),
      start = min(start),
      end = max(end),
      bottom.freq = min(bottom.freq),
      top.freq = max(top.freq),
      Common.Name = first(Common.Name),
      Call.Type = first(Call.Type),
      sound.files = first(sound.files),
      domain = first(domain),
      path = first(path),
      label = first(label),
      .groups = "drop"
    )
  
  untouched <- df %>% filter(!Call.Type %in% wanted_types)
  
  # combine, sort, and convert to base data.frame
  result <- bind_rows(merged, untouched) %>%
    arrange(start)
  
  as.data.frame(result)
}

seltab_sp_xc <- purrr::map(seltab_sp_xc, merge_bark_whine_bouts)

# BL red fox ####
# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# list all selection tables and put the selection table name as wav sound.files inside df
file_paths <- list.files(
  path        = "1. Britishlib_label",
  pattern     = "\\.Table.1.selections.txt$",
  full.names  = TRUE
)

sound_files <- basename(file_paths) %>%
  str_remove("\\.Table.*$") %>% paste0(".wav")

sound_paths <- file.path("1. Britishlib_red fox", sound_files) %>% dirname()

# combine selection table into one list
seltab_sp_bl <- mapply(raven2warbleR, file_paths, sound_files, 
                       sound_paths, "Britishlib", SIMPLIFY = FALSE)



# DR red fox strong label ####
# list all selection tables and put the selection table name as wav sound.files inside df
setwd("F:/MSc Ecology & Data Science Research")

file_paths <- list.files(
  path        = "1. Dartmoor 2023_red fox_label",
  pattern     = "\\.Table.1.selections.txt$",
  full.names  = TRUE
)

sound_files <- basename(file_paths) %>%
  str_remove("\\.Table.*$") %>% paste0(".WAV")

# the codes for DR red fox are different because the files are organised in nested folders
get_sound_paths <- tibble(
  full_path = list.files(
    path        = "1. Dartmoor 2023_red fox",
    pattern     = "\\.WAV$",
    full.names  = TRUE,
    recursive = TRUE)) %>% 
  mutate(sound_file = basename(full_path))

sound_paths <- tibble(sound_file = sound_files) %>%
  left_join(get_sound_paths, by = "sound_file") %>%
  pull(full_path) %>% dirname()


# combine selection table into one list
seltab_sp_dr <- mapply(raven2warbleR, file_paths, sound_files, 
                       sound_paths, "Dartmoor", SIMPLIFY = FALSE)



# XC red fox weak label ####
strong2weak <- function(df, dur = 3) {
  # use pre-defined df column names so the function can use data frames with specific column names as input
  start <- df$start
  end <- df$end
  files <- df$sound.files
  path <- df$path
  
  # create empty vector columns with the same length as start and end
  n <- length(start)
  new_start <- numeric(n)
  new_end <- numeric(n)
  
  set.seed(25) # so the function generates the same numbers each time
  # get the recording duration
  requireNamespace("warbleR", quietly = TRUE)
  # use unique because multiple start and end times have the same files and path
  # keeping the duplicates will cause errors and confuse the function with multiple paths
  sound_length <- warbleR::duration_sound_files(files = unique(files), path = unique(path), skip.error = FALSE)
  
  for (i in seq_len(n)) {
    # 3 issues found while creating this function:
    # 1. recording length < 3 seconds -> keep the original length
    # 2. strong label > 3 seconds -> clip randomly into 3 seconds
    # 3. strong label < 3 seconds -> expand bounding box
    # important information applicable to all conditions:
    # this function relies on warbleR to get the recording length
    # each start and end will use the same recording length from the same file name
    # how the lines below work:
    # warbleR::duration_sound_files returns 2 columns: duration and sound.files
    # to get duration, match the sound.files from df to sound.files from the warbleR result
    # read files
    this_file <- files[i]
    # find a match
    matched_file <- sound_length$sound.files == this_file
    # get recording length from matched files
    rec_length <- sound_length$duration[matched_file]  
    sel_length <- end[i] - start[i]
    # condition 1
    if (rec_length <= dur){
      tmp_start <- 0
      tmp_end   <- rec_length
      new_start[i] <- tmp_start
      new_end[i]   <- tmp_end
    } 
    # condition 2
    else if (sel_length >= dur) {
      red_start <- end[i] - dur
      tmp_start <- runif(1, start[i], red_start)
      tmp_end   <- tmp_start + dur
      new_start[i] <- tmp_start
      new_end[i]   <- tmp_end
    } 
    # condition 3
    else {
      empty <- dur - (end[i] - start[i])
      ex.start <- runif(1, 0, empty) 
      ex.end <- empty - ex.start
      tmp_start <- start[i] - ex.start
      tmp_end   <- end[i]   + ex.end
      new_start[i] <- tmp_start
      new_end[i]   <- tmp_end
      # fix if the temporary start is negative
      if (tmp_start < 0) {
        tmp_end <- tmp_end + (0 - tmp_start)
        tmp_start <- 0
      }
      # fix if the temporary end is beyond the recording length
      if (tmp_end > rec_length) {
        tmp_start <- tmp_start - (tmp_end - rec_length)
        tmp_end <- rec_length
      }
      new_start[i] <- tmp_start
      new_end[i] <- tmp_end
    }
    
  }
  
  return(
    df %>%
      mutate(
        orig_start  = start,
        orig_end    = end,
        start       = new_start,
        end         = new_end,
        label       = "weak"
      ) 
  )
}

# generate weak labels from strong labels
seltab_weak_xc <- lapply(seltab_sp_xc, strong2weak)


# BL red fox weak  label ####
# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# apply strong2weak function
seltab_weak_bl <- lapply(seltab_sp_bl, strong2weak)


# DR red fox weak label ####
# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# apply strong2weak function
seltab_weak_dr <- lapply(seltab_sp_dr, strong2weak)


# XC noise ####
# always set wd when you work with warbler
setwd("F:/MSc Ecology & Data Science Research")
# list all selection tables and put the selection table name as wav sound.files inside df
file_paths <- list.files(
  path        = "1. Xenocanto_label",
  pattern     = "\\.Table.2.selections.txt$",
  full.names  = TRUE
)

sound_files <- basename(file_paths) %>%
  str_remove("\\.Table.*$") %>% paste0(".wav")

sound_paths <- file.path("1. Xenocanto_foreground_wav", sound_files)

# combine selection table into one list
seltab_noise_xc <- mapply(raven2warbleR, file_paths, sound_files, 
                          sound_paths, "Xenocanto", SIMPLIFY = FALSE)

# fix columns
cols_to_fix <- c("Common.Name", "View")
seltab_noise_xc <- lapply(seltab_noise_xc, function(df) {
  if ("Common_Name" %in% names(df)) names(df)[names(df) == "Common_Name"] <- "Common.Name"
  for (col in cols_to_fix) {
    df[[col]] <- as.character(df[[col]])
  }
  df
})

# only choose noise from one channel
xc_noise <- seltab_noise_xc %>% bind_rows() %>% filter(channel == 1) %>%
  # the line below removes noise clips that cause errors
  # noise clips that cause errors are shorter than 3 seconds
  filter((end - start) == 3)

# BL noise ####
# list all selection tables and put the selection table name as wav sound.files inside df
file_paths <- list.files(
  path        = "1. Britishlib_label",
  pattern     = "\\.Table.2.selections.txt$",
  full.names  = TRUE
)

sound_files <- basename(file_paths) %>%
  str_remove("\\.Table.*$") %>% paste0(".wav")

sound_paths <- rep("1. Britishlib_red fox", length(sound_files))

# combine selection tables into one list
seltab_noise_bl <- mapply(raven2warbleR, file_paths, sound_files, 
                          sound_paths, "Britishlib", SIMPLIFY = FALSE)

# fix columns
seltab_noise_bl <- lapply(seltab_noise_bl, function(df) {
  for (col in cols_to_fix) {
    df[[col]] <- as.character(df[[col]])
  }
  df
})

bl_noise <- seltab_noise_bl %>% bind_rows()

# DR noise ####
# DR recordings were only manually labelled at the 10-minute recording level
# noise from DR was not manually labelled at the 3-second clip level
# warbleR was used instead to automatically create 3-second selection tables

# load recording list from Excel
fox_active <- readxl::read_xlsx("1. Dartmoor 2023_red fox/fox_active.xlsx")

# generate a new name for each recording with the site name as prefix,
# because some recordings have the same name (they were named by date and time of survey)
# without renaming, the files would overwrite each other when being moved/copied into a new folder
fox_active$recording <- paste(fox_active$site, fox_active$recording, sep = "_")

# filter out files without red fox calls
noise_dr <- fox_active %>% filter(Fox_Presence != "Red_Fox") %>%
  # exclude unused columns from copying clips based on red fox activity
  subset(select = -c(dest_dir, dest_file, source_file, BirdNET_Prediction)) %>%
  # add a new column for file.path
  mutate(source_file  = file.path("1. Dartmoor 2023_red fox", 
                                  month, site, date, recording),
         source_dir   = file.path("1. Dartmoor 2023_red fox", 
                                  month, site, date)) 

# calculate total recording
total_recording <- 0

for (i in 1:nrow(noise_dr)) {
  file_path <- file.path(noise_dr$source_dir[i], noise_dr$recording[i])
  df <- duration_sound_files(files = file_path)
  # duration_sound_files returns a 2-column df
  # the line below accumulates the total duration from the df
  total_recording <- total_recording + as.numeric(df$duration)
}

print(total_recording)

# split recordings into 3-second clips and save the selection table into a new object
seltab_noise_dr <- lapply(noise_dr$source_dir, 
                          split_sound_files, 
                          sgmt.dur = 3, only.sels = TRUE) %>% bind_rows()

# dartmoor_noise$org.sound.files and dartmoor_noise$recording contain the same values, remove one of them
noise_dr$org.sound.files <- noise_dr$recording

# merge the two data frames (before and after audio split) so that every split is well documented
seltab_noise_dr <-  merge(noise_dr, seltab_noise_dr, by = "org.sound.files")

dartmoor_noise <- seltab_noise_dr %>%
  group_by(sound.files) %>%
  mutate(selec = str_extract(sound.files, "(?<=-)\\d+(?=\\.wav$)"),
         domain = "Dartmoor") %>%
  ungroup() %>% 
  mutate(channel = 1,
         clip.files = sound.files,
         sound.files = org.sound.files)





