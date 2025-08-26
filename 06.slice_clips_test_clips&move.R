library(dplyr)
library(warbleR)

# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# distribution test set
# fix table because they are all in different formats
fix_columns <- function(df) {
  needed_cols <- c("sound.files", "selec", "start", "end", "domain", "label", "path", "Common.Name")
  # some columns might be missing in the noise df. Create respective columns and fill them with NA character
  # e.g. Pool_dartmoor_noise
  for (col in needed_cols) if (!col %in% names(df)) df[[col]] <- NA_character_
  df$sound.files   <- as.character(df$sound.files)
  df$selec         <- as.character(df$selec)
  df$start         <- as.numeric(df$start)
  df$end           <- as.numeric(df$end)
  df$domain        <- as.character(df$domain)
  df$label         <- as.character(df$label)
  df$path          <- as.character(df$path)
  df$Common.Name   <- as.character(df$Common.Name)
  # create Common.Name col and fill it with "nocall"
  # this is to match the BirdNET prediction during evaluation metrics
  df$Common.Name <- "nocall"
  # Fix path for Dartmoor noise
  df$path[df$domain == "Dartmoor"] <- "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise"
  # Fix path for Xenocanto noise: extract only the directory
  # path contains full name, change it to dirname
  df$path[df$domain == "Xenocanto"] <- dirname(df$path[df$domain == "Xenocanto"])
  return(df)
}

# apply function
Pool_xc_noise       <- fix_columns(Pool_xc_noise)
Pool_dartmoor_noise <- fix_columns(Pool_dartmoor_noise)

# sample 15 each for 20 times
set.seed(26)

n_reps <- 20

samples_whine_high_dr <- lapply(1:n_reps, function(i) pool_whine_high %>% slice_sample(n = 15))
samples_whine_low_dr  <- lapply(1:n_reps, function(i) pool_whine_low %>% slice_sample(n = 15))
samples_bark_high_dr  <- lapply(1:n_reps, function(i) pool_bark_high %>% slice_sample(n = 15))
samples_bark_low_dr   <- lapply(1:n_reps, function(i) pool_bark_low %>% slice_sample(n = 15))
samples_whine_high_xc <- lapply(1:n_reps, function(i) pool_whine_high_xc %>% slice_sample(n = 15))
samples_whine_low_xc  <- lapply(1:n_reps, function(i) pool_whine_low_xc %>% slice_sample(n = 15))
samples_bark_high_xc  <- lapply(1:n_reps, function(i) pool_bark_high_xc %>% slice_sample(n = 15))
samples_bark_low_xc   <- lapply(1:n_reps, function(i) pool_bark_low_xc %>% slice_sample(n = 15))
samples_dr_noise   <- lapply(1:n_reps, function(i) Pool_dartmoor_noise %>% slice_sample(n = 60))
samples_xc_noise   <- lapply(1:n_reps, function(i) Pool_xc_noise %>% slice_sample(n = 60))

# compile 15 from each as test dataset
make_selec_char <- function(df) {
  if("selec" %in% names(df)) df$selec <- as.character(df$selec)
  df
}

test_sets <- lapply(1:n_reps, function(i) {
  bind_rows(
    make_selec_char(samples_whine_high_dr[[i]]),
    make_selec_char(samples_whine_low_dr[[i]]),
    make_selec_char(samples_bark_high_dr[[i]]),
    make_selec_char(samples_bark_low_dr[[i]]),
    make_selec_char(samples_whine_high_xc[[i]]),
    make_selec_char(samples_whine_low_xc[[i]]),
    make_selec_char(samples_bark_high_xc[[i]]),
    make_selec_char(samples_bark_low_xc[[i]]),
    make_selec_char(samples_dr_noise[[i]]),
    make_selec_char(samples_xc_noise[[i]])
  )
})

# Define base directory
base_dir <- "F:/MSc Ecology & Data Science Research/3. test_data"

# Generate 20 folder paths
folder_names <- sprintf("test_set_%02d", 1:20)
folder_paths <- file.path(base_dir, folder_names)

# Create the folders
sapply(folder_paths, dir.create, recursive = TRUE, showWarnings = FALSE)

# draw and distribute clips ####
augment_folder_map <- list(
  Whine_High = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_high snr_call",
  Whine_Low  = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_low snr_call",
  Bark_High  = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_high snr_call",
  Bark_Low   = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_low snr_call"
)

# direct dartmoor/britishlib file into test file folder while xeno canto file into xeno canto folder
for (i in seq_along(folder_paths)) {
  dest_folder <- folder_paths[i]
  test_set <- test_sets[[i]]

  # check if clip.files present. If not, create clip.files from sound.files and selec
  if (!"clip.files" %in% names(test_set)) {
    test_set$clip.files <- paste0(sub("\\.wav$", "", test_set$sound.files), "-", test_set$selec, ".wav")
  }
  
  # ensure freq cols exist and have no NAs (sampling rate: 48 kHz)
  # dartmoor noise does not have freq cols because I splitted them using warbleR
  # not directly from Raven Pro
  if (!"bottom.freq" %in% names(test_set)) test_set$bottom.freq <- 0
  if (!"top.freq" %in% names(test_set))    test_set$top.freq    <- 24
  test_set$bottom.freq[is.na(test_set$bottom.freq)] <- 0
  test_set$top.freq[is.na(test_set$top.freq)]       <- 24
  
  for (j in seq_len(nrow(test_set))) {
    row <- test_set[j, , drop = FALSE]
    # cut britishlib / dartmoor call straight to the test set files
    if (row$domain %in% c("Britishlib", "Dartmoor") && row$Common.Name != "nocall") {
      the_path <- row$path
      sel_tab <- selection_table(X = row, files = as.character(row$sound.files), path = the_path)
      cut_sels(X = sel_tab, path = the_path, dest.path = dest_folder)
      next
    }
    # cut dartmoor noise to test set files
    if if (row$Common.Name == "nocall" && row$domain == "Dartmoor") {
      noise_path <- "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise"
      sel_tab <- selection_table(X = row, files = as.character(row$sound.files), path = noise_path)
      cut_sels(X = sel_tab, path = noise_path, dest.path = dest_folder)
      next
    }
    # copy augmented calls if any and cut xeno-canto calls to make up for the remaining
    if (row$domain == "Xenocanto" && row$Common.Name != "nocall") {
      call_type <- row$Call.Type
      snr_cat <- row$SNR_cat
      folder_key <- paste(call_type, snr_cat, sep = "_")
      aug_folder <- augment_folder_map[[folder_key]]
      src_file_aug <- file.path(aug_folder, row$clip.files)
      # copy augmented clip to test set folder if any
      if (!is.null(aug_folder) && file.exists(src_file_aug)) {
        file.copy(src_file_aug, file.path(dest_folder, basename(src_file_aug)), overwrite = TRUE)
        # if XC recordings are not in augmented folders, cut from Xeno Canto recordings
      } else {
        the_path <- row$path
        sel_tab <- selection_table(X = row, files = as.character(row$sound.files), path = the_path)
        cut_sels(X = sel_tab, path = the_path, dest.path = dest_folder)
      }
      next
    }
    # cut xeno-canto noise to the test set folder
    if (row$domain == "Xenocanto" && row$Common.Name == "nocall") {
      the_path <- row$path
      sel_tab <- selection_table(X = row, files = as.character(row$sound.files), path = the_path)
      cut_sels(X = sel_tab, path = the_path, dest.path = dest_folder)
      next
    }
  }
}


# create folder path
for (i in seq_along(folder_paths)) {
  dest_folder <- folder_paths[i]
  test_set <- test_sets[[i]]
  class_col <- test_set$Common.Name
  
  # build output table (always 0-3)
  eval_df <- test_set %>%
    dplyr::mutate(
      `Start Time` = 0,
      `End Time`   = 3,
      Class        = class_col,
      `Begin Path` = file.path(dest_folder, clip.files)
    ) %>%
    dplyr::select(`Start Time`, `End Time`, Class, `Begin Path`)
  
  # write to file in the SAME destination folder as the .wav files
  out_file <- file.path(dest_folder, "test_eval.txt")
  write.table(
    eval_df,
    file = out_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
}

