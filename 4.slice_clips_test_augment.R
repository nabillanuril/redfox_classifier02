library(dplyr)
library(warbleR)

# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# merge overlapping weak lables into one group id and sample one weak label from one group id
sample_overlap <- function(df, n = 26) {
  dartmoor_whine <- df %>% bind_rows() 
  merged <- dartmoor_whine %>% 
    arrange(sound.files, start) %>%
    group_by(sound.files) %>%
    mutate(group_id = cumsum(start > cummax(lag(end, default = 0))))
  set.seed(n)
  non_overlap <- merged %>% group_by(sound.files, group_id) %>% 
    slice_sample(n = 1) %>%
    subset(select = -c(time_gap)) %>% 
    group_by(sound.files) %>% 
    mutate(time_gap = start - lag(end),
           time_gap = ifelse(is.na(time_gap),0, time_gap)) %>%
    ungroup()
  return(non_overlap)
}

# Dartmoor noise ####
# make 4 noise
# random sample
set.seed(26)
# selec column does not exist as a result of split_sound_files
# add selec and channel column
sampled_noise <- dartmoor_noise %>%
  group_by(month, site, date) %>%
  slice_sample(n = 6) %>%
  ungroup()
# move all files into a new folder
setwd("F:/MSc Ecology & Data Science Research")
sampled_noise$dest_file <- file.path("1. Dartmoor 2023_noise", basename(sampled_noise$sound.files))
copy_wav <- file.copy(from = sampled_noise$source_file, to = sampled_noise$dest_file)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)

# join keys for antijoin
join_keys <- c("sound.files", "selec", "start", "end")

# test data ####
# clip sample for testing, all weak labels
# dartmoor whine, high snr ####
sample_1 <- site_domain %>% 
  filter(domain %in% c("Britishlib", "Dartmoor"),
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  sample_overlap() %>%
  slice_sample(n = 30) 

n_available <- nrow(sample_1)

n_needed <- 30 - n_available

if(n_needed > 0){
  sample_xc <- public_domain %>%
    filter(domain == "Xenocanto",
           label == "weak",
           SNR_cat == "High",
           Call.Type == "Whine") %>%
    sample_overlap() %>%
    anti_join(sample_1, by = join_keys ) %>%
    slice_sample(n = n_needed)
  pool_whine_high <- bind_rows(sample_1, sample_xc)
  
  # Save only the Xenocanto portion for later augmentation
  pool_whine_high_xc <- sample_xc
} else {
  pool_whine_high <- sample_1 %>% slice_sample(n = 30)
  # No Xenocanto samples needed, so set an empty data frame for clarity
  pool_whine_high_xc <- pool_whine_high[0,]
}

process_and_cut <- function(row, dest_path){
  # Create one row selection table
  sel_tab <- selection_table(
    X = row,
    files = row$sound.files,
    path = row$path
  )
  cut_sels(
    X = sel_tab,
    path = row$path,
    dest.path = dest_path
  )
}

rows <- split(pool_whine_high_xc, seq_len(nrow(pool_whine_high_xc)))

lapply(rows, process_and_cut,
       dest_path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_high snr_call")

Dartmoor_whine_high_noise <- sampled_noise %>% 
  slice_sample(n = n_needed) %>%
  selection_table(path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise")

cut_sels(Dartmoor_whine_high_noise,
         path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise",
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_high snr_noise")

# dartmoor bark, high snr ####
sample <- site_domain %>% 
  filter(domain %in% c("Britishlib", "Dartmoor"),
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  sample_overlap() %>%
  slice_sample(n = 30) 

n_available <- nrow(sample)

n_needed <- 30 - n_available

if(n_needed > 0){
  sample_xc <- public_domain %>%
    filter(domain == "Xenocanto",
           label == "weak",
           SNR_cat == "High",
           Call.Type == "Bark") %>%
    sample_overlap() %>%
    anti_join(pool_whine_high_xc, 
              by = join_keys ) %>%
    slice_sample(n = n_needed)
  pool_bark_high <- bind_rows(sample, sample_xc)
  
  # Save only the Xenocanto portion for later augmentation
  pool_bark_high_xc <- sample_xc
} else {
  pool_bark_high <- sample %>% slice_sample(n = 30)
  # save empty data if Xeno Canto samples are not needed
  pool_bark_high_xc <- pool_bark_high[0,]
}

rows <- split(pool_bark_high_xc, seq_len(nrow(pool_bark_high_xc)))

lapply(rows, process_and_cut,
       dest_path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_high snr_call")

all_noise_prev <- bind_rows(Dartmoor_whine_high_noise)

Dartmoor_bark_high_noise <- sampled_noise %>% 
  anti_join(all_noise_prev, by = join_keys) %>%
  slice_sample(n = n_needed) %>%
  selection_table(path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise")

cut_sels(Dartmoor_bark_high_noise,
         path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise",
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_high snr_noise")

# dartmoor whine, low snr ####
sample <- site_domain %>% 
  filter(domain %in% c("Britishlib", "Dartmoor"),
         label == "weak",
         SNR_cat == "Low",
         Call.Type == "Whine") %>%
  sample_overlap() %>%
  slice_sample(n = 30) 

n_available <- nrow(sample)

n_needed <- 30 - n_available

all_xc_prev <- bind_rows(pool_whine_high_xc, pool_bark_high_xc)

if(n_needed > 0){
  sample_xc <- public_domain %>%
    filter(domain == "Xenocanto",
           label == "weak",
           SNR_cat == "High",
           Call.Type == "Whine") %>%
    sample_overlap() %>%
    anti_join(all_xc_prev, 
              by = join_keys ) %>%
    slice_sample(n = n_needed)
  pool_whine_low <- bind_rows(sample, sample_xc)
  
  # Save only the Xenocanto portion for later augmentation
  pool_whine_low_xc <- sample_xc
} else {
  pool_whine_low <- sample %>% slice_sample(n = 30)
  # save empty data if Xeno Canto samples are not needed
  pool_whine_low_xc <- pool_whine_low[0,]
}

rows <- split(pool_whine_low_xc, seq_len(nrow(pool_whine_low_xc)))

lapply(rows, process_and_cut,
       dest_path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_low snr_call")

all_noise_prev <- bind_rows(Dartmoor_whine_high_noise, Dartmoor_bark_high_noise)

Dartmoor_whine_low_noise <- sampled_noise %>% 
  anti_join(all_noise_prev, by = join_keys) %>%
  slice_sample(n = n_needed) %>%
  selection_table(path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise")

cut_sels(Dartmoor_whine_low_noise,
         path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise",
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_whine_low snr_noise")

# dartmoor bark, low snr ####
sample <- site_domain %>% 
  filter(domain %in% c("Britishlib", "Dartmoor"),
         label == "weak",
         SNR_cat == "Low",
         Call.Type == "Bark") %>%
  sample_overlap() %>%
  slice_sample(n = 30) 

n_available <- nrow(sample)

n_needed <- 30 - n_available

all_xc_prev <- bind_rows(pool_whine_high_xc, pool_bark_high_xc, pool_whine_low_xc)

if(n_needed > 0){
  sample_xc <- public_domain %>%
    filter(domain == "Xenocanto",
           label == "weak",
           SNR_cat == "High",
           Call.Type == "Bark") %>%
    sample_overlap() %>%
    anti_join(all_xc_prev,
              by = join_keys ) %>%
    slice_sample(n = n_needed)
  pool_bark_low <- bind_rows(sample, sample_xc)
  
  # Save only the Xenocanto portion for later augmentation
  pool_bark_low_xc <- sample_xc
} else {
  pool_bark_low <- sample %>% slice_sample(n = 30)
  # save empty data if Xeno Canto samples are not needed
  pool_bark_low_xc <- pool_bark_low[0,]
}

rows <- split(pool_bark_low_xc, seq_len(nrow(pool_bark_low_xc)))

lapply(rows, process_and_cut,
       dest_path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_low snr_call")

all_noise_prev <- bind_rows(Dartmoor_whine_high_noise, Dartmoor_bark_high_noise, Dartmoor_whine_low_noise)

Dartmoor_bark_low_noise <- sampled_noise %>% 
  anti_join(all_noise_prev, by = join_keys) %>%
  slice_sample(n = n_needed) %>%
  selection_table(path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise")

cut_sels(Dartmoor_bark_low_noise,
         path = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise",
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data/Wilcoxon/Dartmoor_bark_low snr_noise")

all_xc_prev <- bind_rows(pool_whine_high_xc, pool_bark_high_xc, pool_whine_low_xc, pool_bark_low_xc)

# Dartmoor noise ####
all_noise_prev <- bind_rows(Dartmoor_whine_high_noise, Dartmoor_bark_high_noise, 
                            Dartmoor_whine_low_noise, Dartmoor_bark_low_noise)
Pool_dartmoor_noise <- sampled_noise %>% 
  anti_join(all_noise_prev, by = join_keys) %>%
  slice_sample(n = 120)

# xeno canto whine high snr ####
all_xc_prev <- bind_rows(pool_whine_high, pool_bark_high, pool_whine_low, pool_bark_low)

pool_whine_high_xc <- public_domain %>%
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  sample_overlap() %>%
  anti_join(all_xc_prev,
            by = join_keys ) %>%
  slice_sample(n = 30)

# xeno canto bark high snr ####
all_xc_prev <- bind_rows(pool_whine_high, pool_bark_high, pool_whine_low, pool_bark_low,
                         pool_whine_high_xc)

pool_bark_high_xc <- public_domain %>%
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  sample_overlap() %>%
  anti_join(all_xc_prev,
            by = join_keys ) %>%
  slice_sample(n = 30)

# xeno canto whine low snr ####
all_xc_prev <- bind_rows(pool_whine_high, pool_bark_high, pool_whine_low, pool_bark_low,
                         pool_whine_high_xc, pool_bark_high_xc)

pool_whine_low_xc <- public_domain %>%
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "Low",
         Call.Type == "Whine") %>%
  sample_overlap() %>%
  anti_join(all_xc_prev,
            by = join_keys ) %>%
  slice_sample(n = 30)

# xeno canto bark low snr ####
all_xc_prev <- bind_rows(pool_whine_high, pool_bark_high, pool_whine_low, pool_bark_low,
                         pool_whine_high_xc, pool_bark_high_xc, pool_whine_low_xc)

pool_bark_low_xc <- public_domain %>%
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "Low",
         Call.Type == "Bark") %>%
  sample_overlap() %>%
  anti_join(all_xc_prev,
            by = join_keys ) %>%
  slice_sample(n = 30)

# XC noise ####
# check if the noise and the calls overlap for xc ####
library(dplyr)
library(stringr)

# make sure sound.files and selec have the same type
public_domain <- public_domain %>%
  mutate(
    sound.files = as.character(sound.files),
    selec = as.character(selec)
  )
xc_noise <- xc_noise %>%
  mutate(
    sound.files = as.character(sound.files),
    selec = as.character(selec)
  )

# specify sound.files and selec as the key
call_keys <- public_domain %>%
  select(sound.files, selec) %>%
  distinct()

# for each row in xc_noise, check if similar sound.files and selec exist in call_keys
for(i in seq_len(nrow(xc_noise))) {
  this_sf <- xc_noise$sound.files[i]
  this_sel <- xc_noise$selec[i]
  # filter similar sound.files and selec in both public_domain and xc_noise
  while(nrow(filter(call_keys, sound.files == this_sf, selec == this_sel)) > 0) {
    # if found, find next available selec
    used_sel <- c(
      # pull similar all selec values
      call_keys %>% filter(sound.files == this_sf) %>% pull(selec),
      xc_noise %>% filter(sound.files == this_sf) %>% pull(selec)
    )
    used_sel_int <- as.integer(used_sel)
    # find the maximum selec values of similar recording and add 1
    new_sel <- max(used_sel_int) + 1
    this_sel <- as.character(new_sel)
    xc_noise$selec[i] <- this_sel
  }
}

# generate new column called clip.files
xc_noise <- xc_noise %>%
  mutate(
    clip.files = paste0(sub("\\.wav$", "", sound.files), "-", selec, ".wav")
  )

Pool_xc_noise <- xc_noise %>% 

  slice_sample(n = 120)
