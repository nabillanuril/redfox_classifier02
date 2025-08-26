library(dplyr)
library(warbleR)
library(purrr)

# this script is for curating training data for the Dartmoor domain
# calls used to create training data are from Xeno-Canto recordings
# noise used to create augmented calls and background noise is from Dartmoor recordings
# filtered calls go to the "augment_data_train" folder to be mixed with Dartmoor background noise in Python (next script)
# Dartmoor noise for augmented data also goes to the "augment_data_train" folder to be mixed with Xeno-Canto calls in Python (next script)
# results of augmentation will later go to the "train_data" folder for training (explained further in the Python script)
# Dartmoor noise for training goes to the "train_data" folder for training


xc_test_call <- bind_rows(
  pool_whine_high,
  pool_whine_low,
  pool_bark_high,
  pool_bark_low,
  pool_whine_high_xc,
  pool_whine_low_xc,
  pool_bark_high_xc,
  pool_bark_low_xc
)

dr_test_noise <- bind_rows(all_noise_prev,
                           Pool_dartmoor_noise)


# Join keys
# will be used later with anti_join to keep training and test data separate
join_keys_char <- c("sound.files", "selec")
join_keys_num  <- c("start", "end")
join_keys      <- c(join_keys_char, join_keys_num)

# Ensure types MATCH for join keys in BOTH data frames
public_domain <- public_domain %>%
  mutate(
    across(all_of(join_keys_char), as.character),
    across(all_of(join_keys_num),  as.numeric)
  )

xc_test_call <- xc_test_call %>%
  mutate(
    across(all_of(join_keys_char), as.character),
    across(all_of(join_keys_num),  as.numeric)
  )

# define path
path_call  <- file.path("F:/MSc Ecology & Data Science Research", unique(public_domain$path))
path_noise <- file.path("F:/MSc Ecology & Data Science Research/1. Dartmoor 2023_noise")

# dartmoor, weak, high SNR ####
# whine
set.seed(5)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=5) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_weak_call")

# bark
set.seed(5)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=5) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_weak_call")

# sample noise
set.seed(5)
sampled_noise <- dartmoor_noise %>% anti_join(dr_test_noise, by = join_keys) %>% 
  group_by(month, site, date) %>%
  slice_sample(n = 9) %>%
  ungroup() %>% slice_sample(n = 480)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)
# move all files into new folder
setwd("F:/MSc Ecology & Data Science Research")
sampled_noise$dest_file <- file.path("1. Dartmoor 2023_noise", sampled_noise$sound.files)
copy_wav <- file.copy(from = sampled_noise$source_file, to = sampled_noise$dest_file)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)

# noise for mixing
sampled_noise_1 <- sampled_noise %>% slice_sample(n = 240)
sampled_noise <- sampled_noise_1 %>% selection_table(path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_weak_noise")

# noise for training
sampled_noise_2 <- sampled_noise %>% anti_join(sampled_noise_1, by = join_keys) %>% slice_sample(n = 240)
selection_table(sampled_noise, path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. train_data/1. Wilcoxon/dr_highsnr_weak/Noise")

# dartmoor, weak, low SNR ####
# whine
set.seed(6)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=6) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_weak_call")

# bark
set.seed(6)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "weak",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=6) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_weak_call")

# sample noise
set.seed(6)
sampled_noise <- dartmoor_noise %>% anti_join(dr_test_noise, by = join_keys) %>% 
  group_by(month, site, date) %>%
  slice_sample(n = 9) %>%
  ungroup() %>% slice_sample(n = 480)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)
# move all files into new folder
setwd("F:/MSc Ecology & Data Science Research")
sampled_noise$dest_file <- file.path("1. Dartmoor 2023_noise", sampled_noise$sound.files)
copy_wav <- file.copy(from = sampled_noise$source_file, to = sampled_noise$dest_file)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)

# noise for mixing
sampled_noise_1 <- sampled_noise %>% slice_sample(n = 240)
sampled_noise <- sampled_noise_1 %>% selection_table(path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_weak_noise")

# noise for training
sampled_noise_2 <- sampled_noise %>% anti_join(sampled_noise_1, by = join_keys) %>% slice_sample(n = 240)
selection_table(sampled_noise, path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. train_data/1. Wilcoxon/dr_lowsnr_weak/Noise")

# dartmoor, strong, high SNR ####
# whine
set.seed(7)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "strong",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=7) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_strong_call")

# bark
set.seed(7)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "strong",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=7) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_strong_call")

# sample noise
set.seed(7)
sampled_noise <- dartmoor_noise %>% anti_join(dr_test_noise, by = join_keys) %>% 
  group_by(month, site, date) %>%
  slice_sample(n = 9) %>%
  ungroup() %>% slice_sample(n = 480)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)
# move all files into new folder
setwd("F:/MSc Ecology & Data Science Research")
sampled_noise$dest_file <- file.path("1. Dartmoor 2023_noise", sampled_noise$sound.files)
copy_wav <- file.copy(from = sampled_noise$source_file, to = sampled_noise$dest_file)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)

# noise for mixing
sampled_noise_1 <- sampled_noise %>% slice_sample(n = 240)
sampled_noise <- sampled_noise_1 %>% selection_table(path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_high snr_strong_noise")

# noise for training
sampled_noise_2 <- sampled_noise %>% anti_join(sampled_noise_1, by = join_keys) %>% slice_sample(n = 240)
selection_table(sampled_noise, path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. train_data/1. Wilcoxon/dr_highsnr_strong/Noise")

# dartmoor, strong, low SNR ####
# whine
set.seed(8)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "strong",
         SNR_cat == "High",
         Call.Type == "Whine") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=8) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_strong_call")

# bark
set.seed(8)
sample <- public_domain %>% 
  filter(domain == "Xenocanto",
         label == "strong",
         SNR_cat == "High",
         Call.Type == "Bark") %>%
  anti_join(xc_test_call, by = join_keys) %>%
  sample_overlap(n=8) %>%
  slice_sample(n = 120)
sample <- sample %>% selection_table(path = path_call)
cut_sels(sample, 
         path = path_call, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_strong_call")

# sample noise
set.seed(8)
sampled_noise <- dartmoor_noise %>% anti_join(dr_test_noise, by = join_keys) %>% 
  group_by(month, site, date) %>%
  slice_sample(n = 9) %>%
  ungroup() %>% slice_sample(n = 480)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)
# move all files into new folder
setwd("F:/MSc Ecology & Data Science Research")
sampled_noise$dest_file <- file.path("1. Dartmoor 2023_noise", sampled_noise$sound.files)
copy_wav <- file.copy(from = sampled_noise$source_file, to = sampled_noise$dest_file)
sampled_noise$sound.files <- factor(sampled_noise$sound.files)

# noise for mixing
sampled_noise_1 <- sampled_noise %>% slice_sample(n = 240)
sampled_noise <- sampled_noise_1 %>% selection_table(path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. augment data_train/Wilcoxon/Dartmoor_low snr_strong_noise")

# noise for training
sampled_noise_2 <- sampled_noise %>% anti_join(sampled_noise_1, by = join_keys) %>% slice_sample(n = 240)
selection_table(sampled_noise, path = path_noise)
cut_sels(sampled_noise, path = path_noise, 
         dest.path = "F:/MSc Ecology & Data Science Research/3. train_data/1. Wilcoxon/dr_lowsnr_strong/Noise")


