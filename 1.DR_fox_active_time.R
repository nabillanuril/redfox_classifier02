library(warbleR)
library(tidyverse)
library(stringr)
library(chron)


# Set working directory
setwd("F:/MSc Ecology & Data Science Research")

## Create a metadata of months, locations, dates, file names, and file paths from file paths
# List sound files
recording_list <- list.files (path = "1. Dartmoor 2023_Raw/",
                              full.names = TRUE,
                              recursive = TRUE)

# List folders
folder_list <- list.files (path = "1. Dartmoor 2023_Raw/",
                           recursive = TRUE)

# Turn information from the file paths into categorical information
folder_level <- c("month", "site", "date", "recording")
# Create the metadata
recording_metadata <- tibble(path = folder_list) %>%
  separate(path, into = folder_level, sep = "/") %>% 
  drop_na() # drop na caused by the readme file

# Filter recordings from 00:00:00 to 04:00:00 and 22:00:00 to 23:40:00
time_str<- recording_metadata$recording %>%
  # remove string before and after time
      str_remove("^\\d{8}_") %>% # strip leading yyyymmdd_
      str_remove(regex("\\.WAV$")) 

# Turn string from the file names into times
recording_metadata$time <- str_c(
    str_sub(time_str, 1, 2), ":",
    str_sub(time_str, 3, 4), ":",
    str_sub(time_str, 5, 6)) %>% times()

# Keep only fox-active windows
fox_active <- recording_metadata %>% group_by(month, site, date) %>% 
  filter((time >= times("00:00:00") & time <= times("04:00:00")) |
      (time >= times("22:00:00") & time <= times("23:40:00")))

# Create a list of output directory using information from month, site, and date
source_dir <- ("1. Dartmoor 2023_Raw")
fox_active <- fox_active %>%
  mutate(
    # list destination file directory
    dest_dir  = file.path("1. Dartmoor 2023_red fox", month, site, date),
    # list output paths
    dest_file = file.path(dest_dir, paste(site, sep = "_", basename(recording))),
    # see if you can paste the site name to the recording name
    # list source file path
    source_file  = file.path(source_dir, month, site, date, recording))
  
# Create the destination file directory
lapply(fox_active$dest_dir, dir.create, recursive = TRUE)

# Copy files using the source and destination file directory
copy_wav <- file.copy(from = fox_active$source_file, to = fox_active$dest_file)

# check all recordings manually in Raven Pro
# export fox_active as an Excel file
# this code can only be used once. If run, it will overwrite the manually labeled table
# fox_active %>% as.data.frame() %>%
#  write.xlsx(file = "F:/MSc Ecology & Data Science Research/1. Dartmoor 2023/fox_active.xlsx")

# label recordings with and without fox calls in Raven Pro

