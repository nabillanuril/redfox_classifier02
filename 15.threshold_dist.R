library(dplyr)
library(purrr)
library(readr)

# list folder
base_dir <- "F:/MSc Ecology & Data Science Research/3. test_data"
folder_paths <- file.path(base_dir, sprintf("test_set_%02d", 1:20))

# specify txt file name
txt_name <- "test_eval.txt"

# read all txt and combine
all_test_set <- map_dfr(folder_paths, function(folder) {
  file_path <- file.path(folder, txt_name)
  
  if (file.exists(file_path)) {
    txt_name <- basename(folder)
    txt_number <- sub("^.*test_set_(\\d{2})$", "\\1", txt_name)
    read_tsv(file_path, show_col_types = FALSE) %>%
      mutate(test_set = txt_number)
  }
})

test_set_pred <- all_test_set %>% merge(all_threshold, by = "filename") %>% distinct(across(-test_set))

