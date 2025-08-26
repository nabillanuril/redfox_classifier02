library(tibble)
library(dplyr)
library(ggplot2)
library(purrr)
library(yardstick)


# PR curve ####
dirs <- all_combinations$folder_path

# load all data tables
all_threshold <- purrr::map_dfr(
  dirs,
  function(folder_path) {
    csv_files <- list.files(
      path = folder_path,
      pattern = "^data_table_\\d{2}\\.csv$",
      full.names = TRUE
    )
    purrr::map_dfr(
      csv_files,
      function(csv_file) {
        csv_name <- basename(csv_file)
        csv_number <- sub("^.*data_table_(\\d{2})\\.csv$", "\\1", csv_name)
        df <- read.csv(csv_file)
        metrics <- df %>%
          mutate(
            combination = basename(folder_path),
            test_set = csv_number,
            .before = 1
          )
      }
    )
  }
) 

data_table <- all_threshold

all_threshold <- all_threshold %>% distinct(across(-test_set))

df <- all_threshold %>%
  mutate(
    truth = factor(Red.Fox_annotation, levels = c(1, 0)),
    score = Red.Fox_confidence
  )

pr_by_comb <- df %>%
  group_by(combination) %>%
  pr_curve(truth, score)

# visualise PR-curve
ggplot(pr_by_comb, aes(x = recall, y = precision, colour = combination)) +
  geom_path(linewidth = 0.8) +
  coord_equal() +
  labs(title = "Precision–Recall",
       x = "Recall", 
       y = "Precision", 
       colour = "Combination",
       hjust = 0.5) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_fixed(ratio = 1)

# calculate AP by training data combination ####
# prepare data
# set 1 as positive and 0 as negative
df <- all_threshold %>%
  mutate(
    truth = factor(Red.Fox_annotation, levels = c(1, 0)),
    score = Red.Fox_confidence
  )

# AP per combination
ap_by_combo_test <- df %>%
  group_by(combination) %>%
  average_precision(truth, score, event_level = "first") %>%
  rename(AP = .estimate) %>%
  ungroup()

view(ap_by_combo_test)

