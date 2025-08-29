library(tidyverse)
library(readr)
library(fs)
library(stringr)
library(yardstick)

truth_root <- "F:/MSc Ecology & Data Science Research/3. test_data"
pred_root  <- "F:/MSc Ecology & Data Science Research/5. model prediction/1. Wilcoxon"

# get the combination
combinations <- dir_ls(pred_root) %>% path_file()

# look for all test_set
test_sets <- sprintf("test_set_%02d", 1:20)

truth_all <- tibble(test_set = test_sets) %>%
  mutate(
    test_eval_file = file.path(truth_root, test_set, "test_eval.txt")
  ) %>%
  # read each test_eval.txt
  mutate(data = map(test_eval_file, ~ read_tsv(.x, show_col_types = FALSE))) %>%
  select(test_set, data) %>%
  unnest(data) %>%
  # Standardise colnames (in case of spaces)
  rename(
    Start_Time = `Start Time`,
    End_Time   = `End Time`,
    Class      = Class,
    Begin_Path = `Begin Path`) %>%
  mutate(Red.Fox_annotation = if_else(Class == "Red Fox", 1L, 0L)) %>%
  select(test_set, Begin_Path, Red.Fox_annotation)
  
View(truth_all)

read_pred_one_set <- function(model, test_set) {
  pred_dir <- file.path(pred_root, model, test_set)
  files <- dir_ls(pred_dir, regexp = "\\.BirdNET\\.selection\\.table\\.txt$", type = "file")
  
  preds <- map_df(files, ~ read_tsv(.x, show_col_types = FALSE))
  
  preds %>%
    transmute(
      Begin_Path = gsub("\\\\", "/", `Begin Path`),
      Red.Fox_confidence = as.numeric(Confidence)
    )
}

pred_all <- crossing(combination = combinations, test_set = test_sets) %>%
  mutate(data = map2(combination, test_set, read_pred_one_set)) %>%
  select(combination, test_set, data) %>%
  unnest(data)

all_threshold <- pred_all %>%
  merge(truth_all, by = c("test_set", "Begin_Path"))

threshold <- 0.5

df <- all_threshold %>%
  mutate(
    truth = factor(Red.Fox_annotation, levels = c(1, 0)),
    score = Red.Fox_confidence,
    threshold_pred = factor(if_else(score >= threshold, 1, 0), levels = c(1, 0)),
    combination = factor(combination),
    test_set    = factor(test_set)
  ) %>% select(-Red.Fox_annotation)

prob_metrics  <- metric_set(average_precision, roc_auc)
prob_results  <- df %>%
  group_by(combination, test_set) %>%
  reframe(
    AP        = average_precision_vec(truth, score, event_level = "first"),
    AUROC     = roc_auc_vec(truth, score, event_level = "first")
  ) %>%
  ungroup()

class_results <- df %>%
  group_by(combination, test_set) %>%
  reframe(
    accuracy  = accuracy_vec(truth, threshold_pred, event_level = "first"),
    precision = precision_vec(truth, threshold_pred, event_level = "first"),
    recall    = recall_vec(truth, threshold_pred, event_level = "first"),
    f1       = f_meas_vec(truth, threshold_pred, event_level = "first")
  ) %>%
  ungroup()


metrics_by_set <- merge(prob_results, class_results, 
                        by = c("combination", "test_set")) %>%
  arrange(combination, test_set)

metrics_wide <- metrics_by_set %>%
  select(combination, test_set, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)


# save results as an Excel file
write.xlsx(truth_all, "F:/MSc Ecology & Data Science Research/Metadata/test_set_annotation.xlsx")
write.xlsx(pred_all, "F:/MSc Ecology & Data Science Research/Metadata/BirdNET_prediction.xlsx")
write.xlsx(metrics_by_set, "F:/MSc Ecology & Data Science Research/Metadata/evaluation_metrics.xlsx")
