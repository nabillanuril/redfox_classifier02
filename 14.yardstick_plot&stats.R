library(readxl)


metrics_by_set <- read_xlsx("F:/MSc Ecology & Data Science Research/Metadata/evaluation_metrics.xlsx")

df <- tibble(
  Domain = c("dr", "xc"),
  Quality = c("highsnr", "lowsnr"),
  Label = c("strong", "weak"))

# Generate all possible combinations
all_combinations <- expand_grid(
  Domain = df$Domain,
  Quality = df$Quality,
  Label = df$Label
) %>% unique()

all_combinations <- all_combinations %>%
  mutate(
    combination = paste0(Domain, "_", Quality, "_", Label)
  ) %>%
  mutate(
    Domain  = recode(Domain,  "dr" = "Dartmoor",    "xc" = "Xeno-Canto"),
    Quality = recode(Quality, "highsnr" = "High SNR", "lowsnr" = "Low SNR"),
    Label   = recode(Label,   "strong" = "Strong Label", "weak" = "Weak Label"))

all_data <- left_join(metrics_by_set, all_combinations, by = "combination") %>%
  mutate(Domain_Quality = paste0(Domain, "_", Quality),
         Domain_Label = paste0(Domain, "_", Label),
         Quality_Label = paste0(Quality, "_", Label),
         .after = combination) %>%
  relocate(Domain, Quality, Label, .after = Quality_Label)