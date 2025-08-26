library(tibble)
library(tidyverse)

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

root <- "F:/MSc Ecology & Data Science Research/6. evaluation metrics/1. Wilcoxon"
# Create file paths based on the combination
all_combinations <- all_combinations %>%
  mutate(
    combination = paste0(Domain, "_", Quality, "_", Label),
    folder_path = file.path(root, combination)
  ) %>%
  mutate(
    Domain  = recode(Domain,  "dr" = "Dartmoor",    "xc" = "Xeno-Canto"),
    Quality = recode(Quality, "highsnr" = "High SNR", "lowsnr" = "Low SNR"),
    Label   = recode(Label,   "strong" = "Strong Label", "weak" = "Weak Label")
  )

dirs <- all_combinations$folder_path

# list all result_table in the folder and combine all df
all_data <- purrr::map_dfr(
  dirs,
  function(folder_path) {
    csv_files <- list.files(
      path = folder_path,
      pattern = "^results_table_\\d{2}\\.csv$",
      full.names = TRUE
    )
    purrr::map_dfr(
      csv_files,
      function(csv_file) {
        csv_name <- basename(csv_file)
        csv_number <- sub("^.*results_table_(\\d{2})\\.csv$", "\\1", csv_name)
        df <- read.csv(csv_file)
        metrics <- setNames(df$Overall, df$X)
        as.data.frame(t(metrics)) %>%
          mutate(
            combination = basename(folder_path),
            test_set = csv_number,
            .before = 1
          )
      }
    )
  }
)

all_data <- left_join(all_data, all_combinations, by = "combination") %>%
  mutate(Domain_Quality = paste0(Domain, "_", Quality),
         Domain_Label = paste0(Domain, "_", Label),
         Quality_Label = paste0(Quality, "_", Label),
         .after = combination) %>%
  relocate(Domain, Quality, Label, .after = Quality_Label)

# Calculate the mean for each metric
all_data_means <- all_data %>%
  group_by(combination) %>%
  summarise(mean_AUROC = mean(AUROC, na.rm = TRUE),
            mean_AP = mean(AP, na.rm = TRUE),
            mean_F1 = mean(F1, na.rm = TRUE),
            mean_Accuracy = mean(Accuracy, na.rm = TRUE),
            mean_Precision = mean(Precision, na.rm = TRUE),
            mean_Recall = mean(Recall, na.rm = TRUE),
            .groups = "drop")



# figure trend xc and dr ####
all_data_long <- all_data %>%
  pivot_longer(
    cols = c(AP, F1),
    names_to = "Metric",
    values_to = "Score"
  )

ggplot(
  all_data_long,
  aes(
    x = Quality_Label,
    y = Score,
    color = Metric,
    shape = Domain,         
    group = interaction(Metric, Domain)
  )
) + 
  geom_jitter(
    alpha = 0.4, size = 1,
    position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5)
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 5,
    aes(shape = Domain,
        fill= Domain,
        color = Metric),
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Model Performance of Dartmoor and Xeno-Canto Domain \nacross other Combination of Data Characteristics",
    x = "Quality & Label",
    y = "Metric Value",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) +
  scale_x_discrete(
    labels = c(
      "High SNR_Strong Label" = "High SNR, \nStrong Label",
      "Low SNR_Strong Label" = "Low SNR, \nStrong Label",
      "High SNR_Weak Label"  = "High SNR, \nWeak Label",
      "Low SNR_Weak Label"   = "Low SNR, \nWeak Label"
    ))


# wilcoxon dartmoor-xc ####
all_data_wide <- all_data_long %>%
  pivot_wider(id_cols = c(Quality_Label, Metric, test_set),
              names_from = Domain, 
              values_from = Score)


results_dr_vs_xc <- all_data_wide %>%
  filter(Metric == 'AP') %>%
  group_by(Quality_Label, Metric) %>%
  summarise(
    test = list(wilcox.test(Dartmoor, `Xeno-Canto`, paired = TRUE, exact = FALSE)),
    .groups = "drop")


# figure trend high snr and low snr ####
all_data_long <- all_data %>%
  pivot_longer(
    cols = c(AP, F1),
    names_to = "Metric",
    values_to = "Score"
  )
ggplot(
  all_data_long,
  aes(
    x = Domain_Label,
    y = Score,
    color = Metric,
    shape = Quality,         # Optional: shape for Domain, or use fill
    group = interaction(Metric, Quality)
  )
) +
  geom_jitter(
    alpha = 0.4, size = 1,
    position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5)
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 5,
    aes(shape = Quality,
        fill= Quality,
        color = Metric),
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Model Performance Comparison of High SNR and Low SNR Training Data \nacross Combination of Other Data Characteristics",
    x = "Domain & Label",
    y = "Metric Value",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = c(
      "Dartmoor_Strong Label" = "Dartmoor, \nStrong Label",
      "Dartmoor_Weak Label" = "Dartmoor, \nWeak Label",
      "Xeno-Canto_Strong Label"  = "Xeno-Canto, \nStrong Label",
      "Xeno-Canto_Weak Label"   = "Xeno-Canto, \nWeak Label"
    )
  ) 


# wilcoxon high-low snr ####
all_data_wide <- all_data_long %>%
  pivot_wider(id_cols = c(Domain_Label, Metric, test_set),
              names_from = Quality, 
              values_from = Score)


results_high_vs_low <- all_data_wide %>%
  filter(Metric == 'AP') %>%
  group_by(Domain_Label, Metric) %>%
  summarise(
    test = list(wilcox.test(`High SNR`, `Low SNR`, paired = TRUE, exact = FALSE)),
    .groups = "drop")


# figure trend strong and weak label ####
all_data_long <- all_data %>%
  pivot_longer(
    cols = c(AP, F1),
    names_to = "Metric",
    values_to = "Score"
  )
ggplot(
  all_data_long,
  aes(
    x = Domain_Quality,
    y = Score,
    color = Metric,
    shape = Label,         # Optional: shape for Domain, or use fill
    group = interaction(Metric, Label)
  )
) +
  geom_jitter(
    alpha = 0.4, size = 1,
    position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 5,
    aes(shape = Label,
        fill = Label,
        color = Metric),
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Model Performance Comparison of Strong-Labelled and Weak-Labelled \nTraining Data cross Combination of Other Data Characteristics",
    x = "Domain & Quality",
    y = "Metric Value",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = c(
      "Dartmoor_High SNR" = "Dartmoor, \nHigh SNR",
      "Dartmoor_Low SNR" = "Dartmoor, \nLow SNR",
      "Xeno-Canto_High SNR"  = "Xeno-Canto, \nHigh SNR",
      "Xeno-Canto_Low SNR"   = "Xeno-Canto, \nLow SNR"
    )
  ) 


# wilcoxon strong-weak label ####
all_data_wide <- all_data_long %>%
  pivot_wider(id_cols = c(Domain_Quality, Metric, test_set),
              names_from = Label, 
              values_from = Score)


results_strong_vs_weak <- all_data_wide %>%
  filter(Metric == 'AP') %>%
  group_by(Domain_Quality, Metric) %>%
  summarise(
    test = list(wilcox.test(`Strong Label`, `Weak Label`, paired = TRUE, exact = FALSE)),
    .groups = "drop")


# All Metrics ####
all_data_long <- all_data %>%
  pivot_longer(
    cols = c(AP, AUROC, Precision, Recall, F1, Accuracy),
    names_to = "Metric",
    values_to = "Score"
  )


ggplot(
  all_data_long,
  aes(
    x = combination,
    y = Score,
    color = Metric,
    group = interaction(Metric, combination)
  )
) +
  geom_jitter(
    alpha = 0.4, size = 1,
    position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 5,
    aes(color = Metric),
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Model Performance across All Combination of Data Characteristics",
    x = "Domain & Quality & Label",
    y = "Metric Value",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = c(
      "dr_highsnr_strong" = "Dartmoor, \nHigh SNR, \nStrong Label",
      "dr_highsnr_weak" = "Dartmoor, \nHigh SNR, \nWeak Label",
      "dr_lowsnr_strong" = "Dartmoor, \nLow SNR, \nStrong Label",
      "dr_lowsnr_weak" = "Dartmoor, \nLow SNR, \nWeak Label",
      "xc_highsnr_strong"  = "Xeno-Canto, \nHigh SNR, \nStrong Label",
      "xc_highsnr_weak" = "Xeno-Canto, \nHigh SNR, \nWeak Label",
      "xc_lowsnr_strong"  = "Xeno-Canto, \nLow SNR, \nStrong Label",
      "xc_lowsnr_weak"  = "Xeno-Canto, \nLow SNR, \nWeak Label"
    )
  ) 


# overall AP ####
all_data_long <- all_data %>%
  pivot_longer(
    cols = c(AP),
    names_to = "Metric",
    values_to = "Score"
  )

ggplot(
  all_data_long,
  aes(
    x = Label,
    y = Score,
#    color = Metric,
    shape = Domain,       
    group = interaction(Metric, Domain)
  )
) +
  geom_jitter(
    alpha = 0.4, size = 1,
    colour = "#5386b0",
    position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5)
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 5,
    aes(shape = Domain,
#        color = Metric,
        fill= Domain),
    colour = "#5386b0",
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Quality) +
  labs(
    title = "Average Precision (AP) across \nCombination of Data Characteristics",
    x = "Label",
    y = "Metric Value",
    hjust = 0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  )


