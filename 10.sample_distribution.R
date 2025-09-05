library(ggplot2)
library(dplyr)
library(openxlsx)

# Figure 1 ####
# Figure. SNR spread used as the basis for thresholding between low and high SNR category
public_snr <- read.xlsx("F:/MSc Ecology & Data Science Research/Metadata/public_domainSNR.xlsx")

min_snr  <- min(public_snr$SNR, na.rm = TRUE)
mean_snr <- mean(public_snr$SNR, na.rm = TRUE)
max_snr  <- max(public_snr$SNR, na.rm = TRUE)

ggplot(public_snr, aes(x = SNR)) +
  geom_density(alpha = 0.5) + 
  geom_vline(aes(xintercept = mean(SNR)), 
             colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(
    breaks = c(min_snr, mean_snr, max_snr),
    labels = c(paste0("min = ", round(min_snr, 2)),
               paste0("mean = ", round(mean_snr, 2)),
               paste0("max = ", round(max_snr, 2)))
  ) +
  theme_minimal() +
  labs(
    title = "SNR distribution",
    x = "SNR (dB)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Figure 2 ####
# Figure. SNR spread across Dartmoor recording quality A, B, and C
calls <- c("Bark", "Whine")
calls_snr <- public_snr %>% filter(Call.Type %in% calls)
xc_list <- read.xlsx("F:/MSc Ecology & Data Science Research/1. Xenocanto_foreground_wav/xclist.xlsx") %>% 
  rename(sound.files = recording_name) %>%
  select(Quality, sound.files)
xc_snr <- calls_snr %>% merge(xc_list, by = "sound.files") 

ggplot(xc_snr, aes(x = SNR, color = Quality, fill = Quality)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Call.Type)
# remember that each quality group has different sample size


# Figure 2 ####
# Figure.  Clip length of strong labelled barks and whines
clip_bl <- seltab_sp_bl %>% bind_rows() 
clip_dr <- seltab_sp_dr %>% bind_rows()
clip_xc <- seltab_sp_xc %>% bind_rows()

strong_clip <- clip_xc %>% 
  bind_rows(clip_dr, clip_bl) %>% 
  mutate(length = end-start)

call_clips <- strong_clip %>% filter(Call.Type %in% calls)

ggplot(call_clips, aes(x = length, color = Call.Type, fill = Call.Type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Call Duration Distribution",
    x = "Call Duration (s)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    fill = "Call Type",
    color = "Call Type"
  )

