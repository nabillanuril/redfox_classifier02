library(tidyverse)
library(warbleR)
library(openxlsx)
library(rstatix)

# set working directory
setwd("F:/MSc Ecology & Data Science Research")

# calculate SNR for consecutive calls with distance < 0.02 
# calculate distance between calls and assign those with less than 0.02s 
# time difference into a column of group ID
red_foxSNR <- function(df_list, mar = 0.02, type = 2){
  
  seltab_list <- lapply(df_list, function(df) {
    df %>% 
      arrange(start) %>% 
      mutate(
        time_gap = start - lag(end),
        time_gap = ifelse(is.na(time_gap), mar, time_gap), # Fix NA
        # cumsum here calculates how many TRUE present with the  given condition
        # so calls with a time gap less than 0.02 will be counted as FALSE and will be in the
        # same group as the previous TRUE. Those in the same group will have similar SNR
        group_id = cumsum(time_gap >= mar)
      )
  })
  
  # merge calls in the same group ID into one window
  merged <- lapply(seltab_list, function(df) {
    df %>%
      # grouping allows similar calculations across group members
      group_by(group_id, sound.files, channel, path) %>%
      summarise(
        start = min(start),
        end = max(end),
        # create a new select column that has the value of group ID
        # to be recognised by warbleR
        selec = first(group_id),
        # drop group after grouping
        .groups = "drop"
      )
  })
  
  # calculate SNR for the new window
  group_snr <- lapply(seq_along(merged), function(i){
    sig2noise(
      X = merged[[i]], 
      mar = mar, 
      type = type, 
      path = unique(merged[[i]]$path)
    )
  })
  
  # get the group SNR attached to each overlapping call
  df_list_snr <- lapply(seq_along(seltab_list), function(i) {
    df <- seltab_list[[i]]
    snr_df <- group_snr[[i]]
    
    df %>%
      left_join(
        snr_df %>% select(selec, SNR) %>% rename(group_id = selec),
        by = "group_id"
      )
  })
  
  return(df_list_snr)
}

# DR SNR ####
dr_snr <- red_foxSNR(seltab_sp_dr)

# BL SNR ####
bl_snr <- red_foxSNR(seltab_sp_bl)

# XC SNR ####
xc_snr <- red_foxSNR(seltab_sp_xc)

# visualizing SNR range ####
# combine dr and bl clips
site_snr <- c(dr_snr, bl_snr) %>% bind_rows()

# get summary stats for site_domain
get_summary_stats(site_snr)

# combine xc clips
public_snr <- xc_snr %>% bind_rows()

# get summary stats for public_domain
get_summary_stats(public_snr)

# filter barks and whines
site_bark <- site_snr %>% filter(Call.Type == "Bark")
site_whine <- site_snr %>% filter(Call.Type == "Whine")
public_bark <- public_snr %>% filter(Call.Type == "Bark")
public_whine <- public_snr %>% filter(Call.Type == "Whine")

# split SNR to high and low groups by the mean
SNR_mean <- mean(public_snr$SNR, na.rm = TRUE)
print(SNR_mean)

# test if the different groups are different from each other
public_snr <- public_snr %>% 
  mutate(SNR_cat = ifelse(SNR < SNR_mean, "Low", 
                          ifelse(SNR > SNR_mean, "High", NA_character_)))

site_snr <- site_snr %>% 
  mutate(SNR_cat = ifelse(SNR < SNR_mean, "Low", 
                          ifelse(SNR > SNR_mean, "High", NA_character_)))

t.test(public_snr$SNR ~ public_snr$SNR_cat, na.action = na.omit)

# combine all factors for public recording
# public domain
public_strong <- public_snr %>% 
  # subset column with similar rows
  subset(select = c("sound.files", "selec", "SNR_cat", "time_gap","group_id","SNR"))

public_domain <- seltab_weak_xc %>% 
  bind_rows() %>% 
  # merge by similar row
  merge(public_strong, by = c("sound.files", "selec")) %>% 
  bind_rows(public_snr)

# site domain
site_strong <- site_snr %>% 
  # subset column with similar rows
  subset(select = c("sound.files", "selec", "SNR_cat", "time_gap","group_id","SNR"))

df_weak_dr <- seltab_weak_dr %>% bind_rows()
df_weak_bl <- seltab_weak_bl %>% bind_rows()

site_domain <- df_weak_dr %>% 
  bind_rows(df_weak_bl) %>% 
  merge(site_strong, by = c("sound.files", "selec")) %>% 
  bind_rows(site_snr)

# save the seltab_list as an Excel file so that it can be read in Python
write.xlsx(public_domain, "F:/MSc Ecology & Data Science Research/Metadata/public_domainSNR.xlsx")

