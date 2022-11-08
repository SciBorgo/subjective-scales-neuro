

# Setwd once for data files
here::here()

# Neuromuscular variables
d_mvc = read.csv("mvc.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "mvc")

d_va = read.csv("va.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "va")

d_rest_twitch = read.csv("resting-twitch.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "resting_twitch")

d_bicep_mep_to_mmax_area = read.csv("biceps-mep-to-mmax-area.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "bicep_mep_to_mmax_area")

d_bicep_silent = read.csv("biceps-silent-period-duration.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "bicep_silent_period_duration")

d_bicep_rms_EMG = read.csv("biceps-rms-EMG.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "bicep_rms_EMG")


# Subjective variables
d_vas = read.csv("vas.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "vas")

d_omni = read.csv("omni.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "omni")

d_likert = read.csv("likert.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "likert")

d_rpe = read.csv("rpe.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "rpe")

d_borg = read.csv("borg-scale.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "borg_scale")

d_fatigue = read.csv("rating-of-fatigue.csv") %>% clean_names() %>%
  pivot_longer(!block, names_to = "participant", values_to = "fatigue")

# Merges
d1 = merge(d_mvc, d_va, by = c("participant","block"))
d2 = merge(d1, d_rest_twitch, by = c("participant","block"))
d3 = merge(d2, d_bicep_mep_to_mmax_area, by = c("participant","block"))
d4 = merge(d3, d_bicep_silent, by = c("participant","block"))
d5 = merge(d4, d_bicep_rms_EMG, by = c("participant","block"))
d6 = merge(d5, d_vas, by = c("participant","block"))
d7 = merge(d6, d_omni, by = c("participant","block"))
d8 = merge(d7, d_likert, by = c("participant","block"))
d9 = merge(d8, d_rpe, by = c("participant","block"))
d10 = merge(d9, d_borg, by = c("participant","block"))
d11 = merge(d10, d_fatigue, by = c("participant","block"))

# Final dataset; relabel blocks
dat = d11 %>%
  mutate(
    test_block = recode_factor(block, 'Baseline' = '0',
                        'Block 1' = '1',
                        'Block 2' = '2',
                        'Block 3' = '3',
                        'Block 4' = '4',
                        'Block 5' = '5',
                        'Block 6' = '6',
                        'Block 7' = '7',
                        'Block 8' = '8',
                        'Block 9' = '9',
                        'Block 10' = '10'),
    test_block = as.integer(test_block),
    test_block = test_block-1,
    omni = as.factor(omni),
    likert = as.factor(likert),
    borg_scale = as.factor(borg_scale),
    fatigue = as.factor(fatigue)
  )

# Save merged files
write.csv(dat, file = "effort-neuro-data-22-07-21.csv", row.names = F)

