# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: May 2021

# Libraries
library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(naniar)
library(visdat)
library(ggplot2)
library(lmerTest)
library(splines)
library(emmeans)
library(here)
library(cowplot)

# Load data
d = read_csv("effort-neuro-data-18-05-21.csv") %>%
  clean_names() %>%
  mutate(
    likert = as.factor(likert),
    omin = as.factor(omni),
    omni = recode_factor(omni,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    rpe = as.factor(rpe),
    rpe = recode_factor(rpe,'1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    fatigue = as.factor(fatigue),
    fatigue = recode_factor(fatigue,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9')
  )


# Outcome variable: MVC
# Likert scale
dsum = d %>% group_by(likert) %>%
  summarise(mu = mean(mvc, na.rm = T))
d %>% ggplot(aes(x = likert, y = mvc)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"MVC vs. Likert") -> plot_likert
plot_likert

# Fatigue scale
dsum = d %>% group_by(fatigue) %>%
  summarise(mu = mean(mvc, na.rm = T))
d %>% ggplot(aes(x = fatigue, y = mvc)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"MVC vs. Fatigue") -> plot_fatigue
plot_fatigue

# OMNI scale
dsum = d %>% group_by(omni) %>%
  summarise(mu = mean(mvc, na.rm = T))
d %>% ggplot(aes(x = omni, y = mvc)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"MVC vs. OMNI") -> plot_omni
plot_omni

# RPE scale
dsum = d %>% group_by(rpe) %>%
  summarise(mu = mean(mvc, na.rm = T))
d %>% ggplot(aes(x = rpe, y = mvc)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"MVC vs. RPE") -> plot_rpe
plot_rpe


# Panel plot
plot_grid(plot_rpe, plot_omni, plot_likert, plot_fatigue,
          ncol = 2, 
          nrow = 2,
          #labels = c('(A)','(B)','(C)','(D)'),
          #label_size = 16,
          align = 'v',
          axis = "lr")
ggsave(file = "mvc.png", units="in", width = 10, height = 7, dpi = 300)







# Outcome variable: Voluntary activation
# Likert scale
dsum = d %>% group_by(likert) %>%
  summarise(mu = mean(va, na.rm = T))
d %>% ggplot(aes(x = likert, y = va)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Voluntary Activation vs. Likert") -> plot_likert
plot_likert

# Fatigue scale
dsum = d %>% group_by(fatigue) %>%
  summarise(mu = mean(va, na.rm = T))
d %>% ggplot(aes(x = fatigue, y = va)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Voluntary Activation vs. Fatigue") -> plot_fatigue
plot_fatigue

# OMNI scale
dsum = d %>% group_by(omni) %>%
  summarise(mu = mean(va, na.rm = T))
d %>% ggplot(aes(x = omni, y = va)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Voluntary Activation vs. OMNI") -> plot_omni
plot_omni

# RPE scale
dsum = d %>% group_by(rpe) %>%
  summarise(mu = mean(va, na.rm = T))
d %>% ggplot(aes(x = rpe, y = va)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Voluntary Activation vs. RPE") -> plot_rpe
plot_rpe


# Panel plot
plot_grid(plot_rpe, plot_omni, plot_likert, plot_fatigue,
          ncol = 2, 
          nrow = 2,
          #labels = c('(A)','(B)','(C)','(D)'),
          #label_size = 16,
          align = 'v',
          axis = "lr")
ggsave(file = "voluntary-activation.png", units="in", width = 10, height = 7, dpi = 300)





# Outcome variable: Resting twitch torque
# Likert scale
dsum = d %>% group_by(likert) %>%
  summarise(mu = mean(resting_twitch, na.rm = T))
d %>% ggplot(aes(x = likert, y = resting_twitch)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(likert), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Resting Twitch vs. Likert") -> plot_likert
plot_likert

# Fatigue scale
dsum = d %>% group_by(fatigue) %>%
  summarise(mu = mean(resting_twitch, na.rm = T))
d %>% ggplot(aes(x = fatigue, y = resting_twitch)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(fatigue), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Resting Twitch vs. Fatigue") -> plot_fatigue
plot_fatigue

# OMNI scale
dsum = d %>% group_by(omni) %>%
  summarise(mu = mean(resting_twitch, na.rm = T))
d %>% ggplot(aes(x = omni, y = resting_twitch)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(omni), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Resting Twitch vs. OMNI") -> plot_omni
plot_omni

# RPE scale
dsum = d %>% group_by(rpe) %>%
  summarise(mu = mean(resting_twitch, na.rm = T))
d %>% ggplot(aes(x = rpe, y = resting_twitch)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.15, size = 2) +
  theme_bw(base_size = 14) +
  geom_line(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red")) +
  geom_point(data = dsum, aes(y = mu, x = as.numeric(rpe), colour = "red"), size = 2.5) +
  guides(colour = F) +
  facet_grid(~"Resting Twitch vs. RPE") -> plot_rpe
plot_rpe


# Panel plot
plot_grid(plot_rpe, plot_omni, plot_likert, plot_fatigue,
          ncol = 2, 
          nrow = 2,
          #labels = c('(A)','(B)','(C)','(D)'),
          #label_size = 16,
          align = 'v',
          axis = "lr")
ggsave(file = "resting-twitch.png", units="in", width = 10, height = 7, dpi = 300)


