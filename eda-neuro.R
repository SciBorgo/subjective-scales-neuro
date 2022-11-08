

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: June 2021

# Load data
d = read_csv("effort-neuro-data-22-07-21.csv") %>%
  clean_names()

# Missing data
d %>% vis_miss()
d %>% vis_dat()

# Variable names
sort(names(d))

# Theme
rpe_theme = theme(panel.grid.minor.x = element_blank())

# plot_mvc
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(mvc))
d %>% ggplot() +
  geom_line(aes(x = test_block, y = mvc, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "MVC torque (N·m)", x = "Contraction block") +
  guides(alpha = "none") -> plot_mvc

# plot_va
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(va))
d %>% ggplot() +
  geom_line(aes(x = test_block, y = va, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "Voluntary activation (%)", x = "Contraction block") +
  guides(alpha = "none") -> plot_va

# plot_rt
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(resting_twitch))
d %>% ggplot() +
  geom_line(aes(x = test_block, y = resting_twitch, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "Resting twitch torque (N·m)", x = "Contraction block") +
  guides(alpha = "none") -> plot_rt

# plot_bicep_mep_to_mmax_area
dsub = d %>% filter(bicep_mep_to_mmax_area < 2)
dsum = dsub %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_mep_to_mmax_area))
dsub %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_mep_to_mmax_area, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "MEP/Mmax area", x = "Contraction block") +
  guides(alpha = "none") -> plot_bicep_mep_to_mmax_area
plot_bicep_mep_to_mmax_area

# plot_bicep_silent_period_duration
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_silent_period_duration))
d %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_silent_period_duration, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "Silent period", x = "Contraction block")  +
  guides(alpha = "none") -> plot_bicep_silent_period_duration

# plot_bicep_rms_emg
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_rms_emg))
d %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_rms_emg, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  rpe_theme +
  labs(y = "EMG RMS amplitude", x = "Contraction block") +
  guides(alpha = "none") -> plot_bicep_rms_emg


# Panel plot
title_one <- ggdraw() +
  draw_label(
    "Torque-derived variables",
    fontface = 'bold',
    x = 0.55)

title_two <- ggdraw() + 
  draw_label(
    "EMG-derived variables",
    fontface = 'bold',
    x = 0.55)

plot_grid(title_one,
          title_two,
          ncol = 2,
          nrow = 1) -> titles

plot_grid(plot_mvc,
          plot_bicep_rms_emg,
          plot_va,
          plot_bicep_mep_to_mmax_area,
          plot_rt,
          plot_bicep_silent_period_duration,
          ncol = 2, 
          nrow = 3,
          labels = c('A','D','B','E','C','F'),
          align = 'v',
          axis = "lr",
          label_size = 14,
          scale = 0.95) -> plots

plot_grid(titles,
          plots,
          ncol = 1,
          rel_heights = c(0.035,1))

ggsave(file = "supplement-eda.png", units="in", width = 8, height = 8, dpi = 300)





#### END
