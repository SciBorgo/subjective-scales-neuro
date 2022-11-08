

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: July 2021

# Load data: mvc
d_mvc = read_csv("pred-mvc-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'bs(vas,knots=5)' = 'VAS'))

d_mvc %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"MVC torque") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_mvc
plot_rmse_mvc

# Load data: va
d_va = read_csv("pred-va-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS'))

d_va %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"Voluntary activation") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_va
plot_rmse_va

# Load data: resting twitch torque
d_rt = read_csv("pred-resting-twitch-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'poly(vas,degree=2)' = 'VAS'))

d_rt %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"Resting twitch torque") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_rt
plot_rmse_rt

# Load data: EMG RMS amplitude
d_emg = read_csv("pred-rms-emg-amplitude-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'poly(test_block,degree=2)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS'))

d_emg %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"EMG RMS amplitude") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_emg
plot_rmse_emg

# Load data: MEP/Mmax area
d_mep = read_csv("pred-mep-to-mmax-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS'))

d_mep %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"MEP/Mmax area") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_mep
plot_rmse_mep

# Load data: Silent period
d_silent = read_csv("pred-silent-period-performance.csv") %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'test_block' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS'))

d_silent %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  facet_grid(~"Silent period duration") +
  labs(y = "Model") +
  theme(strip.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_rmse_silent
plot_rmse_silent


# Panel plot
plot_grid(plot_rmse_mvc, plot_rmse_emg, plot_rmse_va, plot_rmse_mep, plot_rmse_rt, plot_rmse_silent,
          ncol = 2, 
          nrow = 3,
          labels = c('A','D','B','E','C','F'),
          align = 'v',
          axis = "lr",
          label_size = 10)
ggsave(file = "figure-4.png", units="in", width = 5, height = 5, dpi = 300)


