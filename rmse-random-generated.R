

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: July 2021

# Load data
d = read_csv("effort-neuro-data-22-07-21.csv") %>%
  clean_names() %>%
  mutate(
    likert = as.factor(likert),
    omin = as.factor(omni),
    omni = recode_factor(omni,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    rpe = as.factor(rpe),
    rpe = recode_factor(rpe,'1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    fatigue = as.factor(fatigue),
    fatigue = recode_factor(fatigue,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    vas = vas*100
  )

# Random numbers 1 to 10
set.seed(123)
d$random = floor(runif(n = nrow(d), min = 1, max = 11))
d %>% ggplot() + geom_histogram(aes(x = random)) + theme_bw()
table(d$random)

# Transformations
df = d %>% mutate(
  mvc_log = log(mvc),
  participant = as.factor(participant),
  random = as.factor(random),
  log_resting_twitch = log(resting_twitch),
  bicep_rms_emg_log = log(bicep_rms_emg)
)

# Cross-validate: MVC torque
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("mvc_log ~ likert + (1|participant)",
           "mvc_log ~ omni + (1|participant)",
           "mvc_log ~ rpe + (1|participant)",
           "mvc_log ~ fatigue + (1|participant)",
           "mvc_log ~ bs(vas, knots = 5) + (1|participant)",
           "mvc_log ~ bs(test_block, knots = 5) + (1|participant)",
           "mvc_log ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'bs(vas,knots=5)' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE MVC torque") -> plot_1
plot_1





# Cross-validate: voluntary activation
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("va ~ likert + (1|participant)",
           "va ~ omni + (1|participant)",
           "va ~ rpe + (1|participant)",
           "va ~ fatigue + (1|participant)",
           "va ~ vas + (1|participant)",
           "va ~ bs(test_block, knots = 5) + (1|participant)",
           "va ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE voluntary activation") -> plot_2
plot_2





# Cross-validate: resting twitch torque
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("log_resting_twitch ~ likert + (1|participant)",
           "log_resting_twitch ~ omni + (1|participant)",
           "log_resting_twitch ~ rpe + (1|participant)",
           "log_resting_twitch ~ fatigue + (1|participant)",
           "log_resting_twitch ~ poly(vas, degree = 2) + (1|participant)",
           "log_resting_twitch ~ bs(test_block, knots = 5) + (1|participant)",
           "log_resting_twitch ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'poly(vas,degree=2)' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE resting twitch torque") -> plot_3
plot_3





# Cross-validate: EMG RMS amplitude
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("bicep_rms_emg_log ~ likert + (1|participant)",
           "bicep_rms_emg_log ~ omni + (1|participant)",
           "bicep_rms_emg_log ~ rpe + (1|participant)",
           "bicep_rms_emg_log ~ fatigue + (1|participant)",
           "bicep_rms_emg_log ~ vas + (1|participant)",
           "bicep_rms_emg_log ~ poly(test_block, degree = 2) + (1|participant)",
           "bicep_rms_emg_log ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'poly(test_block,degree=2)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE EMG RMS amplitude") -> plot_4
plot_4





# Cross-validate: MEP/Mmax area
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("bicep_mep_to_mmax_area ~ likert + (1|participant)",
           "bicep_mep_to_mmax_area ~ omni + (1|participant)",
           "bicep_mep_to_mmax_area ~ rpe + (1|participant)",
           "bicep_mep_to_mmax_area ~ fatigue + (1|participant)",
           "bicep_mep_to_mmax_area ~ vas + (1|participant)",
           "bicep_mep_to_mmax_area ~ bs(test_block, knots = 5) + (1|participant)",
           "bicep_mep_to_mmax_area ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'bs(test_block,knots=5)' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE MEP/Mmax area") -> plot_5
plot_5





# Cross-validate: silent period duration
set.seed(123)

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("bicep_silent_period_duration ~ likert + (1|participant)",
           "bicep_silent_period_duration ~ omni + (1|participant)",
           "bicep_silent_period_duration ~ rpe + (1|participant)",
           "bicep_silent_period_duration ~ fatigue + (1|participant)",
           "bicep_silent_period_duration ~ vas + (1|participant)",
           "bicep_silent_period_duration ~ test_block + (1|participant)",
           "bicep_silent_period_duration ~ random")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum %>%
  mutate(model = recode_factor(Fixed, 'rpe' = 'RPE', 'fatigue' = 'Fatigue', 'omni' = 'OMNI', 'test_block' = 'Base', 'likert' = 'Likert', 'vas' = 'VAS')) %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(model, -RMSE))) +
  theme_bw(base_size = 8) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(y = "Model") +
  facet_grid(~"RMSE silent period duration") -> plot_6
plot_6





# Panel plot
plot_grid(plot_1, plot_4, plot_2, plot_5, plot_3, plot_6,
          ncol = 2, 
          nrow = 3,
          labels = c('A','D','B','E','C','F'),
          align = 'v',
          axis = "lr",
          label_size = 10)
ggsave(file = "supplement-rmse-random-generated.png", units="in", width = 5, height = 5, dpi = 300)

