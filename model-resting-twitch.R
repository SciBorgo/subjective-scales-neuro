

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: May 2021

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

# Missing data
vis_dat(d)

# Outcome: Resting twitch torque
fit_base = lmer(log(resting_twitch) ~ bs(test_block, knots = 5) + (1|participant), data = d)
fit_likert = lmer(log(resting_twitch) ~ likert + (1|participant), data = d)
fit_omni = lmer(log(resting_twitch) ~ omni + (1|participant), data = d)
fit_rpe = lmer(log(resting_twitch) ~ rpe + (1|participant), data = d)
fit_fatigue = lmer(log(resting_twitch) ~ fatigue + (1|participant), data = d)
fit_vas = lmer(log(resting_twitch) ~ poly(vas, degree = 2) + (1|participant), data = d)

# Cross-validate
set.seed(123)

df = d %>% mutate(
  participant = as.factor(participant),
  log_resting_twitch = log(resting_twitch)
  )

data = fold(df,
            k = 10,
            num_fold_cols = 5,
            cat_col = 'participant')

models = c("log_resting_twitch ~ likert + (1|participant)",
           "log_resting_twitch ~ omni + (1|participant)",
           "log_resting_twitch ~ rpe + (1|participant)",
           "log_resting_twitch ~ fatigue + (1|participant)",
           "log_resting_twitch ~ poly(vas, degree = 2) + (1|participant)",
           "log_resting_twitch ~ bs(test_block, knots = 5) + (1|participant)")

cv_sum = cross_validate(data,
                        formulas = models,
                        fold_cols = c('.folds_1','.folds_2','.folds_3','.folds_4','.folds_5'),
                        family = 'gaussian',
                        REML = T,
                        rm_nc = T,
                        metrics = list(RMSE = T, r2m = T, r2c = T, all = F))

cv_sum
write.csv(cv_sum[,1:4], file = "pred-resting-twitch-performance.csv", row.names = F)


cv_sum %>%
  ggplot() +
  geom_point(aes(x = RMSE, y = reorder(Fixed, -RMSE))) +
  theme_bw(base_size = 12) +
  facet_grid(~"RMSE Resting Twitch Torque") -> plot_rmse
plot_rmse







# Fitted values: Likert scale
plot(fit_likert)
qqnorm(residuals(fit_likert)); qqline(residuals(fit_likert))
qqPlot(residuals(fit_likert))
hist(residuals(fit_likert), breaks = 40)

(refgrid <- list(likert=c('1','2','3','4','5','6','7')))
mar_ef_68 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=likert,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  geom_line(alpha = 0.8, size = 0.25) +
  labs(y = "Resting Twitch", x = "Likert Scale (1-7)") +
  theme_bw(base_size = 12) +
  facet_grid(~"Likert Scale") -> plot_likert
plot_likert


# Fitted values: OMNI scale
plot(fit_omni)
qqPlot(residuals(fit_omni))
hist(residuals(fit_omni), breaks = 40)

(refgrid <- list(omni=c('0','1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_omni, ~ omni, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_omni, ~ omni, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=omni,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  geom_line(alpha = 0.8, size = 0.25) +
  labs(y = "Resting Twitch", x = "OMNI Scale (0-10)") +
  theme_bw(base_size = 12) +
  facet_grid(~"OMNI Scale") -> plot_omni
plot_omni



# Fitted values: RPE scale
plot(fit_rpe)
qqPlot(residuals(fit_rpe))
hist(residuals(fit_rpe), breaks = 40)

(refgrid <- list(rpe=c('1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=rpe,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  geom_line(alpha = 0.8, size = 0.25) +
  labs(y = "Resting Twitch", x = "RPE Scale (1-10)") +
  theme_bw(base_size = 12) +
  facet_grid(~"RPE Scale") -> plot_rpe
plot_rpe



# Fitted values: Fatigue scale
plot(fit_fatigue)
qqPlot(residuals(fit_fatigue))
hist(residuals(fit_fatigue), breaks = 40)

(refgrid <- list(fatigue=c('0','1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=fatigue,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  geom_line(alpha = 0.8, size = 0.25) +
  labs(y = "Resting Twitch", x = "Fatigue Scale (0-10)") +
  theme_bw(base_size = 12) +
  facet_grid(~"Fatigue Scale") -> plot_fatigue
plot_fatigue



# Fitted values: VAS scale
d %>% ggplot() +
  geom_line(aes(x = vas, y = mvc, group = participant, alpha = 0.2)) +
  theme_bw()

plot(fit_vas)
qqPlot(residuals(fit_vas))
hist(residuals(fit_vas), breaks = 40)

(refgrid <- list(vas=seq(0,100,by = 1)))
mar_ef_68 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.95)

# With individual data points
ggplot(data=mar_ef_95, aes(x = vas,y=exp(yvar))) + 
  geom_line() + 
  geom_ribbon(data = mar_ef_95, aes(ymax = exp(UCL), ymin = exp(LCL), alpha = 0.075)) +
  geom_ribbon(data = mar_ef_68, aes(ymax = exp(UCL), ymin = exp(LCL), alpha = 0.075)) +
  #geom_point(data = d, aes(x = vas, y = resting_twitch, group = participant)) +
  labs(y = "Resting Twitch", x = "Visual Analogue Scale (0-100)") +
  facet_grid(~"Visual Analogue Scale") +
  theme_bw(base_size = 12) +
  guides(alpha = F) +
  ylim(7,18) -> plot_vas
plot_vas





# Panel plot
plot_grid(plot_likert, plot_omni, plot_rpe, plot_fatigue, plot_vas,
          ncol = 2, 
          nrow = 3,
          #labels = c('(A)','(B)','(C)','(D)','(E)','(F)','(G)','(H)','(I)'),
          align = 'v',
          axis = "lr",
          label_size = 16)
ggsave(file = "resting-twitch-prediction.png", units="in", width = 7.5, height = 9, dpi = 300)




## Conclusion:
## The OMNI and fatigue scale do about the same, they only do marginally better than the other scales.



