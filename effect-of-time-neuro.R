

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: July 2021

# Load data
d = read_csv("effort-neuro-data-22-07-21.csv") %>%
  clean_names()

# Missing data
d %>% vis_miss()
d %>% vis_dat()

# Variable names
sort(names(d))

# Theme
rpe_theme = theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  strip.text.x = element_text(size = 10))


# MVC
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(mvc))

d %>% ggplot() +
  geom_line(aes(x = test_block, y = mvc, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit = lmer(log(mvc) ~ bs(test_block, knots = 5) + (1|participant), data = d)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "MVC torque (N·m)", x = "Contraction block") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw(base_size = 10) +
  rpe_theme +
  geom_segment(aes(x=2, xend=10, y=55, yend=55), colour = "gray60", size = 0.35) +
  annotate(geom="text", x=6, y=56.5, label="*",size=5) -> plot_mvc
plot_mvc





# VA
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(va))

d %>% ggplot() +
  geom_line(aes(x = test_block, y = va, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit = lmer(va ~ bs(test_block, knots = 5) + (1|participant), data = d)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

# Look at refitting with outliers removed

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "Voluntary activation (%)", x = "Contraction block") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw(base_size = 10) +
  rpe_theme +
  geom_segment(aes(x=2, xend=10, y=95, yend=95), colour = "gray60", size = 0.35) +
  annotate(geom="text", x=6, y=96, label="*",size=5) -> plot_va
plot_va



# Resting twitch
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(resting_twitch))

d %>% ggplot() +
  geom_line(aes(x = test_block, y = resting_twitch, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit = lmer(log(resting_twitch) ~ bs(test_block, knots = 5) + (1|participant), data = d)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "Resting twitch torque (N·m)", x = "Contraction block")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw(base_size = 10) +
  rpe_theme +
  geom_segment(aes(x=4, xend=10, y=15, yend=15), colour = "gray60", size = 0.35) +
  annotate(geom="text", x=7, y=15.35, label="*",size=5) -> plot_rt
plot_rt









# Biceps MEP/ Mmax Area
dsub = d %>% filter(bicep_mep_to_mmax_area < 2)

dsum = dsub %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_mep_to_mmax_area, na.rm = T))

dsub %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_mep_to_mmax_area, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit = lmer(bicep_mep_to_mmax_area ~ bs(test_block, knots = 5) + (1|participant), data = dsub)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "MEP/Mmax area (%)", x = "Contraction block")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw(base_size = 10) +
  rpe_theme -> plot_bicep_mep_to_mmax_area
plot_bicep_mep_to_mmax_area








# Biceps rms EMG
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_rms_emg, na.rm = T))

d %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_rms_emg, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit1 = lmer(log(bicep_rms_emg) ~ poly(test_block, degree = 2) + (1|participant), data = d)
fit2 = lmer(log(bicep_rms_emg) ~ bs(test_block, knots = 5) + (1|participant), data = d)
anova(fit1,fit2) # Compare models

fit = fit1 # best fit based on BIC

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=exp(yvar))) +
  geom_point(size = 2) +
  #geom_jitter(data = d, aes(x = test_block, y = bicep_rms_emg), width = 0.1) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "EMG RMS amplitude (mV)", x = "Contraction block")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(limits = c(0.25,0.75), breaks = seq(0, 1, by = 0.1)) +
  theme_bw(base_size = 10) +
  rpe_theme -> plot_bicep_rms_emg
plot_bicep_rms_emg






# Biceps Silent Period Duration
dsum = d %>% group_by(test_block) %>%
  summarise(mu = mean(bicep_silent_period_duration, na.rm = T))

d %>% ggplot() +
  geom_line(aes(x = test_block, y = bicep_silent_period_duration, group = participant, alpha = 0.2)) +
  geom_line(data = dsum, aes(x = test_block, y = mu), colour = "red", size = 2) +
  theme_bw()

# Model
fit1 = lmer(bicep_silent_period_duration ~ test_block + (1|participant), data = d)
fit2 = lmer(bicep_silent_period_duration ~ bs(test_block, knots = 5) + (1|participant), data = d)
anova(fit1,fit2) # Compare; is spline term needed for this variable?

fit = fit1 # best fit based on BIC

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
qqPlot(residuals(fit))
hist(residuals(fit), breaks = 40)

summary(fit)

# Look more closely at residuals from origional model
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>% dplyr::select(participant,test_block)
d_outlier
d_no_out <- d %>% anti_join(d_outlier, by = c("participant","test_block")) # keep rows without matching ID

# Refit model
fit_refit <- lmer(bicep_silent_period_duration ~ test_block + (1|participant), data = d_no_out)

qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))
qqPlot(residuals(fit_refit))
summary(fit_refit)

# Use original model, as the substantive conclusions remain the same when outliers, as defined by the method above, are removed.

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(0,10, by = 1)))
mar_ef_68 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=test_block,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "Silent period duration (ms)", x = "Contraction block")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(limits = c(0.115,0.26), breaks = seq(0.1, 0.26, by = 0.03)) +
  theme_bw(base_size = 10) +
  rpe_theme +
  geom_segment(aes(x=1, xend=10, y=0.25, yend=0.25), colour = "gray60", size = 0.35) +
  annotate(geom="text", x=5.5, y=0.255, label="*",size=5) -> plot_bicep_silent_period_duration
plot_bicep_silent_period_duration



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

ggsave(file = "figure-2.png", units="in", width = 8, height = 8, dpi = 300)





#### END











