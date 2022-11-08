

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


# Outcome: EMG RMS amplitude
fit_likert = lmer(log(bicep_rms_emg) ~ likert + (1|participant), data = d)
fit_rpe = lmer(log(bicep_rms_emg) ~ rpe + (1|participant), data = d)

# Fitted values: Likert scale
(refgrid <- list(likert=c('1','2','3','4','5','6','7')))
mar_ef_68 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=likert,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "EMG RMS amplitude (mV)", x = "Likert scale (1-7)") +
  theme_bw(base_size = 10) -> plot_6
plot_6

# Fitted values: RPE scale
(refgrid <- list(rpe=c('1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=rpe,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "EMG RMS amplitude (mV)", x = "RPE (1-10)") +
  theme_bw(base_size = 10) -> plot_7
plot_7




# Outcome: MEP/Mmax area
fit_likert = lmer(bicep_mep_to_mmax_area ~ likert + (1|participant), data = d)
fit_vas = lmer(bicep_mep_to_mmax_area ~ vas + (1|participant), data = d) # linear term better than spline (BIC = -3 vs. 10)

# Fitted values: Likert scale
(refgrid <- list(likert=c('1','2','3','4','5','6','7')))
mar_ef_68 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=likert,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "MEP/Mmax area (%)", x = "Likert scale (1-7)") +
  scale_y_continuous(limits = c(0.65,1.05), breaks = seq(0.65,1.05, 0.1)) +
  theme_bw(base_size = 10) -> plot_8
plot_8

# Fitted values: VAS
(refgrid <- list(vas=seq(0,100,by = 1)))
mar_ef_68 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.95)

# With individual data points
ggplot(data=mar_ef_95, aes(x = vas,y=(yvar))) + 
  geom_line() + 
  geom_ribbon(data = mar_ef_95, aes(ymax = (UCL), ymin = (LCL), alpha = 0.075)) +
  geom_ribbon(data = mar_ef_68, aes(ymax = (UCL), ymin = (LCL), alpha = 0.075)) +
  #geom_point(data = d, aes(x = vas, y = bicep_mep_to_mmax_area, group = participant)) +
  labs(y = "MEP/Mmax area (%)", x = "Visual analogue scale (0-100)") +
  scale_y_continuous(limits = c(0.65,1.05), breaks = seq(0.65,1.05, 0.1)) +
  theme_bw(base_size = 10) +
  guides(alpha = "none") -> plot_9
plot_9


# Outcome: Silent period duration
fit_vas = lmer(bicep_silent_period_duration ~ vas + (1|participant), data = d)

# Fitted values: VAS
(refgrid <- list(vas=seq(0,100,by = 1)))
mar_ef_68 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_vas, ~ vas, at = refgrid, CIs = T, plotit = F, level = 0.95)

# With individual data points
ggplot(data=mar_ef_95, aes(x = vas,y=(yvar))) + 
  geom_line() + 
  geom_ribbon(data = mar_ef_95, aes(ymax = (UCL), ymin = (LCL), alpha = 0.075)) +
  geom_ribbon(data = mar_ef_68, aes(ymax = (UCL), ymin = (LCL), alpha = 0.075)) +
  #geom_point(data = d, aes(x = vas, y = bicep_silent_period_duration, group = participant)) +
  labs(y = "Silent period duration (ms)", x = "Visual analogue scale (0-100)") +
  theme_bw(base_size = 10) +
  guides(alpha = "none") -> plot_10
plot_10



# Panel plot: EMG-derived variables
plot_grid(plot_6,
          plot_7,
          plot_8,
          plot_9,
          plot_10,
          ncol = 2, 
          nrow = 3,
          labels = c('Ai','Aii','Bi','Bii','C'),
          align = 'v',
          axis = "lr",
          label_size = 12,
          scale = 0.95)
ggsave(file = "figure-6.png", units="in", width = 6, height = 7, dpi = 300)




#### END
