

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

# Outcome: MVC
fit_rpe = lmer(log(mvc) ~ rpe + (1|participant), data = d)
fit_fatigue = lmer(log(mvc) ~ fatigue + (1|participant), data = d)

# Fitted values: RPE scale
(refgrid <- list(rpe=c('1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_rpe, ~ rpe, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=rpe,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "MVC torque\n(N·m)", x = "RPE (1-10)") +
  theme_bw(base_size = 10) -> plot_1
plot_1

# Fitted values: Fatigue scale
(refgrid <- list(fatigue=c('0','1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=fatigue,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "MVC torque\n(N·m)", x = "Fatigue scale (0-10)") +
  theme_bw(base_size = 10) -> plot_2
plot_2





# Outcome: Voluntary activation
fit_likert = lmer(va ~ likert + (1|participant), data = d)
fit_fatigue = lmer(va ~ fatigue + (1|participant), data = d)

# Fitted values: Likert scale
(refgrid <- list(likert=c('1','2','3','4','5','6','7')))
mar_ef_68 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_likert, ~ likert, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=likert,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "Voluntary activation\n(%)", x = "Likert scale (1-7)") +
  scale_y_continuous(limits = c(63.5,100), breaks = seq(60,100, 10)) +
  theme_bw(base_size = 10) -> plot_3
plot_3

# Fitted values: Fatigue scale
(refgrid <- list(fatigue=c('0','1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_fatigue, ~ fatigue, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=fatigue,y=(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL), ymin=(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=(UCL), ymin=(LCL)), size = 1, width = 0) +
  labs(y = "Voluntary activation\n(%)", x = "Fatigue scale (0-10)") +
  scale_y_continuous(limits = c(63.5,100), breaks = seq(60,100, 10)) +
  theme_bw(base_size = 10) -> plot_4
plot_4




# Outcome: Resting twitch torque
fit_omni = lmer(log(resting_twitch) ~ omni + (1|participant), data = d)

# Fitted values: OMNI scale
(refgrid <- list(omni=c('0','1','2','3','4','5','6','7','8','9')))
mar_ef_68 = emmip(fit_omni, ~ omni, at = refgrid, CIs = T, plotit = F, level = 0.68)
mar_ef_95 = emmip(fit_omni, ~ omni, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_68, aes(x=omni,y=exp(yvar))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 0.4, width = 0) +
  geom_errorbar(data = mar_ef_68, aes(ymax=exp(UCL), ymin=exp(LCL)), size = 1, width = 0) +
  labs(y = "Resting twitch torque\n(N·m)", x = "OMNI (0-10)") +
  theme_bw(base_size = 10) -> plot_5
plot_5



# Panel plot: torque-derived variables
plot_grid(plot_1,
          plot_2,
          plot_3,
          plot_4,
          plot_5,
          ncol = 2, 
          nrow = 3,
          labels = c('Ai','Aii','Bi','Bii','C'),
          align = 'v',
          axis = "lr",
          label_size = 12,
          scale = 0.95)
ggsave(file = "figure-5b.png", units="in", width = 6, height = 7, dpi = 300)



#### END
