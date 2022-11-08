

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: May 2021

# Load data
d = read_csv("effort-neuro-data-04-06-21.csv") %>%
  clean_names() %>%
  mutate(
    likert = as.factor(likert),
    omin = as.factor(omni),
    rpe = as.factor(rpe),
    fatigue = as.factor(fatigue)
  )

# Missing data
d %>% vis_miss()
d %>% vis_dat()

# Variable names
sort(names(d))

# Theme
rpe_theme = theme(panel.grid.minor.x = element_blank())

# Likert scale
d %>% ggplot(aes(as.numeric(likert))) + geom_freqpoly() + facet_grid(~test_block)

table(d$likert)

test_block = rep({seq(0,10, by = 1)},7)
likert = seq(1,7, by = 1)
dnew = cbind(test_block, likert)

dsum = d %>%
  group_by(test_block, likert) %>%
  summarise(count = n()) %>%
  mutate(
    prop = count/sum(count),
    freq = prop*100
  ) %>%
  dplyr::select(-count, -freq)

dat = merge(dnew, dsum, by = c("test_block","likert"), all.x = T)
df = dat %>% dplyr::mutate(prop = replace_na(prop, 0))


df %>% filter(test_block>0) %>%
  ggplot(aes(x = likert, y = prop, group = test_block)) +
  geom_line(alpha = 1, size = 0.35) +
  geom_point(size = 1.5) +
  labs(y = "Response\nfrequency", x = "Likert scale")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), labels = scales::percent_format(1L)) +
  theme_bw(base_size = 12) +
  facet_grid(~test_block) +
  rpe_theme -> plot_likert
plot_likert
#ggsave(file = "test7.png", units="in", width = 10, height = 3, dpi = 300)






# RPE scale
d %>% ggplot(aes(as.numeric(rpe))) + geom_freqpoly() + facet_grid(~test_block)

table(d$rpe)
d = d %>% mutate(
  rpe = recode_factor(rpe,
                          '1' = '1',
                          '2' = '2',
                          '3' = '3',
                          '4' = '4',
                          '5' = '5',
                          '6' = '6',
                          '7' = '7',
                          '8' = '8',
                          '9' = '9',
                          '10' = '9'))
table(d$rpe)


test_block = rep({seq(0,10, by = 1)},9)
rpe = seq(1,9, by = 1)
dnew = cbind(test_block, rpe)

dsum = d %>%
  group_by(test_block, rpe) %>%
  summarise(count = n()) %>%
  mutate(
    prop = count/sum(count),
    freq = prop*100
  ) %>%
  dplyr::select(-count, -freq)

dat = merge(dnew, dsum, by = c("test_block","rpe"), all.x = T)
df = dat %>% dplyr::mutate(prop = replace_na(prop, 0))

df %>% filter(test_block>0) %>%
  ggplot(aes(x = rpe, y = prop, group = test_block)) +
  geom_line(alpha = 1, size = 0.35) +
  geom_point(size = 1.5) +
  labs(y = "Response\nfrequency", x = "Rating of percevied exertion")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(1L)) +
  theme_bw(base_size = 12) +
  facet_grid(~test_block) +
  rpe_theme -> plot_rpe



# fatigue scale
d %>% ggplot(aes(as.numeric(fatigue))) + geom_freqpoly() + facet_grid(~test_block)

table(d$fatigue)
d = d %>% mutate(
  fatigue = recode_factor(fatigue,
                          '0' = '0',
                          '1' = '1',
                          '2' = '2',
                          '3' = '3',
                          '4' = '4',
                          '5' = '5',
                          '6' = '6',
                          '7' = '7',
                          '8' = '8',
                          '9' = '9',
                          '10' = '9'))
table(d$fatigue)
test_block = rep({seq(0,10, by = 1)},10)
fatigue = seq(0,9, by = 1)
dnew = cbind(test_block, fatigue)

dsum = d %>%
  group_by(test_block, fatigue) %>%
  summarise(count = n()) %>%
  mutate(
    prop = count/sum(count),
    freq = prop*100
  ) %>%
  dplyr::select(-count, -freq)

dat = merge(dnew, dsum, by = c("test_block","fatigue"), all.x = T)
df = dat %>% dplyr::mutate(prop = replace_na(prop, 0))

df %>% filter(test_block>0) %>%
  ggplot(aes(x = fatigue, y = prop, group = test_block)) +
  geom_line(alpha = 1, size = 0.35) +
  geom_point(size = 1.5) +
  labs(y = "Response\nfrequency", x = "Rating of fatigue")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(1L)) +
  theme_bw(base_size = 12) +
  facet_grid(~test_block) +
  rpe_theme -> plot_fatigue




# OMNI scale
d %>% ggplot(aes(as.numeric(omni))) + geom_freqpoly() + facet_grid(~test_block)

table(d$omni)
d = d %>% mutate(
  omni = recode_factor(omni,
                          '0' = '0',
                          '1' = '1',
                          '2' = '2',
                          '3' = '3',
                          '4' = '4',
                          '5' = '5',
                          '6' = '6',
                          '7' = '7',
                          '8' = '8',
                          '9' = '9',
                          '10' = '9'))
table(d$omni)

test_block = rep({seq(0,10, by = 1)},10)
omni = seq(0,9, by = 1)
dnew = cbind(test_block, omni)

dsum = d %>%
  group_by(test_block, omni) %>%
  summarise(count = n()) %>%
  mutate(
    prop = count/sum(count),
    freq = prop*100
  ) %>%
  dplyr::select(-count, -freq)

dat = merge(dnew, dsum, by = c("test_block","omni"), all.x = T)
df = dat %>% dplyr::mutate(prop = replace_na(prop, 0))

df %>% filter(test_block>0) %>%
  ggplot(aes(x = omni, y = prop, group = test_block)) +
  geom_line(alpha = 1, size = 0.35) +
  geom_point(size = 1.5) +
  labs(y = "Response\nfrequency", x = "OMNI scale")  +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(1L)) +
  theme_bw(base_size = 12) +
  facet_grid(~test_block) +
  rpe_theme -> plot_omni
plot_omni




# VAS
d %>% ggplot(aes(x = test_block, y = vas)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# Model
dsub = d %>% filter(test_block>0)
fit = lmer(vas ~ poly(test_block, degree = 2) + (1|participant), data = dsub)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

# Fitted values
(refgrid <- list(test_block=seq(1,10, by = 1)))
mar_ef_95 = emmip(fit, ~ test_block, at = refgrid, CIs = T, plotit = F, level = 0.95)

ggplot(data=mar_ef_95, aes(x = test_block, y=(yvar*100))) +
  geom_point(size = 2) +
  geom_errorbar(data = mar_ef_95, aes(ymax=(UCL*100), ymin=(LCL*100)), size = 0.4, width = 0) +
  labs(y = "Visual analogue\nscale(0-100)", x = "")  +
  facet_grid(~test_block, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> plot_vas
plot_vas




# Panel plot
plot_grid(plot_rpe, plot_omni, plot_likert, plot_fatigue, plot_vas,
          ncol = 1, 
          nrow = 5,
          labels = c('A','B','C','D','E'),
          label_size = 16,
          align = 'v',
          axis = "lr")
ggsave(file = "figure-3.png", units="in", width = 12, height = 9, dpi = 300)




#### END






