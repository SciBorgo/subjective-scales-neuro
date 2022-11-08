

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: June 2021

# Load data
d = read_csv("effort-neuro-data-22-07-21.csv") %>%
  clean_names()

# MVC
fit = lmer(log(mvc) ~ bs(test_block, knots = 5) + (1|participant), data = d)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "none"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
    locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))
  
d$var = log(d$mvc)
dc = d %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = d %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(0,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(0,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(0,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)



# VA
fit = lmer(va ~ bs(test_block, knots = 5) + (1|participant), data = d)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "bonferroni"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

d$var = d$va
dc = d %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = d %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(0,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(0,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(0,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)



# Resting twitch
fit = lmer(log(resting_twitch) ~ bs(test_block, knots = 5) + (1|participant), data = d)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "bonferroni"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

d$var = log(d$resting_twitch)
dc = d %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = d %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(0,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(0,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(0,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)




# Biceps MEP/ Mmax Area
dsub = d %>% filter(bicep_mep_to_mmax_area < 2)
fit = lmer(bicep_mep_to_mmax_area ~ bs(test_block, knots = 5) + (1|participant), data = dsub)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)))}, ~ test_block, adjust = "bonferroni"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

dsub$var = dsub$bicep_mep_to_mmax_area
dc = dsub %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = dsub %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = dsub %>% filter(test_block %in% c(0,3)) %>% filter(!participant == "th02"); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = dsub %>% filter(test_block %in% c(0,4)) %>% filter(!participant == "nm19"); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = dsub %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = dsub %>% filter(test_block %in% c(0,6)) %>% filter(!participant == "hh17"); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = dsub %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = dsub %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = dsub %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = dsub %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)



# Biceps Silent Period Duration
fit = lmer(bicep_silent_period_duration ~ test_block + (1|participant), data = d)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "bonferroni"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

d$var = d$bicep_silent_period_duration
dc = d %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = d %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(0,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(0,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(0,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)


# Biceps rms EMG
fit = lmer(log(bicep_rms_emg) ~ poly(test_block, degree = 2) + (1|participant), data = d)
summary(fit)
post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "bonferroni"))
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

d$var = log(d$bicep_rms_emg)
dc = d %>% filter(test_block %in% c(0,1)); d1 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t1 = d1[3]
dc = d %>% filter(test_block %in% c(0,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(0,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(0,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(0,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(0,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(0,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(0,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(0,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(0,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)



#### END











