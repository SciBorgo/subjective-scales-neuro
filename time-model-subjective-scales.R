

# Study: Perception of effort and neuromuscular function
# Author: DN Borg
# Date: June 2021

# Load data
d = read_csv("effort-neuro-data-22-07-21.csv") %>%
  clean_names() %>%
  mutate(
    likert = as.factor(likert),
    omni = as.factor(omni),
    omni = recode_factor(omni,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    rpe = as.factor(rpe),
    rpe = recode_factor(rpe,'1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9'),
    fatigue = as.factor(fatigue),
    fatigue = recode_factor(fatigue,'0' = '0','1' = '1','2' = '2','3' = '3','4' = '4','5' = '5','6' = '6','7' = '7','8' = '8','9' = '9','10' = '9')
  ) %>%
  filter(!test_block == "0")


# OMNI scale
# Ordinal model with random intercept for participant
fit = clmm(omni ~ test_block + (1|participant), data = d) 

# Summary
summary(fit)
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = F)

# Odds ratio
oddsrat = tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = T)
round(oddsrat[10,2:7], digits = 2)




# RPE scale
# Ordinal model with random intercept for participant
fit = clmm(rpe ~ test_block + (1|participant), data = d)

# Summary
summary(fit)
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = F)

# Odds ratio
oddsrat = tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = T)
round(oddsrat[9,2:7], digits = 2)





# Fatigue scale
# Ordinal model with random intercept for participant
table(d$fatigue)
fit = clmm(fatigue ~ test_block + (1|participant), data = d)

# Summary
summary(fit)
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = F)

# Odds ratio
oddsrat = tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = T)
round(oddsrat[10,2:7], digits = 2)



# Likert
# Ordinal model with random intercept for participant
table(d$likert)
#fit = clmm(likert ~ test_block + (1|participant), data = d)
fit <- MASS::polr(likert ~ test_block, data = d, Hess = T)

# Summary
summary(fit)
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = F)

# P-values
ctable <- coef(summary(fit)) # Make a table of the model 'summary'
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # Calculate p values
ctable <- cbind(ctable, "p value" = p)
print(ctable,2) # your model output with p values added

# Odds ratio
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = T)





# VAS
# Model
fit = lmer(vas ~ poly(test_block, degree = 2) + (1|participant), data = d)

# Diagnostics
plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
hist(residuals(fit), breaks = 40)

# Summary
summary(fit)
confint(fit)

post_hoc = pairs(emmeans({ref_grid(fit, at = list(test_block = seq(0,10, by = 1)), transform = T)}, ~ test_block, adjust = "bonferroni"))
post_hoc
post_hoc[1:10,] %>% 
  as_tibble() %>%
  mutate(adj_p = p.value*10,
         adj_p = round(adj_p,3)) %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "red")),
            locations = cells_body(columns = adj_p, rows = adj_p >= 0.05))

d$var = d$vas
dc = d %>% filter(test_block %in% c(1,2)); d2 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t2 = d2[3]
dc = d %>% filter(test_block %in% c(1,3)); d3 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t3 = d3[3]
dc = d %>% filter(test_block %in% c(1,4)); d4 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t4 = d4[3]
dc = d %>% filter(test_block %in% c(1,5)); d5 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t5 = d5[3]
dc = d %>% filter(test_block %in% c(1,6)); d6 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t6 = d6[3]
dc = d %>% filter(test_block %in% c(1,7)); d7 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t7 = d7[3]
dc = d %>% filter(test_block %in% c(1,8)); d8 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t8 = d8[3]
dc = d %>% filter(test_block %in% c(1,9)); d9 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t9 = d9[3]
dc = d %>% filter(test_block %in% c(1,10)); d10 = cohen.d(d = dc$var, f = as.factor(dc$test_block), paired = T, noncentral = T, conf.level = 0.95, na.rm = T); t10 = d10[3]

eff_s = rbind(t2,t3,t4,t5,t6,t7,t8,t9,t10) %>%
  as.data.frame() %>%
  rownames_to_column(var = "block") %>%
  mutate(estimate = round({as.numeric(estimate)}, digits = 2))
eff_s %>% arrange(estimate)
min(eff_s$estimate)
max(eff_s$estimate)



### END

# # Predicted probs
# newdat = expand.grid(test_block = seq(0,10, by = 1))
# pred_prob = pred_clmm(eta=rowSums(newdat), theta=fit$Theta)
# 
# dat = pred_prob %>% 
#   as.data.frame() %>% 
#   rownames_to_column("test_block") %>%
#   pivot_longer(cols = V1:V7, names_to = "rating", values_to = "prob")
