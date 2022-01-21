library(tidyverse)
library(lme4)

d <- readr::read_csv(here::here("data", "analytic-sample.csv"))

#### Plots
d |>
  group_by(grade) |>
  summarize(score = mean(rit_tot, na.rm = TRUE)) |>
  ggplot(aes(grade, score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "pink", se = FALSE) +
  geom_smooth(method = "lm", color = "orange", formula = y ~ poly(x, 2), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)

# cubic trend looks promising


###### Descriptives
d |>
  group_by(grade) |>
  summarize(
    mean_rit = mean(rit_tot, na.rm = TRUE),
    sd_rit = sd(rit_tot, na.rm = TRUE)
  )

d |>
  summarize(
    mean = mean(scaled_absenteeism, na.rm = TRUE),
    sd = sd(scaled_absenteeism, na.rm = TRUE)
  )

# unconditional growth model
m0 <- lmer(
  rit_tot ~ wave +
    (wave | ssid) + (wave | schid),
  data = d,
  control = lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

m01 <- lmer(
  rit_tot ~ poly(wave, 3) +
    (wave | ssid) + (wave | schid),
  data = d,
  control = lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

# polynomial model clearly fits better, unsurprisingly
anova(m0, m01)
# Data: d
# Models:
# m0: rit_tot ~ wave + (wave | ssid) + (wave | schid)
# m01: rit_tot ~ poly(wave, 3) + (wave | ssid) + (wave | schid)
#     npar     AIC     BIC  logLik deviance Chisq Df Pr(>Chisq)    
# m0     9 1388653 1388741 -694317  1388635                        
# m01   11 1386813 1386920 -693395  1386791  1844  2  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# add economically disadvantaged
m1 <- lmer(
  rit_tot ~ poly(wave, 3) + econ_dis_stable +
    (wave | ssid) + (wave | schid),
  data = d,
  control = lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

anova(m01, m1) # clearly better
# Data: d
# Models:
# m01: rit_tot ~ poly(wave, 3) + (wave | ssid) + (wave | schid)
# m1: rit_tot ~ poly(wave, 3) + econ_dis_stable + (wave | ssid) + (wave | schid)
#     npar     AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# m01   11 1386813 1386920 -693395  1386791                         
# m1    12 1382447 1382564 -691212  1382423 4367.7  1  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# try modeling between-school variance
m2 <- lmer(
  rit_tot ~ poly(wave, 3) + econ_dis_stable +
    (wave | ssid) + (wave + econ_dis_stable | schid),
  data = d,
  control = lmerControl(optimizer = "bobyqa"),
  REML = FALSE
)

anova(m1, m2) # clearly better
# Data: d
# Models:
# m1: rit_tot ~ poly(wave, 3) + econ_dis_stable + (wave | ssid) + (wave | schid)
# m2: rit_tot ~ poly(wave, 3) + econ_dis_stable + (wave | ssid) + (wave + econ_dis_stable | schid)
#    npar     AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# m1   12 1382447 1382564 -691212  1382423                         
# m2   15 1381780 1381926 -690875  1381750 673.17  3  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Try modeling interaction
m3 <- lmer(
  rit_tot ~ poly(wave, 3) + econ_dis_stable + econ_dis_stable:wave +
    (wave | ssid) + (wave + econ_dis_stable | schid),
  data = d,
  control = lmerControl(optimizer = "nlminbwrap"),
  REML = FALSE
)
anova(m2, m3) # clearly better
# Data: d
# Models:
# m2: rit_tot ~ poly(wave, 3) + econ_dis_stable + (wave | ssid) + (wave + econ_dis_stable | schid)
# m3: rit_tot ~ poly(wave, 3) + econ_dis_stable + econ_dis_stable:wave + (wave | ssid) + (wave + econ_dis_stable | schid)
#    npar     AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# m2   15 1381780 1381926 -690875  1381750                         
# m3   16 1381402 1381558 -690685  1381370 379.76  1  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

m4 <- lmer(
  rit_tot ~ poly(wave, 3) * econ_dis_stable + sy_days_z +
    (wave | ssid) + (wave + econ_dis_stable | schid),
  data = d,
  control = lmerControl(optimizer = "nlminbwrap"),
  REML = FALSE
)
anova(m3, m4) # fit is the same, keep it as a control

m5 <- lmer(
  rit_tot ~ poly(wave, 3) * econ_dis_stable +
     sy_days_z + scaled_absenteeism +
    (wave | ssid) + (wave + econ_dis_stable | schid),
  data = d,
  control = lmerControl(optimizer = "nlminbwrap"),
  REML = FALSE
)

# got a warning about scaling - just go with z-scored

m5b <- lmer(
  rit_tot ~ poly(wave, 3) * econ_dis_stable +
    sy_days_z + days_absent_z +
    (wave | ssid) + (wave + econ_dis_stable | schid),
  data = d,
  control = lmerControl(optimizer = "nlminbwrap"),
  REML = FALSE
)

# unstandardize the coef
fixef(m5b)["days_absent_z"] / sd(d$days_absent)

# difference in missing 5 days (one week)
(fixef(m5b)["days_absent_z"] / sd(d$days_absent)) * 5

anova(m4, m5b) # much better
# Data: d
# Models:
# m4: rit_tot ~ poly(wave, 3) + econ_dis_stable + econ_dis_stable:wave + sy_days_z + (wave | ssid) + (wave + econ_dis_stable | schid)
# m5b: rit_tot ~ poly(wave, 3) + econ_dis_stable + econ_dis_stable:wave + sy_days_z + days_absent_z + (wave | ssid) + (wave + econ_dis_stable | schid)
#     npar     AIC     BIC  logLik deviance Chisq Df Pr(>Chisq)    
# m4    17 1381403 1381569 -690685  1381369                        
# m5b   18 1380473 1380649 -690219  1380437 932.1  1  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# model as randomly varying bettween schools
m6 <- lmer(
  rit_tot ~ poly(wave, 3) * econ_dis_stable
     sy_days_z + days_absent_z +
    (wave | ssid) + (wave + econ_dis_stable + days_absent_z | schid),
  data = d,
  control = lmerControl(optimizer = "nlminbwrap"),
  REML = FALSE
)

anova(m5, m6) # much better again
