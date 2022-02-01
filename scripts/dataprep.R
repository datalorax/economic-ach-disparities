library(tidyverse)
library(lme4)
theme_set(theme_minimal(15))

files <- fs::dir_ls(here::here("data", "prelim_prep"), type = "file")

assess <- read_csv(files[1])
attend <- read_csv(files[2])

md_assess <- assess |>
  select(
    ssid, schid, distid, acdm_yr, cohort, grade, content,
    econ_dsvntg, ethnicity, rit_tot
  )

md_attend <- attend |>
  select(
    ssid, schid, distid, acdm_yr, grade,
    sy_calendar_days, days_present, days_absent, program_type
  ) |>
  mutate(
    grade = case_when(
      grade == "PK" ~ -1,
      grade == "KG" ~ 0,
      grade == "AE" ~ 13,
      TRUE ~ as.numeric(grade)
    )
  )

d <- left_join(md_assess, md_attend) |>
  mutate(econ_dsvntg = toupper(econ_dsvntg))

coh_a_mth <- d[d$cohort == "a" & d$content == "Mathematics", ]
coh_a_mth$wave <- coh_a_mth$grade - 3

# drop missing econ_dsvntg
coh_a_mth |>
  count(missing = is.na(econ_dsvntg)) |>
  mutate(prop = n/sum(n))

# # A tibble: 2 × 3
#   missing      n        prop
#   <lgl>    <int>       <dbl>
# 1 FALSE   241261 0.9966909
# 2 TRUE       801 0.003309070


# Schools have to have 10 or more students in each category in each wave
keep_schools <- coh_a_mth |>
  mutate(econ_dis = as.factor(ifelse(econ_dsvntg == "Y", 1, 0))) |>
  count(acdm_yr, schid, wave, econ_dis, .drop = FALSE) |>
  group_by(acdm_yr, schid, wave) |>
  mutate(min = min(n, na.rm = TRUE)) |>
  filter(min > 4) |> # This is different from the pre-registration. I said 10, now using 5
  ungroup()

b <- nrow(coh_a_mth) # before exclusion
coh_a_mth <- semi_join(coh_a_mth, keep_schools)
1 - (nrow(coh_a_mth) / b)
# proportion of cases removed: 0.4432955

# remove any school with less than 10% of each FRL category in a given year
low_prop <- coh_a_mth |>
  count(acdm_yr, schid, wave, econ_dsvntg) |>
  group_by(acdm_yr, schid, wave) |>
  mutate(prop = n / sum(n)) |>
  arrange(desc(prop)) |>
  filter(prop >= 0.1 & prop <= 0.9) |>
  ungroup()

coh_a_mth |>
  count(schid, wave, econ_dsvntg) |>
  group_by(wave, schid) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(prop)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = c(0.1, 0.9), color = "red")

# 10% < keep schools > 90%
b <- nrow(coh_a_mth)
coh_a_mth <- semi_join(coh_a_mth, low_prop)
1 - (nrow(coh_a_mth) / b)
# Proportion: 0.04518504


# Compute variables
coh_a_mth <- coh_a_mth |>
  mutate(
    econ_dis = ifelse(econ_dsvntg == "Y", 1, 0),
    scaled_absenteeism = (days_absent - mean(days_absent, na.rm = TRUE)) *
          (5 / sd(days_absent, na.rm = TRUE)),
    sy_days_z = as.numeric(scale(sy_calendar_days)),
    days_absent_z = as.numeric(scale(days_absent))
  )

# Create stable econd dis variable
set.seed(123)
stable_ed <- coh_a_mth |>
  count(ssid, econ_dis) |>
  group_by(ssid) |>
  filter(n == max(n)) |>
  add_count(ssid) |>
  arrange(desc(nn)) |>
  mutate(rand = rnorm(n())) |>
  filter(rand == max(rand)) |>
  select(ssid, econ_dis_stable = econ_dis)

coh_a_mth <- left_join(coh_a_mth, stable_ed)

# drop cases with missing sy_calendar_days
b <- nrow(coh_a_mth)
coh_a_mth <- coh_a_mth |>
  filter(sy_calendar_days != 0)
1 - (nrow(coh_a_mth) / b)
# 0.001398949

ggplot(coh_a_mth, aes(days_absent, rit_tot)) +
  geom_point()

write_csv(coh_a_mth, here::here("data", "analytic-sample.csv"))

