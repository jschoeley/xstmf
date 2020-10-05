# Prepare Sweden & Denmark deaths data
#
# 2020-10-05
#
# Jonas Schöley

# Init ------------------------------------------------------------

library(tidyverse)

source('code/00-init.R')

dat <- list()
fig <- list()

# Constants -------------------------------------------------------

cnst <- list(
  # first week of test period (inclusive)
  iso_week_start_of_test = 10,
  # last week of test period (inclusive)
  iso_week_end_of_test = 26
)

cnst$epi_week_start_of_test <-
  (52 - glob$week_epi_year_starts + 1) +
  cnst$iso_week_start_of_test
cnst$epi_week_end_of_test <-
  (52 - glob$week_epi_year_starts + 1) +
  cnst$iso_week_end_of_test

# Load data -------------------------------------------------------

# MP's death counts by epi-year forecast
load('data/marie_pierre/2020-09-30-dk_mp_epi_year_death_forecast.RData')

# extended HMD STMF data
load('out/2020-10-05-xstmf.RData')

# Filter down to Denmark and Sweden -------------------------------

dat$xstmf_dk_swe_full <-
  xstmf %>%
  filter(
    country_code %in% c('DNK', 'SWE')
  )

# Add MPs annual death count forecast -----------------------------

dat$xstmf_dk_swe_full <-
  dat$xstmf_dk_swe_full %>%
  left_join(
    dk_mp_epi_year_death_forecast %>%
      mutate(age_group = fct_recode(age_group, `85+` = '[85,Inf)')),
    by = c('country_code', 'epi_year', 'age_group', 'sex')
  ) %>%
  ungroup() %>%
  mutate(
    sample = ifelse(
      (year == 2020 & iso_week >= cnst$iso_week_start_of_test),
      'test', 'training'
    )
  )

# Split into cross validation data sets ---------------------------

# define cross-validation series
dat$cv <-
  # set up K-fold cross-validation series
  # start with 2007 as earliest year in both Sweden and Denmark
  map2(2007+0:4, 2015+0:4, EpiYearSequence)

# test-training data
dat$xstmf_dk_swe_cv <-
  dat$cv %>%
  map(~filter(dat$xstmf_dk_swe_full, epi_year %in% .x)) %>%
  map(~mutate(.x, sample = ifelse(
    (year == max(epi_year_int)+1 &
       iso_week >= cnst$iso_week_start_of_test),
    'test', 'training')
  )) %>%
  bind_rows(.id = 'cv_id') %>%
  filter(
    sample == 'training' |
      (sample == 'test' & iso_week <= cnst$iso_week_end_of_test)
  ) %>%
  mutate(cv_id = as.integer(cv_id)) %>%
  arrange(cv_id, sex, age_group, date) %>%
  # add weeks since start of series
  group_by(cv_id) %>%
  mutate(
    weeks_since_origin =
      WeeksSinceOrigin(date, min(date))
  ) %>%
  ungroup()

# Visual checks of xstmf_dk_swe_full ------------------------------

fig$death_counts_dk_swe <-
  dat$xstmf_dk_swe_full %>%
  ggplot(aes(x = date, y = observed_deaths)) +
  geom_point(size = 0.4, aes(color = sex)) +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_grid(age_group~country_code, scales = 'free_y') +
  glob$MyGGplotTheme(grid = 'xy', panel_border = TRUE) +
  labs(
    y = 'Weekly deaths', x = 'Year',
    title = 'Weekly death counts Denmark-Sweden'
  ) +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank())

fig$exposures_dk_swe <-
  dat$xstmf_dk_swe_full %>%
  ggplot(aes(x = date, color = sex)) +
  geom_step(
    aes(y = exposure_pw_hmd),
    size = 0.4, alpha = 0.5
  ) +
  geom_line(size = 0.4, aes(y = exposure_pw_interp)) +
  geom_point(
    aes(x = date, color = sex, y = pop_jan1st),
    size = 0.4,
    data = dat$xstmf_dk_swe_full %>%
      group_by(country_code, sex, age_group, year) %>%
      slice(1)
  ) +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_grid(age_group~country_code, scales = 'free_y') +
  glob$MyGGplotTheme(grid = 'xy', panel_border = TRUE) +
  labs(
    y = 'Weekly person-weeks of exposure', x = 'Year',
    title = 'Exposures Denmark-Sweden',
    subtitle = 'Points: Population January 1st\nSmooth line: Interpolated and weekly integrated January 1st population\nStepped line: HMD reported exposures'
  ) +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank())

fig$epi_years <-
  dat$xstmf_dk_swe_full %>%
  group_by(country_code, age_group, sex, year) %>%
  slice(1) %>%
  ggplot(aes(x = date)) +
  geom_vline(
    aes(xintercept = start_of_epi_year),
    color = 'grey'
  ) +
  geom_point(
    aes(y = deaths_per_epi_year_fcast_mp, color = sex)
  ) +
  scale_x_date(date_breaks = '1 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_grid(age_group~country_code, scales = 'free_y') +
  glob$MyGGplotTheme(grid = 'xy', panel_border = TRUE) +
  labs(
    y = 'Deaths per epi-year', x = 'Year',
    title = 'MPs forecasted death counts by epi-year'
  ) +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank())

# Visual checks data dkswe_weekly_deaths_cv -----------------------

fig$cross_validation_dk_swe <-
  dat$xstmf_dk_swe_cv %>%
  group_by(date, cv_id, sample, country_code) %>%
  summarise(
    observed_deaths = sum(observed_deaths)
  ) %>%
  group_by(country_code) %>%
  mutate(max_observed_deaths = max(observed_deaths)) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = cv_id + 2/max_observed_deaths*observed_deaths-1.5,
                 color = sample), size = 0.5) +
  glob$ggtheme +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  scale_y_continuous(breaks = 0:20, labels = 0:20) +
  labs(x = 'Year', y = 'Cross Validation Series') +
  guides(color = 'none') +
  facet_wrap(~country_code) +
  scale_color_manual(values = glob$colors$sample) +
  glob$MyGGplotTheme(grid = 'xy')


# Export ----------------------------------------------------------

xstmf_dk_swe_full <- dat$xstmf_dk_swe_full
save(
  xstmf_dk_swe_full,
  file = paste0('out/swe_dk_export/', Sys.Date(), '-xstmf_dk_swe_full.RData')
)
xstmf_dk_swe_cv <- dat$xstmf_dk_swe_cv
save(
  xstmf_dk_swe_cv,
  file = paste0('out/swe_dk_export/', Sys.Date(), '-xstmf_dk_swe_cv.RData')
)

ExportFiguresFromList(
  fig, path = 'out/swe_dk_export/', add_date = TRUE,
  width = 250, height = 200
)
