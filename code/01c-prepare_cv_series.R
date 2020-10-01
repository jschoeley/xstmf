# Prepare 5-fold cross validation time-series splits of weekly
# deaths counts by age and sex
#
# 2020-09-30
#
# Jonas Schöley

# Init ------------------------------------------------------------

source('code/00-init.R')

library(tidyverse)

dat <- list()
fig <- list()

# Constants -------------------------------------------------------

cnst <- list(
  # first week of test period (inclusive)
  iso_week_start_of_test = 10,
  # last week of test period (inclusive)
  iso_week_end_of_test = 39
)

cnst$epi_week_start_of_test <-
  (52 - glob$week_epi_year_starts + 1) +
  cnst$iso_week_start_of_test
cnst$epi_week_end_of_test <-
  (52 - glob$week_epi_year_starts + 1) +
  cnst$iso_week_end_of_test

# Load xstmf ------------------------------------------------------

load('out/2020-09-30-xstmf.RData')

# Prepare cross-validation data sets ------------------------------

# define cross-validation series
dat$cv <-
  # set up K-fold cross-validation
  map2(2007+0:4, 2015+0:4, EpiYearSequence)

dat$country_selection <-
  xstmf %>%
  group_by(country_code) %>%
  summarise(
    min_year = min(year)
  ) %>%
  filter(min_year <= 2007) %>%
  pull(country_code)

# test-training data
dat$xstmf_cv <-
  dat$cv %>%
  map(~filter(filter(xstmf, country_code %in% dat$country_selection),
              epi_year %in% .x)) %>%
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
  arrange(cv_id, country_code, sex, age_group, date) %>%
  # add weeks since start of series
  group_by(cv_id) %>%
  mutate(
    weeks_since_origin =
      WeeksSinceOrigin(date, min(date))
  ) %>%
  ungroup()

xstmf_cv <- dat$xstmf_cv
save(
  xstmf_cv,
  file = paste0('out/', Sys.Date(), '-xstmf_cv.RData')
)

# Plot training-test split ----------------------------------------

# plot training-test split
fig$training_test_bars <-
  dat$xstmf_cv %>%
  filter(age_group == '85+', sex == 'Male') %>%
  ggplot(aes(x = date, y = cv_id)) +
  geom_path(
    aes(color = sample, group = interaction(cv_id, sample)),
    size = 3
  ) +
  facet_wrap(~country_name) +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  scale_y_continuous(breaks = 1:20) +
  labs(x = NULL, y = NULL) +
  guides(color = 'none') +
  scale_color_manual(values = glob$colors$sample) +
  glob$MyGGplotTheme()
