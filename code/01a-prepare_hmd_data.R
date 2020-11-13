# Download and merge HMD weekly deaths and yearly population counts
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
  codebook = list(
    sex = c(`f` = "Female", `m` = "Male", `b` = "Total"),
    sex2 = c(`Female` = "Female", `Male` = "Male", `Total` = "Total"),
    age_group =
      c(
        `0_14` = "[0,15)",
        `15_64` = "[15,65)",
        `65_74` = "[65,75)",
        `75_84` = "[75,85)",
        `85p` = "85+"
      ),
    age_group2 =
      c(
        `[0,15)` = "[0,15)",
        `[15,65)` = "[15,65)",
        `[65,75)` = "[65,75)",
        `[75,85)` = "[75,85)",
        `85+` = "85+"
      )
  ),
  age_group_breaks = c(0, 15, 65, 75, 85, Inf),
  age_group_labels = c('[0,15)', '[15,65)', '[65,75)', '[75,85)', '85+')
)

# Load data -------------------------------------------------------

# HMD short term mortality fluctuations data base
# https://www.mortality.org/Public/STMF/Outputs/stmf.csv
dat$stmf <-
  readr::read_csv(
    "data/hmd_stmf/stmf.csv",
    col_types = "ciicddddddddddddlll",
    skip = 2, col_names = TRUE
  )

cnst$countries_in_stmf <- unique(dat$stmf$CountryCode)

# HMD population january 1st estimates
# https://www.mortality.org/hmd/zip/by_statistic/population.zip
file_names <- list.files('data/hmd_population_jan1st/Population/')
dat$hmd_pop <-
  map(file_names, ~{
  cntry <- gsub('\\..+$', '', .x)
  paste0("data/hmd_population_jan1st/Population/", .x) %>%
    read_delim(
      col_types = "ccddd", delim = ' ', trim_ws = TRUE,
      skip = 2, col_names = TRUE, na = '.'
    ) %>%
    mutate(
      country_code = cntry
    )
  }) %>%
  bind_rows()

# Harmonize format of population data -----------------------------

dat$population_harmonized <-
  dat$hmd_pop %>%
  rename(year = Year, age = Age) %>%
  pivot_longer(
    cols = c(Female, Male, Total),
    names_to = 'sex',
    values_to = 'pop_jan1st'
  ) %>%
  # aggregate to same age groups as STMF data
  mutate(
    age =
      ifelse(age == '110+', 110, age) %>% as.integer(),
    age_group =
      cut(
        age,
        breaks = cnst$age_group_breaks,
        labels = cnst$age_group_labels,
        right = FALSE, include.lowest = TRUE
      )
  ) %>%
  group_by(year, country_code, sex, age_group) %>%
  summarise(
    pop_jan1st = sum(pop_jan1st)
  ) %>%
  mutate(
    sex =
      factor(sex, levels = names(cnst$codebook$sex2),
             labels = cnst$codebook$sex2)
  ) %>%
  ungroup() %>%
  # filter down to countries also available in STMF
  filter(country_code %in% cnst$countries_in_stmf) %>%
  # breaks in territorial coverage are marked by an additional
  # `+` (post change) or `-` (prior change) suffix to the year
  # this will be separated into an extra variable and only the
  # post-change year will be kept
  filter(!grepl('\\-', year)) %>%
  mutate(
    pop_jan1st_territory_change =
      ifelse(substr(year, 5, 5) == '+', TRUE, FALSE),
    year = substr(year, 1, 4) %>% as.integer()
  )

# Harmonize format of death data ----------------------------------

dat$deaths_harmonized <-
  dat$stmf %>%
  select(
    country_code = CountryCode,
    year = Year, iso_week = Week,
    sex = Sex,
    D0_14:D85p, R0_14:R85p,
  ) %>%
  pivot_longer(
    cols = D0_14:R85p,
    names_sep = 1,
    names_to = c('statistic', 'age_group'),
    values_to = 'value'
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  rename(observed_deaths = D, mortality_hmd = R) %>%
  mutate(
    sex =
      factor(sex, levels = names(cnst$codebook$sex), labels = cnst$codebook$sex),
    age_group = 
      factor(
        age_group,
        levels = names(cnst$codebook$age_group),
        labels = cnst$codebook$age_group
      ),
    country_name =
      factor(
        country_code,
        glob$countries$country_code,
        glob$countries$country_name
      )
  )

# Merge population and deaths data --------------------------------

dat$pop_deaths_merged <-
  dat$deaths_harmonized %>%
  left_join(
    dat$population_harmonized,
    by = c('country_code', 'year', 'sex', 'age_group')
  ) %>%
  # use same weekly exposure as used by HMD
  group_by(country_code, sex, age_group) %>%
  arrange(year, iso_week) %>%
  mutate(
    exposure_pw_hmd =
      observed_deaths / mortality_hmd * 52,
    # if mortality is 0 then exposures are NaN,
    # fill with previous value
    exposure_pw_hmd =
      ifelse(mortality_hmd == 0, NA, exposure_pw_hmd)
  ) %>%
  fill(exposure_pw_hmd, .direction = 'down') %>%
  ungroup() %>%
  arrange(country_code, sex, age_group, year, iso_week) %>%
  select(
    country_code, country_name, sex, age_group, year, iso_week,
    observed_deaths, pop_jan1st, everything()
  ) %>%
  filter(sex != 'Total') %>%
  mutate(sex = fct_drop(sex))

# Export ----------------------------------------------------------

hmd_yearly_pop_weekly_deaths <- dat$pop_deaths_merged
save(
  hmd_yearly_pop_weekly_deaths,
  file = paste0('out/', Sys.Date(), '-hmd_yearly_pop_weekly_deaths.RData')
)
