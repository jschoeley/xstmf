# Add date and exposure variables
#
# 2020-10-05
#
# Jonas Schöley

# Init ------------------------------------------------------------

source('code/00-init.R')

library(tidyverse)

dat <- list()
fig <- list()

# Load data -------------------------------------------------------

load('out/2020-09-30-hmd_yearly_pop_weekly_deaths.RData')

# Add time related variables --------------------------------------

dat$expanded_hmd_stmf_data <-
  hmd_yearly_pop_weekly_deaths %>%
  # add variables relevant for analysis
  mutate(
    # date at start of week assuming weeks start on Monday
    # (which is not always the case in the HMD reported data)
    date =
      ISOWeekDate2Date(year, iso_week, 1),
        
    ## epi-year
    
    # date that corresponding epi-year started
    start_of_epi_year =
      ISOWeekDate2Date(
        ifelse(iso_week<glob$week_epi_year_starts, year-1, year),
        glob$week_epi_year_starts, 1
      ),
    # current epi-year
    epi_year =
      paste0(lubridate::year(start_of_epi_year),'/',
             lubridate::year(start_of_epi_year)+1),
    # start year of current epi-year
    epi_year_int =
      as.integer(substr(epi_year,1,4)),
    # weeks into epi-year (starting at 0)
    epi_week =
      # difftime(date, start_of_epi_year, units = 'weeks') %>%
      # floor() %>% as.integer(),
      (iso_week-glob$week_epi_year_starts) %>%
      {ifelse(. < 0, 51 + . + 1, .)},

    ## indicators for special time of year
        
    # indicator variable for season
    season = case_when(
      iso_week %in% glob$seasons$northern$Winter ~ 'Winter',
      iso_week %in% glob$seasons$northern$Spring ~ 'Spring',
      iso_week %in% glob$seasons$northern$Summer ~ 'Summer',
      iso_week %in% glob$seasons$northern$Fall ~ 'Fall'
    ),

    # special weeks
    special_week =
      case_when(
        iso_week == 52 ~ 'Last week',
        iso_week == 1 ~ 'First week',
        TRUE ~ 'Regular week'
      ) %>%
      as_factor() %>%
      fct_relevel(
        c('Regular week', 'First week', 'Last week')
      )
  ) %>%
  # weeks since start of data series for a country
  group_by(country_code) %>%
  mutate(
    weeks_since_origin =
      WeeksSinceOrigin(date, min(date), week_format = 'integer')
  ) %>%
  ungroup()

# Add alternative exposure variables ------------------------------

# add interpolated January 1st population counts
# cubic spline interpolation, integrated over single weeks
# to get weekly exposures
dat$expanded_hmd_stmf_data <-
  dat$expanded_hmd_stmf_data %>%
  select(country_code, sex, age_group, weeks_since_origin, year, pop_jan1st) %>%
  group_by(country_code, sex, age_group) %>%
  group_modify(~{
    max_weeks_since_origin <- max(.x$weeks_since_origin)
    group_by(.x, year) %>%
      slice(1) %>%
      ungroup() %>%
      Population2Exposures(
        x = weeks_since_origin,
        P = pop_jan1st,
        breaks_out = 0:max_weeks_since_origin
      ) %>%
      select(weeks_since_origin = x1, exposure_pw_interp = Ex)
  }) %>%
  ungroup() %>%
  right_join(
    dat$expanded_hmd_stmf_data,
    by = c('country_code', 'sex', 'age_group', 'weeks_since_origin')
  )

# Beautification --------------------------------------------------

dat$xstmf <-
  dat$expanded_hmd_stmf_data %>%
  arrange(country_code, sex, age_group, date) %>%
  # add stratum id (strata are sex and age group, not country)
  group_by(sex, age_group) %>%
  mutate(stratum_id = cur_group_id()) %>%
  ungroup() %>%
  # add cell id
  mutate(
    cell_id = 1:n()
  ) %>%
  select(
    # the meat
    country_code, sex, age_group, year, iso_week, observed_deaths,
    # exposures
    pop_jan1st, exposure_pw_interp, exposure_pw_hmd,
    # time
    date, weeks_since_origin,
    epi_year, epi_year_int, start_of_epi_year, epi_week,
    # time flags
    season, special_week,
    # misc
    country_name, mortality_hmd, pop_jan1st_territory_change,
    # id's
    cell_id, stratum_id
  )

# Export ----------------------------------------------------------

xstmf <- dat$xstmf
save(
  xstmf,
  file = paste0('out/', Sys.Date(), '-xstmf.RData')
)
