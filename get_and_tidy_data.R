# CREATE TIDY FOI STATS DATA

library(tidyverse)

# https://www.gov.uk/government/statistics/freedom-of-information-statistics-annual-2019
foi_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/881837/foi-statistics-q4-2019-and-annual-published-data2.csv"

foi_raw <- read_csv(foi_url, col_types = cols(.default = col_character()))

# Warning message:
# Duplicated column names deduplicated: 
#   'Total requests received (excluding on-hold and lapsed)' => 
#   'Total requests received (excluding on-hold and lapsed)_1' [14]

# Check duplicate columns are identical

identical(foi_raw$`Total requests received (excluding on-hold and lapsed)`,
          foi_raw$`Total requests received (excluding on-hold and lapsed)_1`)

not_identical <- foi_raw %>% 
  filter(`Total requests received (excluding on-hold and lapsed)` != 
           `Total requests received (excluding on-hold and lapsed)_1`) %>%
  select(Quarter,
         `Government body`,
         `Total requests received (excluding on-hold and lapsed)`,
         `Total requests received (excluding on-hold and lapsed)_1`)

# mismatches are from 2012, 2011 and 2010, exclude any data from these years

foi_tidy <- foi_raw %>%
  filter(str_detect(Quarter,"2010|2011|2012", negate = TRUE)) %>%
  select(-`Total requests received (excluding on-hold and lapsed)_1`) %>%
  rename(time_period = Quarter, government_body = `Government body`) %>%
  mutate(year = str_extract(time_period, "\\d\\d\\d\\d"),
         qtr = str_extract(time_period, "Q\\d"),
         release_type = if_else(is.na(qtr), 
                                "Annual",
                                "Quarterly"),
         time_period = if_else(release_type == "Annual", 
                               paste0("Y  ", year),
                               time_period),
         qtr = if_else(release_type == "Annual", 
                       "Y",
                       qtr)) %>%
  pivot_longer(cols = c(-time_period, 
                           -year, 
                           -qtr, 
                           -release_type, 
                           -government_body),
               names_to = "variable",
               values_to = "value") %>%
  filter(str_detect(variable, "[P|p]ercent", negate = TRUE)) %>%
  mutate(value = as.numeric(value)) %>%
  drop_na(value)

write_lines(unique(foi_tidy$variable), "variables.txt")

readr::write_excel_csv(foi_tidy,"tidy_foi_data_long.csv.bz2")
