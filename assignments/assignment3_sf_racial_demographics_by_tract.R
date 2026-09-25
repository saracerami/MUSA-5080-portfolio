library(tidyverse)

sf_tract_race <- get_acs(
  geography = "tract",
  variables = "B02001_008E",
  state = "CA",
  county = "San Francisco",
  year = 2020,
  survey = "acs5"
)
