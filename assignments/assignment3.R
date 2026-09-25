library(tidyverse)
library(tidycensus)

sf_tract_income <- get_acs(
  geography = "tract ",
  variables = "B19013_001",
  state = "CA",
  county = "San Francisco",
  year = 2020,
  survey = "acs5"
)
sf_tract_income <- sf_tract_income %>%
  mutate(moe_pct = moe / estimate * 100)

sf_tract_income <- sf_tract_income %>%
  mutate(reliability = case_when(
    moe_pct < 5  ~ "High confidence",
    moe_pct < 10 ~ "Moderate",
    TRUE         ~ "Low confidence"
  ))

ggplot(sf_tract_income)+
  aes(x=estimate, y = moe_pct, color = "blue")+
  geom_point()

ggplot(sf_tract_income)+
  aes(x=estimate, y = moe_pct)+
  geom_point(color = "maroon")

sf_tract_income %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe))

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe))

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, "San Francisco County, California")) %>%
  mutate(NAME = str_remove(NAME, "Census")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, "San Francisco County, California")) %>%
  mutate(NAME = str_remove(NAME, "Census")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip()

sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, "San Francisco County, California")) %>%
  mutate(NAME = str_remove(NAME, "Census")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col(fill = "maroon") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip() +
  theme_minimal()


sf_tract_income %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, "San Francisco County, California")) %>%
  mutate(NAME = str_remove(NAME, "Census")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col(fill = "maroon") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "San Francisco Household Income Estimates by Largest Margin of Error",
    subtitle = "Median Household Income by San Francisco Census Tracts, ACS 2020 5-year estimates",
    x = "Tract",
    y = "Median Household Income"
  )


get_median_hh_income_sf <- function(yr) {
  get_acs(
    geography = "tract",
    variables = "B19013_001",
    county = "San Francisco",
    state     = "CA",
    year      = yr,
    survey    = "acs5"
  ) %>%
    mutate(period = paste0(yr - 4, "-", yr))
}

median_hh_income_sf_change <- bind_rows(
  get_median_hh_income_sf(2015),
  get_median_hh_income_sf(2020)
)

median_hh_income_sf_new <- median_hh_income_sf_change %>%
  select(GEOID, NAME, period, estimate, moe) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(estimate, moe)
  )

hh_income_tested <- median_hh_income_sf_wide %>%
  mutate(
    change   = `estimate_2011-2015` - `estimate_2016-2020`,
    se_1     = `moe_2011-2015` / 1.645,
    se_2     = `moe_2016-2020` / 1.645,
    se_diff  = sqrt(se_1^2 + se_2^2),
    moe_diff = se_diff * 1.645,
    z        = change / se_diff,
    significant = abs(z) > 1.645
  )

hh_income_tested %>%
  count(significant)

x = NULL;
y = ("will fill in later")

library(gt)

reliability_table <- sf_tract_income %>%
  mutate(
    cv = (moe / 1.645) / estimate * 100,
    dot = case_when(
      cv < 12  ~ "🟢",
      cv <= 40 ~ "🟡",
      TRUE     ~ "🔴"
    ),
    flag = case_when(
      cv < 12  ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      TRUE     ~ "Unreliable"
    ),
    NAME = str_remove(NAME, " San Francisco County, California")
  ) %>%
  arrange(desc(cv)) %>%
  select(dot, NAME, estimate, moe, cv, flag)

reliability_table %>%
  gt() %>%
  fmt_number(columns = c(estimate, moe), decimals = 0) %>%
  fmt_number(columns = cv, decimals = 1) %>%
  cols_label(
    dot      = "",
    NAME     = "County",
    estimate = "Estimate",
    moe      = "MOE",
    cv       = "CV (%)",
    flag     = "Reliability"
  ) %>%
  cols_align(align = "center", columns = dot) %>%
  tab_header(
    title    = "Median Household Income by San Francisco Census Tract",
    subtitle = "San Francisco Census Tracts, ACS 2020 5-year estimates"
  ) %>%
  tab_source_note(
    "Reliability thresholds follow Jurjevich et al. (2018): CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable."
  )

#An attempt to map dfs

acs_2015 <- get_acs(
  geography = "tract",
  county = "San Francisco",
  variables = "B19013_001",
  year = 2015,
  state = "CA",
  geometry = TRUE
)

acs_2020 <- get_acs(
  geography = "tract",
  county = "San Francisco",
  variables = "B19013_001",,
  year = 2020,
  state = "CA",
  geometry = FALSE
)

library(dplyr)
library(sf)

hh_income_tested <- hh_income_tested %>%
  left_join(acs_2015 %>% select(GEOID, geometry), by = 'GEOID')

hh_income_tested <- hh_income_tested %>%
  filter(NAME != "Census Tract 9804.01, San Francisco County, California", NAME != "Census Tract 017903, San Francisco County, California")

library(ggplot2)
geom_to_sf <- st_as_sf(hh_income_tested)

ggplot(data = geom_to_sf) +
  geom_sf(aes(fill = significant), color = "black", linewidth = .2) +
  labs(title = "Tract by Statistical Significance", subtitle = "Assessing the Statistical Significance of Median Household \nIncome Changes: ACS 2015 vs. ACS 2020",  fill = "Legend") +
  scale_fill_manual(values = c("FALSE" = "limegreen", "TRUE" = "maroon")) +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      size = 16,
      color = "black",
      hjust = 0,
      vjust = 0,
      margin = margin(b=8)
    ),
    plot.subtitle = element_text(
      family = "sans",
      face = "italic",
      hjust = 0,
      vjust = 0,
      margin = margin(b=-90)
    )
  );

