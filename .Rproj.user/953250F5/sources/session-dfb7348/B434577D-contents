library(tidyquant)

library(tidyverse)

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get = "economic.data",
#  from = "1976-01-01"
#)
#
# head(unemp_raw)
#
# dir.create("data", showWarnings = FALSE)
#
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR"    ~ "California",
      symbol == "TXUR"    ~ "Texas",
      symbol == "NYUR"    ~ "New York"
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )
head(unemp)

unemp_top_increases <- unemp |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5
  )

unemp_top_decreases <- unemp |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5
  )

unemp_top_increases
unemp_top_decreases

unemp_top_increases_no_covid <- unemp |>
  filter(year(date) != 2020) |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5
  )
unemp_top_decreases_no_covid <- unemp |>
  filter(year(date) != 2020) |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5
  )

unemp_top_increases_no_covid
unemp_top_decreases_no_covid


