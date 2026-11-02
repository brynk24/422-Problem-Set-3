library(fpp3)
library(tidyverse)
library(tidyr)
library(forecast)

library(readr)

retail_data <- read_csv("data/retail_sales_raw.csv")

head(retail_data)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")


head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)

head(canadian_gas)

key_vars(canadian_gas)

index_var(canadian_gas)
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)


can_gas_recent <- can_gas_recent |>
  mutate(
    year = year(Month),
    month = month(Month, label = TRUE),
    quarter = quarter(Month)
  )

head(can_gas_recent)


monthly_avg <- can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

monthly_avg

monthly_avg |>
  arrange(desc(avg_volume))

can_gas_recent |>
  mutate(
    next_month = Month + 1,
    same_month_last_year = Month - 12
  ) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |>
  autoplot(Volume)

retail_ts |>
  autoplot(sales)


retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally Adjusted vs. Not Seasonally Adjusted",
    x = NULL,
    y = "Millions of dollars",
    color = NULL
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    x = "Year",
    y = "Billions of Cubic Meters"
  ) +
  theme_minimal()


can_gas |>
  autoplot(Volume) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Canadian Natural Gas Production",
    x = "Year",
    y = "Billions of Cubic Meters"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Natural Gas Production",
    x = "Year",
    y = "Log Volume"
  ) +
  theme_minimal()


retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail Sales (NSA)",
    x = "Year",
    y = "Millions of Dollars"
  ) +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Log of U.S. Retail Sales (NSA)",
    x = "Year",
    y = "Log Sales"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

head(can_gas)

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Monthly Growth Rate of Canadian Natural Gas Production",
    x = "Year",
    y = "Monthly Growth Rate (%)"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

head(can_gas)

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Difference Between Growth Rate Methods",
    x = "Year",
    y = "Absolute Difference (Percentage Points)"
  ) +
  theme_minimal()

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

head(retail_ts)

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL,
    y = "Growth Rate (%)",
    color = NULL
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y = "Log Volume"
  ) +
  theme_minimal()

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau",
    x = NULL,
    y = "Difference (%)"
  ) +
  theme_minimal()
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(
    title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
    y = "log(Volume)",
    color = NULL
  ) +
  theme_minimal()

can_gas_fit |>
  gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

gas_roll_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_roll_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))