install.packages("BVAR")

library(tidyverse)
library(fpp3)
library(BVAR)

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

autoplot(liquor, sales) +
  labs(title = "Liquor Sales Over Time",
       x = "Year",
       y = "Sales")

gg_season(liquor, log(sales)) +
  labs(title = "Seasonal Pattern of Log Liquor Sales",
       x = "Month",
       y = "Log Sales")

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))|>
  knitr::kable(digits = 2)

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)
forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1,power = 2)
