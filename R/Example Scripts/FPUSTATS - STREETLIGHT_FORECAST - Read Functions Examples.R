
library(rscl)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to read streetlight forecasts from the fpustats database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Reading 1 year forecast

forecast_vintage <- 2026  # year of forecast vintage
year <- 2025          # year of interest
most_eff_dt <- T     # most recently uploaded
read_all <- F        # false because looking at year's forecast (is false by default)

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

sl_forecast <- fpustats_read_streetlight_forecast(forecast_vintage = forecast_vintage,
                                                  year = year,
                                                  most_eff_dt = most_eff_dt,
                                                  read_all = read_all,
                                                  user = user,
                                                  database = database)




# reading full vintage forecast

forecast_vintage <- 2026  # year of forecast vintage
year <- 2025          # year of interest
most_eff_dt <- T     # most recently uploaded
read_all <- T        # True for reading full forecast

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

sl_forecast <- fpustats_read_streetlight_forecast(forecast_vintage = forecast_vintage,
                                                  year = year,
                                                  most_eff_dt = most_eff_dt,
                                                  read_all = read_all,
                                                  user = user,
                                                  database = database)


