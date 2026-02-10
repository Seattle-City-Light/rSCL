
library(rscl)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to write an OUL Forecast to the FPUSTATS database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# specifying upload parameters
forecast_path <- "I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/OUL/2026 OUL Forecast.xlsx"

# These are the scl_connect() manager user and database for the FPUSTATS database
user <- 'MATTHEW'
db <- 'EPMMART_RW'

# writing the report to the database
fpustats_write_oul_forecast(forecast_path = forecast_path,
                            user = user,
                            database = db)


check <- fpustats_read_oul_forecast(year=2025,
                                    most_eff_dt = T,
                                    user = user,
                                    database = db)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to delete a OUL forecast from the FPUSTATS database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# deleting 1 vintage from the database

# specifying delete parameters
forecast_vintage <- 2026           # vintage of the forecast
eff_dt <- '2026-02-10' # <- check this matches the uploaded eff_dt
connect <- T           # true for having the function handling the connect

# These are the scl_connect() manager user and database for the FPUSTATS database
user <- 'MATTHEW'
db <- 'EPMMART_RW'

fpustats_delete_oul_forecast(forecast_vintage = forecast_vintage,
                             eff_dt = eff_dt,
                             user = user,
                             database = db)



check <- fpustats_read_oul_forecast(forecast_vintage= forecast_vintage,
                                    most_eff_dt = T,
                                    user = user,
                                    database = db)





