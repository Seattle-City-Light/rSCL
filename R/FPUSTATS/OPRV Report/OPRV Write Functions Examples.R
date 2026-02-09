
library(rscl)
library(tidyverse)
source('I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/OPRV/OPRV Write Functions.R')





# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to write an OPRV report to the FPUSTATS database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# specifying upload parameters
base_path <- 'I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/OPRV/OPRV Files/Formatted/'
file_name <- '2025-12 OPRV.xlsx'
file_path <- paste0(base_path,file_name)

# These are the scl_connect() manager user and database for the FPUSTATS database
user <- 'MATTHEW'
db <- 'EPMMART_RW'

# writing the report to the database
fpustats_write_oprv_report(report_path = file_path,
                           scl_connect_user = user,
                           scl_connect_db = db)





# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to delete a OPRV report from the FPUSTATS database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# deleting 1 specific report from the database

# specifying delete parameters
year <- 2024           # year of report
month <- 11             # month of report
eff_dt <- '2026-02-05' # <- check this matches the uploaded eff_dt
connect <- T           # true for having the function handling the connect

# These are the scl_connect() manager user and database for the FPUSTATS database
user <- 'MATTHEW'
db <- 'EPMMART_RW'

fpustats_delete_oprv_report(year = year, 
                            month = month,
                            eff_dt = eff_dt,
                            scl_connect_user = user,
                            scl_connect_db = db)






# deleting multiple reports from the database

# specifying delete parameters
years <- seq(2021,2024) # years of reports
months <- seq(1,12)     # months of reports
eff_dt <- '2026-02-05' # <- check this matches the uploaded eff_dt
connect <- F           # False for looping since we don't want to connect everytime we delete a report

# These are the scl_connect() manager user and database for the FPUSTATS database
user <- 'MATTHEW'
db <- 'EPMMART_RW'

scl_connect(user, db)

for(year in years){
  
  for (month in months) {
    
    fpustats_delete_oprv_report(year = year, 
                                month = month,
                                eff_dt = eff_dt,
                                connect = F)
    
  }
}

RJDBC::dbDisconnect(con)



