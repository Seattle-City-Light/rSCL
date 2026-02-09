
library(rscl)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# How to read reports from the OPRV report database
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Reading 1 specific report

year <- 2024         # year of report
month <- 1           # month of report
most_eff_dt <- T     # most recently uploaded
read_all <- F        # false because looking at 1 report (is false by default)

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

oprv_report <- fpustats_read_oprv(year,
                                  month,
                                  most_eff_dt,
                                  read_all,
                                  user,
                                  database)




# reading all reports for a given month

year <- 2024         # year of report
month <- 1           # month of report
most_eff_dt <- F     # all uploaded reports for the month
read_all <- F        # false because looking at 1 report (is false by default)

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

oprv_report <- fpustats_read_oprv(year,
                                  month,
                                  most_eff_dt,
                                  read_all,
                                  user,
                                  database)




# reading all most recently uploaded reports (returns one unique report for each month)

most_eff_dt <- T     # Most recently uploaded report for the month
read_all <- T        # True for pulling every report

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

oprv_report <- fpustats_read_oprv(most_eff_dt = most_eff_dt,
                                  read_all = read_all,
                                  scl_connect_user = user,
                                  scl_connect_db = database)




# reading all oprv reports (Can include multiple reports for the same month if there were updates or multiple uploads)

most_eff_dt <- F     # all uploaded reports for all the months
read_all <- T        # True for pulling every report

user <- 'MATTHEW'          # scl_connect user
database <- 'EPMMART_RW'   # scl_connect fpustats database

oprv_report <- fpustats_read_oprv(most_eff_dt = most_eff_dt,
                                  read_all = read_all,
                                  scl_connect_user = user,
                                  scl_connect_db = database)
