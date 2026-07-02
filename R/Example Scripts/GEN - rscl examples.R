

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Install Examples
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

devtools::install_github("Seattle-City-Light/rSCL")

library('rscl')

# Prints current keyring connections - Default are Matthew's connections
# Will need to add your specific connections to the keyring manager
print_keyring_connections()


# Example of deleting and adding a keyring connection to the keyring manager
delete_keyring_connection('MATTHEW', 'MSCS')

add_keyring_connection(user = 'MATTHEW',
                       database = 'MSCS',
                       server = "https://us-ashburn-1.utilities-cloud.oracleindustry.com/c898w8/prod/msc/sql/rest",
                       keyring_service = "MSCS",
                       keyring_name = 'MH',
                       keyring_username = "matthew.hamlin@seattle.gov",
                       keyring_password = "")







#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# examples of how to connect to different databases
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# for generic databases like CCB scl_connect sets con for ccb in the global environment
scl_connect('CCB')

# for the ACCELA database there is no keyring password needed so it is just set as the global environment connection
scl_connect('ACCELA')

# for the MSCS database there is no direction session connection possible so instead the keyring password with other
# credentials are returned for in script rest API queries.
scl_connect('MSCS')





#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# For re-installing rscl, one should make a backup of all their keyring connections in a local folder first for fast updating
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# exporting current manager table and saving to location on local computer
print_keyring_connections() %>% clipr::write_clip()

# re-installing rscl to get updated version
devtools::install_github("Seattle-City-Light/rSCL")

library('rscl')

# uploading the backup keyring manager table
upload_backup_connections(backup_dir = 'I:/FINANCE/FPU/Matthew/Keyring Manager Backup/RSCL Keyring Manager Backup.xlsx')








#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Example of how to pull meter data for a premise
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(dplyr)

# pulling all customer meta info for one specific premise
customer_meta <- scl_pull_customer_meter_meta(ids = '7010433273', id_type = 'PREM_ID')

customer_meta <- customer_meta %>%
  filter(SA_STATUS_FLG=='20') %>% # only active service agreements
  filter(is.na(REMOVAL_DTTM) & is.na(RETIRE_DT)) # only currently active meters at the premise

serivce_point <- scl_pull_badge_to_d1sp_map(ids = customer_meta$BADGE_NBR, id_type = 'BADGE_NBR')


temp <- scl_pull_mscs_hourly_load(start_date = '2025-01-01',end_date =  '2026-01-01')



