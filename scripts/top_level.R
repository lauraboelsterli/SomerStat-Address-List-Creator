if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, fs, readxl)

source("scripts/download_addresses_from_mass_gis.R")
source("scripts/utils/key_creation.R")

all_somerville_addresses_validation_result <- read_csv("data/somerville_all_addresses.csv")

raw_address_list <- download_raw_adddress_list()

address_list <- raw_address_list %>%
  mutate(primary_line = paste(ADDR_NUM, STREETNAME)) %>%
  rowwise() %>%
  mutate(address = make_key(primary_line, UNIT, TOWN, "ma", ZIPCODE)) %>%
  ungroup()
  
misses <- address_list %>% filter(!address %in% all_somerville_addresses_validation_result$address)


removed <- all_somerville_addresses_validation_result %>% filter(!address %in% address_list$address)
## TODO validate with google or another validation api


# add date when i validated as well 

# goal to kinda like recreate the somerville address validation result 

# use the usps address add as another column bc thats probably the address we wanna use 




## end state:
## script that will run each day/week/month/you tell me, stay under the google free quota, that first checks any new addresses and then the oldest remaining addresses.

## I'll handle getting it to IT, deciding what combination of columns make an address "good" and getting it to Socrata (unless something else was decided at the meeting)
