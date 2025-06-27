if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse)
source("Geolocating-scripts/scripts/geolocation_scripts_for_coding.R")

## LOAD IN AND MASSAGE ADDRESSES 

somerville_addresses_fp <- "data/somerville_all_addresses.csv"
somerville_addresses_base <- read_csv(somerville_addresses_fp) %>%
  separate(address,
           into = c("address", "unit", "city", "state", "zip"),
           sep = "\\|") %>%
  rename(address_first_line = address) %>% 
  mutate(address = paste(primary_line, last_line))


# ---------------------------------------------------------------------------- #

## GEOLOCATE
somerville_addresses_geolocated <- geolocationFromTableUsingAddress(somerville_addresses)

somerville_addresses_test <- somerville_addresses_base %>%
  left_join(somerville_addresses, by = "address") %>%
  distinct()

write_csv(somerville_addresses_test, "data/all_somerville_addresses_geolocated.csv")

somerville_addresses_no_stat <- somerville_addresses_test %>%
  filter(dpv_no_stat == "N")

write_csv(somerville_addresses_no_stat, "data/deliverable_addresses_geolocated.csv")

