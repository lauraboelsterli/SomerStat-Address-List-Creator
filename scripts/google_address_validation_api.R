# talk about what params i should keep from the API response 
# https://developers.google.com/maps/documentation/address-validation/reference/rest/v1/TopLevel/validateAddress#request-body

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, fs, readxl,httr,jsonlite)

source("scripts/download_addresses_from_mass_gis.R")
source("scripts/utils/key_creation.R")

# get api key from .Renv file
readRenviron(".Renviron")
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

url <- paste0("https://addressvalidation.googleapis.com/v1:validateAddress?key=", api_key)


all_somerville_addresses_validation_result <- read_csv("data/somerville_all_addresses.csv")

raw_address_list <- download_raw_adddress_list()

# Load address list
# using file for address_list to start api where was last left off w validation
if (file.exists("data/somerville_api_validated_addresses.csv")) {
  # address_list <- read_csv("data/somerville_api_validated_addresses.csv")
  address_list <- read_csv("data/somerville_api_validated_addresses.csv", show_col_types = FALSE)
  # problems(address_list)
} else {
  address_list <- raw_address_list %>%
    mutate(primary_line = paste(ADDR_NUM, STREETNAME)) %>%
    rowwise() %>%
    mutate(address = make_key(primary_line, UNIT, TOWN, "ma", ZIPCODE)) %>%
    ungroup()
  # Add new empty columns for validation
  if (!"formatted_address" %in% colnames(address_list)) {
    address_list$formatted_address <- NA
    address_list$validation_timestamp <- NA
    address_list$validation_granularity <- NA
    address_list$geocode_granularity <- NA
    address_list$usps_dpvo_return_code <- NA
    address_list$usps_address_line1 <- NA
    address_list$address_complete_check <- NA
    address_list$postal_code <- NA
    address_list$locality <- NA
    address_list$region_code <- NA
    address_list$latitude <- NA
    address_list$longitude <- NA
    address_list$place_id <- NA
    address_list$location_type <- NA
    address_list$has_unconfirmed <- NA
    address_list$has_inferred <- NA
    address_list$has_replaced <- NA
    address_list$dpv_footnote <- NA
    address_list$dpv_footnotes_desc <- NA
    address_list$dpv_vacant <- NA
    address_list$dpv_cmra <- NA
    address_list$dpv_no_stat <- NA
  }}

# api call function for address validation
# assuming all addresses are in the US (hard coded)
address_validation <- function(addressLines, regionCode = "US", enableUspsCass = TRUE) {
  address_body <- list(address = list(addressLines = addressLines,
                                      regionCode = regionCode),enableUspsCass=enableUspsCass)

  # API request 
  response <- POST(
    url = url,
    body = toJSON(address_body, auto_unbox = TRUE),
    encode = "json",
    content_type_json()
  )
  
  # break out if rate limit and run again next day (starts with the 
  # address row without a validation time stamp from there)
  if (status_code(response) == 429) {
    message("Daily quota exceeded. Stopping script.")
    stop("Quota hit")
  }

  return(content(response, "parsed", simplifyVector = TRUE))
}


for (i in seq_len(nrow(address_list))) {
  if (!is.na(address_list$formatted_address[i])) next # skip if alr validated (has to include formatted_address to prove an api repsonse came thru)
  # (if api address response is there its been validated at least once)
  # can check when it was validated and revalidate them by changing the logic here
  # e.g., instead of checking if formatted address columns are na and have an or clause where one filters based on
  # timestamp values (e.g., old addresses that should be revalidated from a certain time ago)
  address <- address_list$address[i]
  cat("address from df:", address)
  result <- address_validation(addressLines = address)

  # Extract useful api response fields
  # address
  formatted <- result$result$address$formattedAddress %||% NA
  postal_code <- result$result$address$postalAddress$postalCode %||% NA
  locality <- result$result$address$postalAddress$locality %||% NA
  region_code <- result$result$address$postalAddress$regionCode %||% NA
  # location/latlong
  latitude <- result$result$geocode$location$latitude %||% NA
  longitude <- result$result$geocode$location$longitude %||% NA
  place_id <- result$result$geocode$placeId %||% NA
  location_type <- result$result$geocode$locationType %||% NA
  # verdict
  validation_granularity <- result$result$verdict$validationGranularity %||% NA
  geocode_granularity <- result$result$verdict$geocodeGranularity %||% NA
  address_complete <- result$result$verdict$addressComplete %||% NA
  has_unconfirmed <- result$result$verdict$hasUnconfirmedComponents %||% NA
  has_inferred <- result$result$verdict$hasInferredComponents %||% NA
  has_replaced <- result$result$verdict$hasReplacedComponents %||% NA
  #usps data
  usps <- result$result$uspsData %||% list()
  dpv_footnote <- usps$dpvFootnote %||% NA
  dpv_footnotes_desc <- usps$dpvFootnotesDesc %||% NA
  dpv_vacant <- usps$dpvVacant %||% NA
  dpv_cmra <- usps$dpvCmra %||% NA
  dpv_no_stat <- usps$dpvNoStat %||% NA
  usps_address_line1 <- usps$standardizedAddress$firstAddressLine %||% NA
  # dpv_footnote
  # add timestamp
  timestamp <- Sys.time()

  # now updating main address_list
  row_index <- match(address, address_list$address)
  
  cat("Updating row:", row_index, "\n")
  cat("usps_address_line1:", usps_address_line1, "\n")


  address_list$formatted_address[row_index] <- formatted
  address_list$postal_code[row_index] <- postal_code
  address_list$locality[row_index] <- locality
  address_list$region_code[row_index] <- region_code
  
  address_list$latitude[row_index] <- latitude
  address_list$longitude[row_index] <- longitude
  address_list$place_id[row_index] <- place_id
  address_list$location_type[row_index] <- location_type
  
  address_list$validation_timestamp[row_index] <- timestamp
  address_list$validation_granularity[row_index] <- validation_granularity
  address_list$geocode_granularity[row_index] <- geocode_granularity
  address_list$address_complete_check[row_index] <- address_complete
  address_list$has_unconfirmed[row_index] <- has_unconfirmed
  address_list$has_inferred[row_index] <- has_inferred
  address_list$has_replaced[row_index] <- has_replaced
  
  address_list$dpv_footnote[row_index] <- dpv_footnote
  address_list$dpv_footnotes_desc[row_index] <- dpv_footnotes_desc
  address_list$dpv_vacant[row_index] <- dpv_vacant
  address_list$dpv_cmra[row_index] <- dpv_cmra
  address_list$dpv_no_stat[row_index] <- dpv_no_stat
  address_list$usps_address_line1[row_index] <- usps_address_line1
  
  # sort na at top and then from oldest to newest (ascending order) 
  address_list <- address_list %>%
    arrange(desc(is.na(validation_timestamp)), validation_timestamp) # ascending is default in arrange
  
  # save to file after each API call 
  write_csv(address_list, "data/somerville_api_validated_addresses.csv")
}
