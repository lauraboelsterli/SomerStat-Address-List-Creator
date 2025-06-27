chooseCRANmirror(ind = 1)

# Download all packages
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
}

# Load required packages
pacman::p_load(
  purrr,
  tidyverse,
  tidygeocoder,
  tibble,
  readxl,
  httr,
  dplyr,
  stringi,
  sp,
  sf,
  magrittr,
  stringr,
  memoise,
  tidyr,
  gt,
  parallel
)

# Define your caching directory
cache_dir <- "data/cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# read in the magical ArcGIS csv's
blockToPrecinct2010 <- read.csv("Geolocating-scripts/data/blockToPrecinct2010.csv")
blockToPrecinct2020 <- read.csv("Geolocating-scripts/data/blockToPrecinct2020.csv")
blocksToOtherGeographies2010 <- read.csv("Geolocating-scripts/data/blockToWard2010.csv")
blocksToOtherGeographies2020 <- read.csv("Geolocating-scripts/data/Somerville Blocks and Wards2020.csv")
blocksToNeighborhoods <- read.csv("Geolocating-scripts/data/Somerville Blocks and Neighborhoods2010.csv")

# Convert GEOID columns in the lookup tables to character
blocksToOtherGeographies2010 <- blocksToOtherGeographies2010 %>%
  mutate(
    GEOID = as.character(GEOID10),
    TRACT2010 = as.character(TRACT2010),
    BLKGRP2010 = as.character(BLKGRP2010),
    BLOCK2010 = as.character(BLOCK2010),
    WARD2010 = as.character(WARD2010)
  ) %>%
  select(GEOID, TRACT2010, BLKGRP2010, BLOCK2010, WARD2010)

blockToPrecinct2010 <- blockToPrecinct2010 %>%
  mutate(
    GEOID = as.character(GEOID10),
    Precinct2010 = as.character(Precinct)
  ) %>%
  select(GEOID, Precinct2010)
blockToPrecinct2020 <- blockToPrecinct2020 %>%
  mutate(
    GEOID = as.character(GEOID20),
    Precinct2020 = as.character(Precinct)
  ) %>%
  select(GEOID, Precinct2020)

blocksToOtherGeographies2020 <- blocksToOtherGeographies2020 %>%
  mutate(
    GEOID = as.character(GEOID),
    TRACT2020 = as.character(TRACT),
    BLKGRP2020 = as.character(BLKGRP),
    BLOCK2020 = as.character(BLOCK),
    WARD2020 = as.character(WARD)
  ) %>%
  select(GEOID, TRACT2020, BLKGRP2020, BLOCK2020, WARD2020)

blocksToNeighborhoods <- blocksToNeighborhoods %>% mutate(GEOID = as.character(GEOID), NBHD = as.character(NBHD))

geo2fips_no_memoize_na <- function(lat, long, census_year, retries = 25, delay = 1) {
  url <- sprintf("https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json&censusYear=%d", lat, long, census_year)
  attempt <- 1
  
  # Create an HTTP handle to manage connections
  handle <- httr::handle(url)
  
  while (attempt <= retries) {
    try(
      {
        response <- GET(url, handle = handle)
        if (http_status(response)$category == "Success") {
          if (length(content(response)$results) == 0) {
            return(NA)  # Do not memoize NA values
          }
          res <- content(response)$results[[1]]$block_fips
          if (length(res) > 0) {
            return(res)
          } else {
            stop()
          }
        } else {
          stop()
        }
      },
      silent = TRUE
    )
    
    attempt <- attempt + 1
    Sys.sleep(delay) # Wait before retrying
  }
  
  warning("Max retries reached for lat: ", lat, ", long: ", long, ". Returning NA.")
  return(NA)
}

# Wrap the function in memoise but prevent caching NA results
geo2fips <- memoise::memoise(function(lat, long, census_year) {
  result <- geo2fips_no_memoize_na(lat, long, census_year)
  if (is.na(result)) {
    return(geo2fips_no_memoize_na(lat, long, census_year))  # Direct call, no memoization
  }
  return(result)
}, cache = memoise::cache_filesystem(cache_dir))

# main actual geolocating function
geocode_address <- memoise::memoise(function(address) {
  # Geocode the address and join additional data
  geo_data <- tidygeocoder::geo(
    address = address,
    method = "arcgis",
    verbose = FALSE,
    full_results = TRUE,
    progress_bar = FALSE,
    quiet = TRUE
  )

  # Check for NA in lat or long
  if (is.na(geo_data$lat) || is.na(geo_data$long)) {
    return(tibble(
      address = address,
      matched_address = NA_character_,
      match_score = NA_character_,
      lat_geo = NA_real_,
      long_geo = NA_real_,
      blockCode_2010 = NA_character_,
      blockCode_2020 = NA_character_
    ))
  }

  geo_data %>%
    mutate(
      blockCode_2010 = map_chr(geo2fips(lat, long, 2010), ~
        if_else(length(.x) > 0,
          .x[1],
          NA_character_
        )),
      blockCode_2020 = map_chr(geo2fips(lat, long, 2020), ~
        if_else(length(.x) > 0,
          .x[1],
          NA_character_
        ))
    ) %>%
    select(-c(
      location.x,
      location.y,
      extent.xmin,
      extent.ymin,
      extent.xmax,
      extent.ymax
    )) %>%
    rename(
      matched_address = address,
      match_score = score,
      lat_geo = lat,
      long_geo = long
    ) %>%
    mutate(
      blockCode_2010 = as.character(blockCode_2010),
      blockCode_2020 = as.character(blockCode_2020)
    ) %>%
    left_join(blocksToOtherGeographies2010,
      by = c("blockCode_2010" = "GEOID")
    ) %>%
    left_join(blocksToOtherGeographies2020,
      by = c("blockCode_2020" = "GEOID")
    ) %>%
    left_join(blocksToNeighborhoods, by = c("blockCode_2010" = "GEOID")) %>%
    left_join(blockToPrecinct2010, by = c("blockCode_2010" = "GEOID")) %>%
    left_join(blockToPrecinct2020, by = c("blockCode_2020" = "GEOID"))
}, cache = memoise::cache_filesystem(cache_dir))

# test <- geocode_address("74 GILMAN ST Somerville, MA")

# Main geolocation function with progress
geolocationFromTableUsingAddress <- function(table) {
  num_cores <- detectCores() - 1

  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) :elapsed Elapsed, :eta remaining.",
    total = nrow(table),
    clear = FALSE,
    width = 60
  ) # Initialize progress estimator
  table <- table %>% select(address)

  print("Creating cluster")
  # Create a cluster for parallel execution
  cl <- makeCluster(num_cores)

  print("Downloading packages in each thread")
  clusterEvalQ(cl, {
    # Download all packages
    if (!require(pacman, quietly = TRUE)) {
      install.packages("pacman")
    }

    pacman::p_load(
      purrr,
      tidyverse,
      tidygeocoder,
      tibble,
      readxl,
      httr,
      dplyr,
      stringi,
      sp,
      sf,
      magrittr,
      stringr,
      memoise,
      tidyr,
      gt
    )
  })

  print("Exporting functions and variables to each thread")
  
  ## Export necessary functions to the cluster
  clusterExport(cl, varlist = c("geocode_address",
                                "geo2fips",
                                "geo2fips_no_memoize_na",
                                "blocksToNeighborhoods",
                                "blocksToOtherGeographies2010",
                                "blocksToOtherGeographies2020",
                                "blockToPrecinct2010",
                                "blockToPrecinct2020")) 
  
  print("Starting to geolocate!")

  # Use parLapply for parallel execution
  results <- parLapply(cl, seq_len(nrow(table)), function(i) {
    # Call geocode_address and process the result
    address <- table$address[i]
    geocode_result <- geocode_address(address)

    tibble(
      address = address,
      matched_address = as.character(geocode_result$matched_address %||% NA_character_),
      match_score = as.character(geocode_result$match_score %||% NA_character_),
      lat_geo = as.character(geocode_result$lat_geo %||% NA_character_),
      long_geo = as.character(geocode_result$long_geo %||% NA_character_),
      BLOCKCODE2010 = as.character(geocode_result$blockCode_2010 %||% NA_character_),
      BLOCKCODE2020 = as.character(geocode_result$blockCode_2020 %||% NA_character_),
      TRACT2010 = as.character(geocode_result$TRACT2010 %||% NA_character_),
      BLKGRP2010 = as.character(geocode_result$BLKGRP2010 %||% NA_character_),
      WARD2010 = as.character(geocode_result$WARD2010 %||% NA_character_),
      TRACT2020 = as.character(geocode_result$TRACT2020 %||% NA_character_),
      BLKGRP2020 = as.character(geocode_result$BLKGRP2020 %||% NA_character_),
      WARD2020 = as.character(geocode_result$WARD2020 %||% NA_character_),
      NBHD = as.character(geocode_result$NBHD %||% NA_character_),
      PRECINCT2010 = as.character(geocode_result$Precinct2010 %||% NA_character_),
      PRECINCT2020 = as.character(geocode_result$Precinct2020 %||% NA_character_)
    )
  })

  # Stop the cluster after processing
  stopCluster(cl)

  results <- bind_rows(results)
  return(results)
}

# # # Example input data
# table <- tibble::tibble(
#   id = c(1:11),
#   address = c("478 ARTISAN WY Somerville, MA",
#               "74 GILMAN ST Somerville, MA",
#               "1 Microsoft Way, Redmond, WA",
#               "725 Granville St, Vancouver, BC",
#               "1 Hacker Way, Menlo Park, CA",
#               "345 Park Ave, San Jose, CA",
#               "1 Main St, Cambridge, MA",
#               "1 Canal Park, Cambridge, MA",
#               "1 Broadway, Cambridge, MA",
#               "1 Kendall Square, Cambridge, MA",
#               "18 Russell Street, Cambridge, MA")
# )

# # Run geolocation with progress
# test2 <- geolocationFromTableUsingAddress(table)
#
# # Output the result
# print(test2)
#

# Main geolocation function with progress
geolocationFromTableUsingLatLong <- function(table, census_year = 2020) {
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) :elapsed Elapsed, :eta remaining.",
    total = nrow(table),
    clear = FALSE,
    width = 60
  ) # Initialize progress estimator
  table <- table %>% select(lat, long)
  results <- pmap(table, function(lat, long) {
    pb$tick() # Increment progress bar

    blockCode_2010 <- if (is.na(lat) | is.na(long)) {
      NA
    } else {
      
      blockCode_2010 <- geo2fips(lat, long, 2010)[[1]]
      if (length(blockCode_2010) > 0) {
        blockCode_2010[1]
      } else {
        NA
      }
    }

    blockCode_2020 <- if (is.na(lat) | is.na(long)) {
      NA
    } else {
      blockCode_2020 <- geo2fips(lat, long, 2020)[[1]]
      if (length(blockCode_2020) > 0) {
        blockCode_2020[1]
      } else {
        NA
      }
    }

    result <- tibble(
      lat_geo = lat,
      long_geo = long,
      blockCode_2010 = blockCode_2010,
      blockCode_2020 = blockCode_2020
    )

    # Convert block codes to character
    result <- result %>%
      mutate(
        blockCode_2010 = as.character(blockCode_2010),
        blockCode_2020 = as.character(blockCode_2020)
      )

    # Join with additional datasets
    result <- result %>%
      left_join(blocksToOtherGeographies2010,
        by = c("blockCode_2010" = "GEOID")
      ) %>%
      left_join(blocksToOtherGeographies2020,
        by = c("blockCode_2020" = "GEOID")
      ) %>%
      left_join(blocksToNeighborhoods, by = c("blockCode_2010" = "GEOID")) %>%
      left_join(blockToPrecinct2010, by = c("blockCode_2010" = "GEOID")) %>%
      left_join(blockToPrecinct2020, by = c("blockCode_2020" = "GEOID"))

    return(result)
  })

  results <- bind_rows(results)
  return(results)
}

# # Example input data
# table <- tibble::tibble(
#   lat = c(37.423022, 37.33182, 47.63962, 49.28273, 37.4844, 37.3382, 42.3736, 42.3668, 42.3624, 42.3627, 42.3731),
#   long = c(-122.083739, -122.03118, -122.12775, -123.12073, -122.1484, -121.8863, -71.1097, -71.0803, -71.0842, -71.0867, -71.1132)
# )
#
# # # Run geolocation with progress using lat and long as input
# test <- geolocationFromTableUsingLatLong(table)
# #
# # # # Output the result
# # print(test)
# #
# elections_geocode_result <- geolocationFromTableUsingAddress(elections45k)

