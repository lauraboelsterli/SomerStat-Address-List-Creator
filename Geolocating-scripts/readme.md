
# Geolocation Script

## Description

This project contains scripts used for geolocating addresses and latitude/longitude coordinates to obtain FIPS (Federal Information Processing Standards) codes via the FCC's Census Block API. The script is designed to efficiently handle large datasets by caching API responses and providing real-time progress tracking.

The functions responsible for geocoding and fetching FIPS codes are defined in the `geolocation_functions.R` script. These functions can process both address data and latitude/longitude pairs, returning the necessary geolocation data in a structured format. The caching mechanism ensures that repeated requests for the same data do not result in unnecessary API calls, improving both speed and efficiency.

## Dependencies

The script requires several R libraries to run successfully. The script first checks if `pacman` is installed and then uses it to install and load the necessary packages. If you encounter errors indicating missing packages, such as:

```r
Error in library("dplyr") : there is no package called 'dplyr'
```

You can manually install the missing packages with:

```r
install.packages("dplyr")
```

Additionally, a caching directory is required for the memoization of function results. This directory is specified in the variable `cache_dir`, and if it does not exist, the script will automatically create it.

## Configuration

To run this script, ensure the `cache_dir` is correctly set up. The default path is specified in the script, but it should be modified to fit your local environment if necessary.

## Functions and Usage

### `geocode_address(address)`

#### Description

This function takes a single address as input and returns its corresponding latitude and longitude. It uses the `tidygeocoder` package to perform the geocoding.

#### Usage

```r
geocode_result <- geocode_address("1600 Amphitheatre Parkway, Mountain View, CA")
print(geocode_result)
```

#### Output

The function returns a data frame containing the following columns:

- `matched_address`: The address that was matched by the geocoding service.
- `match_score`: A score indicating how good the match is (100 is perfect). Anything other than 100 is pretty questionable.
- `lat_geo`: Latitude of the geocoded address.
- `long_geo`: Longitude of the geocoded address.
- `blockCode_2010`: FIPS code corresponding to the latitude/longitude from the 2010 census.
- `blockCode_2020`: FIPS code corresponding to the latitude/longitude from the 2020 census.
- `TRACT2010`: The 2010 census tract code.
- `TRACT2020`: The 2020 census tract code.
- `BLKGRP2010`: The 2010 census block group code.
- `BLKGRP2020`: The 2020 census block group code.
- `WARD2010`: The 2010 ward code.
- `WARD2020`: The 2020 ward code.
- `PRECINCT2010`: The 2010 precinct code.
- `PRECINCT2020`: The 2020 precinct code.
- `NBHD`: The neighborhood code (uses 2010 block code to compute).

### `geolocationFromTableUsingAddress(table, census_year = 2020)`

#### Description

This function processes a table of addresses, geocodes each address, and then retrieves the corresponding FIPS codes for each geocoded location.

#### Usage

```r
table <- tibble::tibble(
  id = c(1:3),
  address = c("1600 Amphitheatre Parkway, Mountain View, CA",
              "1 Infinite Loop, Cupertino, CA",
              "1 Microsoft Way, Redmond, WA")
)

results <- geolocationFromTableUsingAddress(table)
print(results)
```

#### Input

- `table`: A data frame with at least two columns: `id` and `address`.
- `census_year`: The census year to use when fetching FIPS codes (default is 2020).

#### Output

The function returns a data frame with the following columns:

- `id`: The original ID from the input table.
- `address`: The original address from the input table.
- `matched_address`: The address that was matched by the geocoding service.
- `match_score`: A score indicating how good the match is (100 is perfect). Anything other than 100 is pretty questionable.
- `lat_geo`: Latitude of the geocoded address.
- `long_geo`: Longitude of the geocoded address.
- `blockCode_2010`: FIPS code corresponding to the latitude/longitude from the 2010 census.
- `blockCode_2020`: FIPS code corresponding to the latitude/longitude from the 2020 census.
- `TRACT2010`: The 2010 census tract code.
- `TRACT2020`: The 2020 census tract code.
- `BLKGRP2010`: The 2010 census block group code.
- `BLKGRP2020`: The 2020 census block group code.
- `WARD2010`: The 2010 ward code.
- `WARD2020`: The 2020 ward code.
- `PRECINCT2010`: The 2010 precinct code.
- `PRECINCT2020`: The 2020 precinct code.
- `NBHD`: The neighborhood code (uses 2010 block code to compute).

### `geolocationFromTableUsingLatLong(table, census_year = 2020)`

#### Description

This function processes a table of latitude and longitude coordinates, fetching the corresponding FIPS codes for each location.

#### Usage

```r
table <- tibble::tibble(
  id = c(1:3),
  lat = c(37.423021, 37.33182, 47.63962),
  long = c(-122.083739, -122.03118, -122.12775)
)

results <- geolocationFromTableUsingLatLong(table)
print(results)
```

#### Input

- `table`: A data frame with at least three columns: `id`, `lat`, and `long`.
- `census_year`: The census year to use when fetching FIPS codes (default is 2020).

#### Output

The function returns a data frame with the following columns:

- `id`: The original ID from the input table.
- `lat_geo`: The original latitude from the input table.
- `long_geo`: The original longitude from the input table.
- `blockCode_2010`: FIPS code corresponding to the latitude/longitude from the 2010 census.
- `blockCode_2020`: FIPS code corresponding to the latitude/longitude from the 2020 census.
- `TRACT2010`: The 2010 census tract code.
- `TRACT2020`: The 2020 census tract code.
- `BLKGRP2010`: The 2010 census block group code.
- `BLKGRP2020`: The 2020 census block group code.
- `WARD2010`: The 2010 ward code.
- `WARD2020`: The 2020 ward code.
- `PRECINCT2010`: The 2010 precinct code.
- `PRECINCT2020`: The 2020 precinct code.
- `NBHD`: The neighborhood code (uses 2010 block code to compute).

## In Order to Use

### Download the Repo and Source the Script

To use the functions in this project, download the repository and then source the `geolocation_functions_for_coding.R` file:

```r
source("path_to_geolocation_repo/scripts/geolocation_functions_for_coding.R")
```

You should now be able to call the functions as shown above.

## Authors

- Derek Schaadt

## Future Development

- [x] **Adding Wards and Other Geographies:** Expanding the script to include additional geographies like wards, tracts, etc.


## Major Update History

### 08/13/2024

- Launched a major update for ease of use, added 2020 and 2010 block code information.

### 07/01/2023

- Initial release of the geolocation script.

---

