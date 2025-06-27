
download_raw_adddress_list <- function() {
  base_url <- "https://s3.us-east-1.amazonaws.com/download.massgis.digital.mass.gov/shapefiles/mad/town_exports/adv_addr"
  dest_dir <- "data/raw_files/downloads"
  dir_create(dest_dir)
  
  # Parameters
  somerville_code <- "M274"
  max_consecutive_misses <- 3
  misses <- 0
  
  while (misses < max_consecutive_misses) {
    file_name <- paste0("AdvancedAddresses_", somerville_code, ".zip")
    url <- file.path(base_url, file_name)
    zip_path <- path(dest_dir, file_name)
    # Try to download
    cat("Trying:", url, "\n")
    success <- tryCatch({
      download.file(url, destfile = zip_path, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    # Check if file is valid
    if (success && file.size(zip_path) > 1000) {
      cat("Downloaded:", file_name, "\n")
      unzip(zip_path, exdir = dest_dir)
      break
    } else {
      cat("Missed:", file_name, "\n")
      misses <- misses + 1
      if (file.exists(zip_path)) file_delete(zip_path)
    }
  }
  
  somerville_addresses <- read_excel("data/raw_files/downloads/AdvancedAddresses_M274.xlsx", col_types = "text")
  
  ## Write to CSV
  write_csv(somerville_addresses, "data/raw_files/raw_somerville_addresses.csv")
}
