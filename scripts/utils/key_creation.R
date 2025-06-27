normalize <- function(x) {
  tolower(gsub("\\s+", " ", trimws(ifelse(is.na(x), "", x))))
}

make_key <- function(primary_line, secondary_line, city, state, zip) {
  paste(normalize(c(primary_line, secondary_line, city, state, zip)), collapse = "|")
}
