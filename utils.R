# Define a custom function to relabel NA
relabel_na <- function(x) {
  x[is.na(x)] <- "No available data" # Check for both R's NA and string "NA"
  return(x)
}