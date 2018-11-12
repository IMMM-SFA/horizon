# helper functions for unit testing

input_file_dir <- system.file("extdata/", package = "horizon")

# is_input_csv
# T/F for whether a file name has ".csv" at the end
is_input_csv <- function(file){
  substr(file, nchar(file) - 3, nchar(file)) == ".csv"
}

# check_header_names
# are headers all there and named correctly
check_header_names <- function(file){
  read_csv(paste0(input_file_dir, "/", file),
           col_types = cols("D", "d", "d", "d")) %>%
    names() %in%  required_variable_names %>%
    all()
}


system.file("extdata/",
            package = "horizon")
