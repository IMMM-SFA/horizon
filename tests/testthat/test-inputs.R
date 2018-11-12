context("test-inputs")

library(purrr)
library(readr)

# get all files in extdata folders
all_files <- list.files(system.file("extdata/",
                                   package = "horizon"),
                        recursive = T)

# filter for processed variable files (storage, inflow, release)
processed_variable_files <- all_files[which(grepl("processed/", all_files))]

test_that("input files are type .csv", {
  expect_true(all(
    map_lgl(processed_variable_files, is_input_csv)
  ))
})

test_that("all variables are present and named correctly", {
  expect_true(all(
    map_lgl(processed_variable_files, check_header_names)
  ))
})
