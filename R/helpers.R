# these functions should not be considered part of the package...
# they merely provide some means to fix problems in the input data...
# ahead of running the core package functions.


# fix_inputs
#
# This function fixes column names where o_cfs is entered instead of r_cfs.

fix_inputs <- function(data_source){

  input_file_dir <- system.file("extdata/", package = "horizon")

  # get files that need correcting
  list.files((paste0(input_file_dir, data_source, "/processed"))) ->
    files_for_correction

  # map through files and replace o_cfs with r_cfs, then rewrite back to file
  files_for_correction %>%
    map(function(file){
      filename <- paste0(input_file_dir, data_source, "/processed/", file)
      read_csv(filename) %>%
        rename(r_cfs = o_cfs) %>%
        write_csv(paste0(getwd(), "/inst/extdata/", data_source, "/processed/", file))
    })
}

# fix_usgs_names

fix_usgs_names <- function(){

  # get directory of usgs data
  input_file_dir = paste0(getwd(), "/inst/extdata/usgs/processed/")

  # get file list to be renamed
  files <- list.files(input_file_dir, full.names = T)

  # rename all files remove the usgs_
  map(files, function(each_path){
    file.rename(from = each_path,
                to = sub("usgs_", "", each_path))
  })
}



# combine_sources <- function(){
#   sys_file_dir <- system.file("extdata/", package = "horizon")
#   list.files(sys_file_dir, recursive = T) %>%
#     .[grepl("processed", .)] -> filenames
#
#   filenames %>%
#     map(function(x) read_csv(paste0(sys_file_dir, x),
#                              col_types = cols("D", "d", "d", "d"))) ->
#     all_obs_data
#   names(all_obs_data) ->
#
#
#   all_files %>%
#
#
#   tibble(file = all_files) %>%
#     separate(file, "/", into = c("source", "xx", "filename")) %>%
#     mutate(filename = case_when(
#       source == "usgs" ~ filename,
#       source != "usgs" ~ paste0(source, filename)
#     )) %>%
#     mutate(file_read = paste0(sys_file_dir, filename))
#
# # gluwasp <- gluwasp::build_gluwasp()
# # usethis::use_data(gluwasp, overwrite = T)
#
#       tibble(file = all_files) %>%
#     mutate(dam = case_when(
#       grepl("_usgs_", file) ~ substr(file, 6, nchar(file) - 4),
#       !grepl("_usgs_", file) ~ substr(file, 1, nchar(file) - 4)
#     )) %>%
#     split(.$dam) %>%
#     purrr::map(function(x){
#       read_csv(x$file)
#     })

