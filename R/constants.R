# constants

# data set up
required_variable_names <- c("date", "s_af", "i_cfs", "r_cfs")

# unit conversion
m3_to_Mm3 <- 1e-6
cfs_to_Mm3day <- 0.0283168 * (60 * 60 * 24) * 1e-6
af_to_Mm3 <- 1233.48 * 1e-6
seconds_per_day <- 86400
