rweekly_data <- data_load()

# save(rweekly_data, file = "data/rweekly_data.rda", compress = "xz")
usethis::use_data(rweekly_data, overwrite = T)
