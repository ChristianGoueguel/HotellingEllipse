## code to prepare `specData` dataset goes here
specData <-
  data.table::fread("~/Documents/Laserag/R&D projects/specData.csv") %>%
  dplyr::select(-c(V1:V1500))

usethis::use_data(specData, overwrite = TRUE, compress = "xz")
