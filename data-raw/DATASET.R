## code to prepare `specData` dataset goes here
specData <-
  data.table::fread("~/Documents/Laserag/R&D projects/Soil/Eurofins/Texture/processed data/Data_10mJ/specDatafull.csv") %>%
  dplyr::select(-c(V1:V1000))

usethis::use_data(specData, overwrite = TRUE, compress = "xz")

