specData <-
  data.table::fread("~/Documents/Laserag/R&D projects/Soil/Eurofins/Texture/processed data/Data_10mJ/data_avg.csv") %>%
  dplyr::select(-c(Group.1:Silt)) %>%
  dplyr::select(-c(`199.3771616`:`240.0751218`))

colnames(specData) <- paste("X", 1:6669, sep = "")

usethis::use_data(specData, overwrite = TRUE)
