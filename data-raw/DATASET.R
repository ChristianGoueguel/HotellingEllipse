## code to prepare `DATASET` dataset goes here
specData <-
  data.table::fread("~/Documents/Laserag/R&D projects/Soil/Eurofins/Texture/processed data/Data_10mJ/data_avg.csv") %>%
  select(-c(Group.1:Silt)) %>%
  select(-c(`199.3771616`:`240.0751218`))

usethis::use_data(DATASET, overwrite = TRUE)
