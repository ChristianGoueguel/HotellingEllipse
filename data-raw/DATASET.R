specData <- data.table::fread("~/Documents/Laserag/R&D projects/Soil/Eurofins/Texture/processed data/Data_10mJ/specData.csv")
usethis::use_data(specData, overwrite = TRUE)
