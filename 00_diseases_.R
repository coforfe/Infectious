
#--------------------------------
# Author: Carlos Ortega
# Diseases Analysis - 2017_07_05
# Input: "Nombres_ficheros_interes.txt"
# Output: DataFrame with 2013 - Name_Disease - All Ages - Value for Females - Males
#--------------------------------


#--------------------------------
# Library Loadings
library(data.table)
library(stringr)

#--------------------------------
# Get Data
nam_files <- fread("Nombres_ficheros_de_interes.txt", header = FALSE)

a <- Sys.time()
dat_end <- data.frame()
# for (i in 1:nrow(nam_files)) {
for (i in 1:1) {
  print(nam_files$V1[i])
a2 <- Sys.time()
  file_tmp <- fread(nam_files$V1[i], header = TRUE)
  val_mal <- file_tmp[location_name != "Global" & year == 2013 & age_group_name != "All Ages" &  unit == "number", ]
  dat_tmp  <- file_tmp[location_name == "Global" & year == 2013 & age_group_name == "All Ages" &  unit == "number"]
  print(i)
  print(dat_tmp)
  dat_end <- rbind.data.frame(dat_end, dat_tmp)
b2 <- Sys.time(); print(b2 - a2)
}
b <- Sys.time(); b - a

save(dat_end, file = "dat_end.RData")
write.table(dat_end, file = "dat_end.csv", sep = ",", row.names = FALSE, dec = ".", quote = FALSE)


