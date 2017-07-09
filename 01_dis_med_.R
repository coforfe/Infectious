

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
dat_end <- data.frame( cause = 0, med_mal = 0, med_fem = 0, med_tot = 0)
for (i in 1:nrow(nam_files)) {
# for (i in 1:3) {
  print(nam_files$V1[i])
  a2 <- Sys.time()
  
  file_tmp <- fread(nam_files$V1[i], header = TRUE)
  
  med_all <- file_tmp[location_name != "Global" & year == 2013 & age_group_name != "All Ages" &  unit == "number" ]
  # loc_all <- unique(med_all[ ,.(location_id, location_code, location_name)])
  to_rem <- c(2, 3, 4, 5,9,31,32,42,56,64,65,73,96,100,103,104,120,124,134,137,138,166,167,174,192,199)
  med_cl <- med_all[ !(location_id %in% to_rem),]
  
  med_tot <- median(as.numeric(med_cl$mean)) 
  med_mal <- median(as.numeric(med_cl[ sex_id == 1, mean]))
  med_fem <- median(as.numeric(med_cl[ sex_id == 2, mean]))
  cau_se <- unique(med_cl$cause_name)
  
  dat_end[i, 1] <- cau_se
  dat_end[i, 2] <- med_mal
  dat_end[i, 3] <- med_fem
  dat_end[i, 4] <- med_tot
   
  b2 <- Sys.time(); print(b2 - a2)
}
b <- Sys.time(); b - a

save(dat_end, file = "dat_med_end.RData")
write.table(dat_end, file = "dat_med_end.csv", sep = ",", row.names = FALSE, dec = ".", quote = FALSE)


