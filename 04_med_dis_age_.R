

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
for (i in 1:nrow(nam_files)) {
  # for (i in 1:3) {
  print(i)
  print(nam_files$V1[i])
  a2 <- Sys.time()
  
  file_tmp <- fread(nam_files$V1[i], header = TRUE)
  
  med_all <- file_tmp[location_name != "Global" & year == 2013 & age_group_name != "All Ages" &  unit == "number" ]
  # loc_all <- unique(med_all[ ,.(location_id, location_code, location_name)])
  to_rem <- c(2, 3, 4, 5,9,31,32,42,56,64,65,73,96,100,103,104,120,124,134,137,138,166,167,174,192,199)
  med_cl <- med_all[ !(location_id %in% to_rem),]
  
  # by country
  med_cl$mean <- as.numeric(med_cl$mean)  #in some cases are character
  tot_age  <- med_cl[, tot_age := sum(mean), by=c('age_group_id','sex_id') ] 
  tot_all  <- med_cl[, tot_all := sum(mean), by=c('age_group_id')]
  tot_rat  <- med_cl[, tot_rat := tot_age/tot_all]
  med_age  <- med_cl[, med_age := median(tot_rat), by=c('age_group_id', 'sex_id')]
  
  age_df <- unique(med_cl[, .(cause_name, age_group_name, sex_name, med_age)])
  
  dat_end <- rbind.data.frame(dat_end, age_df)
  
  
  b2 <- Sys.time(); print(b2 - a2)
}
b <- Sys.time(); b - a

save(dat_end, file = "dat_med_age_end.RData")
write.table(dat_end, file = "dat_med_age_end.csv", sep = ",", row.names = FALSE, dec = ".", quote = FALSE)


library(ggplot2)
library(ggalt)
library(dplyr)

df <- dat_end
lev_age <- unique(df$age[1:40])
df$age <- factor(df$age, ordered = TRUE, levels = lev_age, labels = lev_age)
df$sex <- factor(df$sex, ordered = TRUE, levels = c("Male", "Female"), labels = c("Male", "Female"))

# Males and Females separately
pdf(file = "Deaths Disparity by Age and Disease.pdf")
library(lattice)
#df_new <- df[df$cause_name=="Tuberculosis",]
mytheme <- simpleTheme( col.points = c("blue", "pink"), fill = c("blue", "pink"), pch = 19, cex = 0.7 )
tt <- dotplot(
                 age ~ med_age | cause_name,
                 groups = sex,
                 data = df,
                 scales = list(y = list(cex = 0.5, col = "blue"), x = list(cex=0.75, col ="red")),
                 par.strip.text = list(col = "white", cex = 0.85),
                 layout = c(3,3),
                 xlab = "Median Deaths",
                 par.settings = list(
                   superpose.symbol = list(
                     pch = 21,
                     fill = c("blue","red"),
                     cex = 0.50,
                     col = "black"
                   ),
                   strip.background = list(col = "black"),
                   background  = list(col = "white")),
                 as.table = TRUE
             )
print(tt)
dev.off()



# Differences
gg <- ggplot(df, aes(x=med_dif, xend=med_dif, y=disease))
gg <- gg + geom_dumbbell(colour="#686868",
                         colour_xend="blue",
                         colour_x="blue",
                         size_x=2.5,
                         size_xend=2.5)
gg <- gg + labs(x="(Male minus Female)", y=NULL,
                title="Gender Deaths Differences",
                caption="Data from UN")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))
gg <- gg + geom_vline(xintercept = 0, color = "red")
gg




x="blue",
                         size_x=2.5,
                         size_xend=2.5)
gg <- gg + labs(x="(Male minus Female)", y=NULL,
                title="Gender Deaths Differences",
                caption="Data from UN")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))
gg <- gg + geom_vline(xintercept = 0, color = "red")
gg




