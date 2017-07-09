
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
  tot_coun <- med_cl[, tot_coun := sum(mean), by = c('location_id','sex_id') ] 
  tot_all  <- med_cl[, tot_all := sum(mean), by = c('location_id')]
  tot_rat  <- med_cl[, tot_rat := tot_coun/tot_all]
  
  coun_df <- unique(med_cl[, .(cause_name, location_name, sex_name, tot_rat)])
  dat_end <- rbind.data.frame(dat_end, coun_df)
  
  b2 <- Sys.time(); print(b2 - a2)
}
b <- Sys.time(); b - a

save(dat_end, file = "dat_disea_allcountries_ratio_mf.RData")
write.table(dat_end, file = "dat_disea_allcountries_ratio.csv", sep = ",", row.names = FALSE, dec = ".", quote = FALSE)
#load(file = "dat_disea_allcountries_ratio_mf.RData")


#----------------- Charts
library(ggplot2)
library(ggalt)
library(dplyr)

#----------------------------------------------------
# Separate diseses in two groups to plot them easier
dif_disea <- unique(dat_end$cause_name)
num_disea <- length(dif_disea)
gr_a <- dif_disea[1:round(num_disea/4,0)]
dat_end$group <- ifelse(dat_end$cause_name %in% gr_a, 'gr_1', 'gr_2')   
df_a <- dat_end[ dat_end$group == "gr_1", ]
df_b <- dat_end[ dat_end$group == "gr_2", ]

# Chart it in two groups to see them easier  
library(plotluck)

plotluck(df_a, cause_name ~ tot_rat| sex_name )
ggsave("Boxplot_Median_Deaths_Ratios_country_disease_A_.eps", device = "eps")

plotluck(df_b, cause_name ~ tot_rat| sex_name)
ggsave("Boxplot_Median_Deaths_Ratios_country_disease_B_.eps", device = "eps")

# Chart in traditional way.
gg <- ggplot(data = df_a, aes(x = cause_name, y = tot_rat, fill = sex_name)) +
  geom_violin(position = dodge)+
  # geom_boxplot(width=.1, outlier.colour=NA, position = dodge) +
  coord_flip()
gg
gg <- gg + labs(x = "Median deaths ratios", y = NULL,
                title = "Sex Deaths Disparity",
                caption = "Data from UN")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(axis.title.x = element_text(hjust = 1, face = "italic", margin = margin(t = -24)))
gg <- gg + theme(plot.caption = element_text(size = 8, margin = margin(t = 24)))
gg
ggsave("Boxplot_Median_Deaths_Ratios_country_disease_grA_.eps", device = "eps")


#---------------------------------------------------------------
#--------- Differences
diff_2 <- function(x) c(0,-diff(x))
dat_end[, dif_rat := diff_2(tot_rat), by="location_name"]
dat_end[, group := NULL] 
dat_dif <- dat_end[ sex_name != "Male"]

#---------------------
# Separate diseses in several groups to plot them easier
dif_disea <- unique(dat_dif$cause_name)
num_gr <- 6
gr_val <- as.numeric(cut_number(1:length(dif_disea), num_gr))
nam_df <- data.frame(nam=dif_disea, grp = gr_val)

dat_grp <- merge(dat_dif, nam_df, by.x = "cause_name", by.y = "nam", sort = FALSE )


# Chart in traditional way.
my_gg <- function(df_x) {
gg <- ggplot(data = df_x, aes(x = cause_name, y = dif_rat )) +
  geom_violin(fill = "tomato") +
  coord_flip()
gg
gg <- gg + labs(x = "Median Deaths differences", y = NULL,
                title = "Sex Deaths Disparity",
                caption = "Data from UN")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(axis.title.x = element_text(hjust = 1, face = "italic", margin = margin(t = -24)))
gg <- gg + theme(plot.caption = element_text(size = 8, margin = margin(t = 24)))
gg
ggsave(paste("Boxplot_Differences_Median_Deaths_Ratios_country_disease_",i,"_.eps", sep =""), device = "eps" )

}

for(i in 1:num_gr) {
  df_x <- dat_grp[ grp == i]
  my_gg(df_x) 
}

= i]
  my_gg(df_x) 
}


