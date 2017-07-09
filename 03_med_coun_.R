
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
dat_end <- data.frame( cause = 0, med_mal = 0, med_fem = 0, med_dif = 0)
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
  
  med_mal <- median(as.numeric(med_cl[ sex_id == 1, tot_rat]), na.rm = TRUE)
  med_fem <- median(as.numeric(med_cl[ sex_id == 2, tot_rat]), na.rm = TRUE)
  med_dif <- med_mal - med_fem
  
  cau_se <- unique(med_cl$cause_name)
  
  dat_end[i, 1] <- cau_se
  dat_end[i, 2] <- med_mal
  dat_end[i, 3] <- med_fem
  dat_end[i, 4] <- med_dif
  
  b2 <- Sys.time(); print(b2 - a2)
}
b <- Sys.time(); b - a

save(dat_end, file = "dat_med_country_end.RData")
write.table(dat_end, file = "dat_med_country_end.csv", sep = ",", row.names = FALSE, dec = ".", quote = FALSE)


library(ggplot2)
library(ggalt)
library(dplyr)
dat_end <- as.data.table(dat_end)
dat_end <- unique(dat_end)
dat_end <- dat_end[!is.na(dat_end$med_mal), ] # Ine Row is has NA (Chlamydial)

df <- dat_end
df <- arrange(df, desc(med_mal))
df$disease <- factor(df$cause, ordered = TRUE, levels = df$cause, labels = df$cause)
# df <- mutate(df, disease=factor(cause, levels=rev(cause)))

# Maes and Females separately
gg <- ggplot(df, aes(x = med_fem, xend = med_mal, y = disease))
gg <- gg + geom_dumbbell(colour = "#686868",
                         colour_x = "pink",
                         colour_xend = "blue",
                         size_x = 2.5,
                         size_xend = 2.5)
# gg <- gg + scale_x_continuous(breaks=seq(60, 160, by=20),
#                               labels=sprintf("$%sK", comma(seq(60, 160, by=20))))
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
ggsave("Median_Deaths_Ratios_Country.eps", device = "eps")


# Differences
gg <- ggplot(df, aes(x = med_dif, xend = med_dif, y = disease))
gg <- gg + geom_dumbbell(colour = "#686868",
                         colour_xend = "blue",
                         colour_x = "blue",
                         size_x = 2.5,
                         size_xend = 2.5)
gg <- gg + labs(x = "(Male minus Female)", y = NULL,
                title = "Sex Deaths Differences",
                caption = "Data from UN")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(axis.title.x = element_text(hjust = 1, face = "italic", margin = margin(t = -24)))
gg <- gg + theme(plot.caption = element_text(size = 8, margin = margin(t = 24)))
gg <- gg + geom_vline(xintercept = 0, color = "red")
gg
ggsave("Median_Deaths_Differences_Country.eps", device = "eps")


# Differences - Boxplots
y <- df$med_dif
df_box <- data.frame(
  x = 1,
  y0 = min(y),
  y25 = quantile(y, 0.25),
  y50 = median(y),
  y75 = quantile(y, 0.75),
  y100 = max(y)
)
gg <- ggplot(df_box, aes(x)) +
  geom_boxplot(
    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
    stat = "identity"
  )
gg
gg <- gg + coord_flip()
gg
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(axis.title.x = element_text(hjust = 1, face = "italic", margin = margin(t = -24)))
gg <- gg + theme(plot.caption = element_text(size = 8, margin = margin(t = 24)))
gg
ggsave("Boxplot_Median_Deaths_Differences_Country.eps", device = "eps")


# Differences - Violin
ggplot(df, aes(x = 1:nrow(df), y = med_dif)) + 
  geom_violin(trim = FALSE, fill = "steelblue") +
  labs(title = "Differences Sex Deaths", x = "", y = "Difference") +
  geom_boxplot(width = 0.1, fill = "tomato") +
  theme_classic() + coord_flip() +
  scale_fill_brewer(palette = "RdBu") + theme_minimal()
ggsave("Violinplot_Median_Deaths_Differences_Country.eps", device = "eps")

)
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(axis.title.x = element_text(hjust = 1, face = "italic", margin = margin(t = -24)))
gg <- gg + theme(plot.caption = element_text(size = 8, margin = margin(t = 24)))
gg <- gg + geom_vline(xintercept = 0, color = "red")
gg


