

#Join all Trap dataframes together by "Species"
Floristics_2021 <- bind_rows(Trap1, Trap2, Trap3, Trap4, Trap5, Trap6, Trap7,Trap8)
unique(Floristics_2021$Species)
unique(Floristics_2021$Trap)

#Change "wahlenbergia" to "Wahlenbergia"
Floristics_2021$Species[Floristics_2021$Species == "wahlenbergia"] <- "Wahlenbergia"
write.csv(Floristics_2021, "data/Floristics_2021.csv")


#Add in a year column to Floristics_2021
Floristics_2021$Year <- 2021
Floristics_2018$Year <- 2018

#remove the first column from Floristics_2018
Floristics_2021 <- Floristics_2021[, -c(6:26)]


#Join Floristics_2018 and Floristics_2021
Floristics <- bind_rows(Floristics_2018, Floristics_2021)

unique(Floristics$Species)

#Change "Argyrotegium fordianum/Euchiton fordianus" to "Argyrotegium fordianum"
Floristics$Species[Floristics$Species == "Argyrotegium fordianum/Euchiton fordianus"] <- "Argyrotegium fordianum"
Floristics$Species[Floristics$Species == "Craspedia coolaminica"] <- "Craspedia gracilis"
Floristics$Species[Floristics$Species == "Erigeron bellidiodes"] <- "Pappochroma bellidioides"

#Check for missing data
sum(is.na(Floristics))

#change any values in the 1m column that are >0 to a 1
Floristics$`1m`[Floristics$`1m` > 0] <- 1
Floristics$`5m`[Floristics$`5m` > 0] <- 1
Floristics$`10m`[Floristics$`10m` > 0] <- 1
Floristics$`15m`[Floristics$`15m` > 0] <- 1

#make any NAs 0
Floristics[is.na(Floristics)] <- 0


######### 
###### Read in data ####
####
Floristics <- read.csv("data/Floristics_PA_long.csv")

#Spread the data to make it wide by distance
Floristics_w <- spread(Floristics, key = Distance, value = Cover)

#Change the column name "1m" to "1" 
colnames(Floristics_w)[colnames(Floristics_w) == "1m"] <- "1"
colnames(Floristics_w)[colnames(Floristics_w) == "5m"] <- "5"
colnames(Floristics_w)[colnames(Floristics_w) == "10m"] <- "10"
colnames(Floristics_w)[colnames(Floristics_w) == "15m"] <- "15"

#Make the data long to add in a column for distance
Floristics_long <- pivot_longer(Floristics_w, cols = c(`1`, `5`, `10`, `15`), 
                                names_to = "Distance", values_to = "Cover")

#Remove any 0s
Floristics_long <- Floristics_long[Floristics_long$Cover != 0,]

#Change the distance column to numeric
Floristics_long$Distance <- as.numeric(Floristics_long$Distance)

#Calcuate average distance of each species in each trap
Floristics_avg <- Floristics_long %>%
  group_by(Species) %>%
  summarise(mean_distance = mean(as.numeric(Distance)))

#Calcuate minimum distance of each species in each trap
Floristics_min <- Floristics_long %>%
  group_by(Species) %>%
  summarise(min_distance = min(as.numeric(Distance)))

#Save the data
write.csv(Floristics_avg, "data/Average distance dispersed.csv", row.names = FALSE)
write.csv(Floristics_min, "data/Minimum distance dispersed.csv", row.names = FALSE)

