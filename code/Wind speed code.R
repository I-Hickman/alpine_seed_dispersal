##########
### ALPINE SEED DISPERSAL PROJECT ########
##########
#####
## Q: Wind speed at different heights and wind direction
######
#Paired t-test 


# Required packages
require(DHARMa)
require(lme4)
require(emmeans)
require(performance)
require(report)
require(ggplot2)
require(tidyverse)
library(cowplot)
library(arm)
library(mgcv)
library(ggeffects)

### read in files
wind <- read.csv("data/Wind speed.csv")

#test assumptions for a paired t-test
#normality
qqnorm(wind$Horizontal)
qqline(wind$Horizontal)
qqnorm(wind$Vertical)

#Violated assumptions - sqrt transform
wind$Horizontal.sqrt <- sqrt(wind$Horizontal)
wind$Vertical.sqrt  <- sqrt(wind$Vertical)

#paired t test on horizontrol and vertical wind speeds
#T-test
t.test(wind$Horizontal.sqrt, wind$Vertical.sqrt, paired = TRUE)

#Calcuate the mean and confidence intervals of the vertical 
#and horizontal wind speeds forn each height category
wind2 <- wind[,c(2,4,5)]

wind3 <- wind2 %>%
  group_by(Height.above.Ground..cm.) %>% 
  gather(key = "Wind", value = "Speed", 2:3)

wind4 <- wind3 %>% 
  group_by(Wind, Height.above.Ground..cm.) %>% 
  summarise(mean = mean(Speed, na.rm = TRUE),
            sd = sd(Speed, na.rm = TRUE),
            se = sd/sqrt(n()),
            ci = qt(0.975, n()-1)*se)

wind4 <- na.omit(wind4)

#plot the data and group by wind
wind_plot <- ggplot(wind4, aes(x = Height.above.Ground..cm., y = mean, fill = Wind)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.3), alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2, 
                position = position_dodge(width = 0.9)) + 
  theme_cowplot() +
  ylab("Wind speed (m/s)") +
  xlab("Height above ground (cm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Save
ggsave("output/Traps winds plots.png", 
       plot = wind_plot, width = 8, height = 3.5) 

