##########
### ALPINE SEED DISPERSAL PROJECT ########
##########
#####
## Q: Proortion of vertical wind events  above the terminal velocity of seeds
######


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
library(ggpubr)

#####
###### Read data  #################
wind_data2 <- read.csv("data/Vertical wind data (cleaned).csv")
species_deets <- read.csv("data/Species details.csv")

#Join data by species
wind_data3 <- merge(wind_data2, species_deets, by = "Species")
wind_data3$perc_threshold_events <- as.numeric(wind_data3$perc_threshold_events)
wind_data3$Tvel <- as.numeric(wind_data3$Tvel)

#Correlation between seed mass and terminal velocity
ggplot(wind_data3, aes(x = Tvel, y = perc_threshold_events)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  xlab("Terminal velocity (s/m)") +
  ylab("Proportion of vertical wind events above threshold")

### Step 3) Data analysis
#Transform response variable - Logit transformation of proportional cover data
Wind_data3 <- wind_data3 %>% 
  dplyr::mutate(qthreshold = qlogis(perc_threshold_events+0.001)) #apply logit transformation - qlogis
anyNA(Wind_data3) 
#remove NA
Wind_data3 <- na.omit(Wind_data3)

#Transform the explanatory variable
Wind_data3$Tvel.log <- log(Wind_data3$Tvel)

###### Step 4) Fit the models
m1 = glm(qthreshold ~ Tvel, 
         family = 'gaussian', Wind_data3)

### Step 6) Model validation
### GLM Check for Normality and homogeneity of variances
res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, Wind_data3$Tvel)
par(op) 

##
##### Model violated assumptions
#### Next step is to use correlation test

######
####### Correlation test ######
#####

##### Test for normality
shapiro.test(wind_data3$perc_threshold_events) #data not normally distributed data
#Need to use spearman correlation

#Plot with R2
cor_plot <- wind_data3 %>%
  ggplot(aes(x = Tvel, y = perc_threshold_events, colour = DS)) +
  geom_point(size = 2, alpha = 1) +
  stat_smooth(method = lm, colour = "black", linewidth = 0.9) + # se = FALSE) +
  stat_cor(aes(label = after_stat(r.label)), 
           method = "spearman",
           label.y = 0.4, label.x = 4,
           colour = "#d95f02",
           cor.coef.name = "rho") +
  stat_cor(aes(label = after_stat(p.label)), 
           method = "spearman",
           label.y = 0.35, label.x = 4,
           colour = "#d95f02") +
  theme(axis.text.y = element_blank()) +
  theme_cowplot() +
  #ylim(-0.03,0.4) +
  labs(x = "Terminal velocity (m/s)", 
       y = "Proportion of vertical wind events > threshold",
       color = "Dispersal syndrome") +
  theme_cowplot()+
  ylab(expression(atop("Proportion of vertical wind", paste("events above threshold"))))+ 
  xlab(expression("Terminal velocity (m/s)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) 
cor_plot

#Plot without correlation 
cor_plot2 <- wind_data3 %>%
  ggplot(aes(x = Tvel, y = perc_threshold_events, colour = DS)) +
  geom_point(size = 2, alpha = 1) +
  stat_smooth(method = gam, 
              colour = "black", 
              na.rm = TRUE,
              linewidth = 0.9, 
              se = FALSE) +
  theme(axis.text.y = element_blank()) +
  theme_cowplot() +
  ylim(0,0.4) +
  labs(x = "Terminal velocity (m/s)", 
       y = "Proportion of vertical wind events > threshold",
       color = "Dispersal syndrome") +
  theme_cowplot()+
  ylab(expression(atop("Proportion of vertical wind", paste("events above threshold"))))+ 
  xlab(expression("Terminal velocity (m/s)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) 
cor_plot2

## With loess function
cor_plot3 <- wind_data3 %>%
  ggplot(aes(x = Tvel, y = perc_threshold_events, colour = DS)) +
  geom_point(size = 2, alpha = 1) +
  stat_smooth(method = loess, 
    colour = "black", 
    na.rm = TRUE,
    linewidth = 0.9,
    se = FALSE) +
  theme(axis.text.y = element_blank()) +
  theme_cowplot() +
  ylim(0,0.4) +
  labs(x = "Terminal velocity (m/s)", 
       y = "Proportion of vertical wind events > threshold",
       color = "Dispersal syndrome") +
  theme_cowplot()+
  ylab(expression(atop("Proportion of vertical wind", paste("events above threshold"))))+ 
  xlab(expression("Terminal velocity (m/s)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) 
cor_plot3

#Save
ggsave("output/Vertical wind events model/Vertical wind correlation plot (without numbers).png", 
       plot = cor_plot2, width = 8, height = 3.5) 
ggsave("output/Vertical wind events model/Vertical wind correlation plot (loess).png", 
       plot = cor_plot3, width = 8, height = 3.5) 


###############################
#### END
###############################

