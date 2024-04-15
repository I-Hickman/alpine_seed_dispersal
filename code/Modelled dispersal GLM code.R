##########
### ALPINE SEED DISPERSAL PROJECT ########
##########
#####
## Q: Relationship between modelled dispersal distance and observed distance dispersed + release height
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
library(modelsummary)


### Step 1) Load data
Modelled_dispersal <- read.csv("data/Modelled dispersal.csv")
Distance <- read.csv("data/Minimum distance dispersed.csv")
mean_distance <- read.csv("data/Average distance dispersed.csv")

#Join distance and mean distance data
Distance <- merge(Distance, mean_distance, by = "Species")

### Step 2) Data exploration
# Check data structure
str(Modelled_dispersal)

#Change column name Mean.predicted.dispersal..m. to mean_pred_distance
colnames(Modelled_dispersal)[colnames(Modelled_dispersal) == 'Mean.predicted.dispersal..m.'] <- 'mean_pred_distance'
colnames(Modelled_dispersal)[colnames(Modelled_dispersal) == 'Release.height..m.'] <- 'release_height'

#Merge the two datasets
Modelled_dispersal_2 <- merge(Modelled_dispersal, Distance, by = "Species")

#Correlation between seed mass and terminal velocity
ggplot(Modelled_dispersal_2, aes(x = release_height, y = mean_pred_distance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  ylab("Predicted dispersal") +
  xlab("Release height")

ggplot(Modelled_dispersal_2, aes(x = mean_distance, y = mean_pred_distance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  ylab("Predicted dispersal") +
  xlab("Release height")

cor.test(Modelled_dispersal_2$mean_pred_distance, Modelled_dispersal_2$mean_distance) # r = 0.4956246  , p = 0.005352
cor.test(Modelled_dispersal_2$mean_pred_distance, Modelled_dispersal_2$release_height) # r = 0.4316504  , p = 0.02181

### Step 3) Data analysis

#Standardise variables
Modelled_dispersal_2$mean_dist.std <- scale(Modelled_dispersal_2$mean_distance)
Modelled_dispersal_2$release_ht.std <- scale(Modelled_dispersal_2$release_height)

#Remove outlier from dataset- species Acaena novae-zelandiae from the data
Modelled_dispersal_2 <- Modelled_dispersal_2[!Modelled_dispersal_2$Species == "Acaena novae-zelandiae",]

#Fit the models
m1 = glm(mean_pred_distance ~ mean_dist.std + release_ht.std,
         family = 'gaussian', Modelled_dispersal_2)

### Step 5) Model validation
# Normality and homogeneity of variances
res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, Modelled_dispersal_2$mean_dist.std)
par(op) # Normality and homogeneity look good for m1 

res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, Modelled_dispersal_2$release_ht.std)
par(op) # Normality and homogeneity look good for m1 

# Check predictions
check_predictions(m1)
dev.off()


### Step 6) Model interpretation
# Model summary
summary(m1)
#none are significant

#### Step 7) Make a coefficient plot
#Formatting
m1_labels <- c(
        'mean_dist.std' = 'Mean distance (m)',
        'release_ht.std' = 'Release height (m)',
        '(Intercept)' = 'Intercept')

b <- list(geom_vline(xintercept = 0, color = 'red'))

#Make plot
m1_plot <- modelplot(m1, coef_map = m1_labels, background = b) +
  labs(x = 'Coefficients') +
  theme_cowplot() 

#Save
ggsave("Output/Modelled dispersal coefficient plot (mean dis).png", 
       plot = m1_plot, width = 7, height = 3.5) 


###############################
##### APPENDICES ######
###############################

pdf("output/Appendix/Modelled dispersal model predictions1.pdf", width = 5, height = 3.5)
preds <- as.data.frame(fitted(m1))
preds$fitted_m1 <- preds$`fitted(m1)`
plot(preds$fitted_m1 ~ m1$data$mean_pred_distance,  
     xlab = "Modelled diaspore dispersal observations",
     ylab = "Model predictions")
abline(0, 1, col= 'red')
dev.off() 



#########
##### END #####
##########
