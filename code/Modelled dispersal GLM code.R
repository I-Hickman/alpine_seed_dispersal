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
Dispersal_data <- read.csv("data/Species dispersal data.csv")
Distance <- read.csv("data/Minimum distance dispersed.csv")
mean_distance <- read.csv("data/Average distance dispersed.csv")

#Join distance and mean distance data
Distance <- merge(Distance, mean_distance, by = "Species")

### Step 2) Data exploration
# Check data structure
str(Dispersal_data)

#Change column name Mean.predicted.dispersal..m. to mean_pred_distance
colnames(Dispersal_data)[colnames(Dispersal_data) == 'Mean.predicted.dispersal..m.'] <- 'mean_pred_distance'
colnames(Dispersal_data)[colnames(Dispersal_data) == 'Release.height..m.'] <- 'release_height'

#Merge the two datasets
Dispersal_data_2 <- merge(Dispersal_data, Distance, by = "Species")

#Correlation between seed mass and terminal velocity
ggplot(Dispersal_data_2, aes(x = release_height, y = mean_pred_distance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  ylab("Predicted dispersal") +
  xlab("Release height")

ggplot(Dispersal_data_2, aes(x = mean_distance, y = mean_pred_distance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  ylab("Predicted dispersal") +
  xlab("Release height")

cor.test(Dispersal_data_2$mean_pred_distance, Dispersal_data_2$mean_distance) # r = 0.4956246  , p = 0.005352
cor.test(Dispersal_data_2$mean_pred_distance, Dispersal_data_2$release_height) # r = 0.4316504  , p = 0.02181

### Step 3) Data analysis

#Standardise variables
Dispersal_data_2$mean_dist.std <- scale(Dispersal_data_2$mean_distance)
Dispersal_data_2$release_ht.std <- scale(Dispersal_data_2$release_height)

#Remove outlier from dataset- species Acaena novae-zelandiae from the data
Dispersal_data_2 <- Dispersal_data_2[!Dispersal_data_2$Species == "Acaena novae-zelandiae",]

#Fit the models
m1 = glmmTMB(mean_pred_distance ~ mean_dist.std + release_ht.std,
             family = gaussian(),
             data = Dispersal_data_2)

m2 = glmmTMB(mean_pred_distance ~ mean_dist.std + release_ht.std + (1|Species),
             family = gaussian(),
             data = Dispersal_data_2)

m3 = glmmTMB(mean_pred_distance ~ mean_dist.std + release_ht.std + (1|Dispersal.syndrome),
             family = gaussian(),
             data = Dispersal_data_2)

#Compare models
AIC(m1, m2, m3)
#model 1 is best, so we will use this for the rest of the analysis

### Step 5) Model validation
# Normality and homogeneity of variances
res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, Dispersal_data_2$mean_dist.std)
par(op) # Normality and homogeneity look good for m1 

res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, Dispersal_data_2$release_ht.std)
par(op) # Normality and homogeneity look good for m1 

# Check predictions
check_predictions(m1)
dev.off()


### Step 6) Model interpretation
# Model summary
summary(m1)
#none are significant

#### Step 7) Make a coefficient plot
# Extract fixed effects
fixed_effects <- summary(m1)$coefficients$cond  # $cond needed for glmmTMB
ci <- confint(m1, parm = "beta_", method = "Wald")  # Wald to avoid freezing

# Check row order
fixed_effects
ci

# Build coefficient data frame
coef_data <- data.frame(
  term      = c("Intercept", "Mean distance (m)", "Release height (m)"),
  estimate  = c(fixed_effects[1, 1], fixed_effects[2, 1], fixed_effects[3, 1]),
  conf.low  = c(ci[1, 1], ci[2, 1], ci[3, 1]),
  conf.high = c(ci[1, 2], ci[2, 2], ci[3, 2])
) %>%
  mutate(term = factor(term, levels = c("Release height (m)",
                                        "Mean distance (m)",
                                        "Intercept")))

# Coefficient plot
m1_plot <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', linetype = "dashed") +
  labs(x = "Coefficients", y = "") +
  theme_cowplot()

ggsave("Output/Dispersal model/GLM Dispersal coefficient plot (mean dis).png", 
       plot = m1_plot, width = 7, height = 3.5, bg = "white")


###############################
##### APPENDICES ######
###############################

pred_df2 <- data.frame(
  observed = m1$data$mean_pred_distance,
  fitted   = fitted(m1))

ggplot(pred_df2, aes(x = observed, y = fitted)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Diaspore dispersal observations",
       y = "Model predictions") +
  theme_cowplot() +
  panel_border(colour = "black")

ggsave("output/Appendix/Dispersal_model_predictions.png", 
       width = 5, height = 3,  
       bg = "white",
       dpi = 300)


#########
##### END #####
##########

#R and package versions
sessionInfo()
