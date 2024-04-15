##########
### ALPINE SEED DISPERSAL PROJECT ########
##########
#####
## Q: Relationship between diaspore terminal velocity & mass
######

# Required packages
library(tidyverse)
library(DHARMa)
library(Matrix)
library(lme4)
library(emmeans)
library(performance)
library(report)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(modelsummary)

### Step 1) Load data
seed_data <- read.csv("data/Seed mass data.csv")

### Step 2) Data exploration
# Check data structure
str(seed_data)

#change seed_data `Seed mass (mg)` column name
colnames(seed_data)[colnames(seed_data) == 'Seed.mass..mg.'] <- 'Seed_mass_mg'
colnames(seed_data)[colnames(seed_data) == 'Tvelocity.m.s'] <- 'mean_terminal_velocity'

# Check data structure
ggplot(seed_data, aes(mean_terminal_velocity)) + geom_density()

#Correlation between seed mass and terminal velocity
ggplot(seed_data, aes(x = Seed_mass_mg, y = mean_terminal_velocity)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_cowplot() +
  ylab("Mean terminal velocity (s/m)") +
  xlab("Mean seed mass (mg)")

cor.test(seed_data$Seed_mass_mg, seed_data$mean_terminal_velocity) # r = -0.4034895 , p = 0.06258


### Step 3) Data analysis

#Fit the models
m1 = glm(mean_terminal_velocity ~ Seed_mass_mg,
        family = 'gaussian', seed_data)
m2 = glm(mean_terminal_velocity ~ Seed_mass_mg + Family,
         family = 'gaussian', seed_data)

### Step 4) Compare models performance
compare_performance(m1, m2) #m2 is the best fit but it violates linear model assumtptions

### Step 5) Model validation
# Check for Normality and homogeneity of variances
res = simulateResiduals(m1)
op = par(mfrow = c(1, 2), pty = 's')
plotQQunif(res)
plotResiduals(res, seed_data$Seed_mass_mg)
par(op) 

# Check predictions
check_predictions(m1) # Looking good 
#Check R2
r.squaredGLMM(m1) #0.163

### Step 6) Model interpretation
# Model summary
summary(m1)
#p = 0.0626 
#Estimate -0.5235

#Calculate the estimated marginal means for the range of seed mass values to make predictions
emm <- emmeans(m1, "Seed_mass_mg", 
               at = list(Seed_mass_mg = c(0, 0.25, 0.5, 0.75, 1,
                                          1.25, 1.5, 1.75, 2,
                                          2.25, 2.5, 2.75, 3,
                                          3.25, 3.5, 3.75, 
                                          4, 4.25, 4.5)))
#convert to dataframe
emm_df <- as.data.frame(emm)

#In emm_df, change lower.CL values that are less than 0.2206276 to 0 
#to make an appropriate graph
emm_df$lower.CL[emm_df$lower.CL < 0.2206276] <- 0

### Step 7) Model visualisation
# Plot the relationship between seed mass and terminal velocity
TV_mass_plot <- ggplot(emm_df, aes(x = Seed_mass_mg, y = emmean)) + 
  geom_path() +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) +
  geom_point(data = seed_data, aes(x = Seed_mass_mg, y = mean_terminal_velocity), 
             shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  theme_cowplot()+
  ylim(0,NA) +
  ylab(expression("Average terminal velocity (m/s)"))+
  xlab(expression("Average diaspore mass (mg)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 
#View plot
TV_mass_plot

#Make coefficient plot
mod_labels <- c(
               'Seed_mass_mg' = 'Diaspore mass (mg)',
               '(Intercept)' = 'Intercept')
b <- list(geom_vline(xintercept = 0, color = 'red'))
#plot
m1_plot <- modelplot(m1, coef_map = mod_labels, background = b) +
  labs(x = 'Coefficients') +
  theme_cowplot() 

#Join plots together
plots <- grid.arrange(m1_plot, TV_mass_plot,
                            ncol = 2)

### Step 8) Save plot
ggsave("output/Terminal velocity vs mass.png", 
       plot = plots, width = 11, height = 3.5) 

###############################
##### APPENDICES ######
###############################

pdf("output/Appendix/Terminal velocity model predictions.pdf", width = 5, height = 3.5)
preds <- as.data.frame(fitted(m1))
preds$fitted_m1 <- preds$`fitted(m1)`
plot(preds$fitted_m1 ~ m1$data$mean_terminal_velocity,  
     xlab = "Terminal velocity observations",
     ylab = "Model predictions")
abline(0, 1, col= 'red')
dev.off() 

#What version of R are you using?
sessionInfo() 

