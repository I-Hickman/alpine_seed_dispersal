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
library(tidyverse)
library(cowplot)
library(effsize)


### Read data
wind <- read.csv("data/Wind speed.csv")

### Quick normality check
hist(wind$Horizontal, main = "Horizontal Wind")
hist(wind$Vertical, main = "Vertical Wind")

### Transform data
wind$Horizontal.sqrt <- sqrt(wind$Horizontal)
wind$Vertical.sqrt <- sqrt(wind$Vertical)

### Paired t-test
t_result <- t.test(wind$Horizontal.sqrt, wind$Vertical.sqrt, paired = TRUE)
print(t_result)

### Summary statistics
wind_summary <- wind %>%
  dplyr::select(Height.above.Ground..cm., Vertical, Horizontal) %>%
  pivot_longer(cols = c(Vertical, Horizontal), 
               names_to = "Wind_direction", 
               values_to = "Speed") %>%
  group_by(Wind_direction, Height.above.Ground..cm.) %>%
  summarise(mean = mean(Speed, na.rm = TRUE),
            se = sd(Speed, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

### Handle missing values for plotting
wind_summary_clean <- wind_summary %>% 
  filter(!is.na(mean))

### Plot
wind_plot <- ggplot(wind_summary_clean, aes(x = Height.above.Ground..cm., y = mean, 
                                            colour = Wind_direction)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 5) +
  labs(x = "Height above ground (cm)", y = "Wind speed (m/s)") +
  theme_cowplot()

print(wind_plot)

### Quick summary
cat("Horizontal mean:", round(mean(wind$Horizontal, na.rm = TRUE), 3), "m/s\n")
cat("Vertical mean:", round(mean(wind$Vertical, na.rm = TRUE), 3), "m/s\n")
cat("Mean difference:", round(t_result$estimate, 3), "m/s (sqrt scale)\n")
cat("p-value: < 0.001\n")
cat("Sample size:", t_result$parameter + 1, "pairs\n")
cat("95% CI for difference:", round(t_result$conf.int[1], 3), "to", round(t_result$conf.int[2], 3), "\n")

#Save
ggsave("output/Wind speed/Traps winds plots.png", 
       plot = wind_plot, width = 8, height = 3.5, bg = "white") 


### END ####