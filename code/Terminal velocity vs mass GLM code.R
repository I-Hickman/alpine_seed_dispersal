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

# Clean column names
colnames(seed_data)[colnames(seed_data) == 'Seed.mass..mg.'] <- 'Seed_mass_mg'
colnames(seed_data)[colnames(seed_data) == 'Tvelocity.m.s'] <- 'mean_terminal_velocity'

# Data exploration
ggplot(seed_data, aes(mean_terminal_velocity)) + geom_density()

# Correlation test
cor_result <- cor.test(seed_data$Seed_mass_mg, seed_data$mean_terminal_velocity)
print(cor_result)

### Step 3) Data analysis
# Fit models (fixed syntax)
m1 <- glm(mean_terminal_velocity ~ Seed_mass_mg,
          family = gaussian(), data = seed_data)
m2 <- glm(mean_terminal_velocity ~ Seed_mass_mg + Family,
          family = gaussian(), data = seed_data)

### Step 4) Compare model performance
compare_performance(m1, m2)

### Step 5) Model validation
# Check residuals
res <- simulateResiduals(m1)
plot(res)

# Check predictions
check_predictions(m1)

# Check R-squared
r2_result <- r2(m1)
print(paste("R² =", round(r2_result$R2, 3)))

### Step 6) Model interpretation
summary(m1)

# Extract key statistics
coef_estimate <- round(coef(m1)[2], 3)
p_value <- round(summary(m1)$coefficients[2,4], 4)
r_squared <- round(r2_result$R2, 3)

cat("Seed mass coefficient:", coef_estimate, "\n")
cat("P-value:", p_value, "\n") 
cat("R-squared:", r_squared, "\n")

### Step 7) Model visualisation (formatted like wind graph)
# Create prediction data
pred_data <- data.frame(
  Seed_mass_mg = seq(min(seed_data$Seed_mass_mg), 
                     max(seed_data$Seed_mass_mg), 
                     length.out = 100)
)

# Get predictions with confidence intervals
predictions <- predict(m1, newdata = pred_data, se.fit = TRUE)
pred_data$fitted <- predictions$fit
pred_data$se <- predictions$se.fit
pred_data$lower <- pred_data$fitted - 1.96 * pred_data$se
pred_data$upper <- pred_data$fitted + 1.96 * pred_data$se

# Create plot 
TV_mass_plot <- ggplot() +
  geom_point(data = seed_data, 
             aes(x = Seed_mass_mg, y = mean_terminal_velocity), 
             alpha = 0.6, colour = "grey70") +
  geom_ribbon(data = pred_data,
              aes(x = Seed_mass_mg, ymin = lower, ymax = upper),
              alpha = 0.3, fill = "black") +
  geom_line(data = pred_data,
            aes(x = Seed_mass_mg, y = fitted),
            colour = "black", size = 1) +
  labs(x = "Average diaspore mass (mg)",
       y = "Average terminal velocity (m/s)",
       title = "b)") +
  theme_cowplot()

# Coefficient plot
# Extract fixed effects
fixed_effects <- summary(m1)$coefficients
ci <- confint.default(m1)  # Wald CIs to avoid freezing

# Build coefficient data frame
coef_data <- data.frame(
  term      = c("Intercept", "Diaspore mass (mg)"),
  estimate  = c(fixed_effects[1, 1], fixed_effects[2, 1]),
  conf.low  = c(ci[1, 1], ci[2, 1]),
  conf.high = c(ci[1, 2], ci[2, 2])
) %>%
  mutate(term = factor(term, levels = c("Diaspore mass (mg)", "Intercept")))

# Coefficient plot
m1_plot <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', linetype = "dashed") +
  labs(x = "Coefficients", y = "", tag = "a)") +
  theme_cowplot()

# Combine plots
plots <- grid.arrange(m1_plot, TV_mass_plot, ncol = 2)

### Step 8) Save plot
ggsave("output/Terminal vs mass model/Terminal velocity vs mass.png", 
       plot = plots, width = 11, height = 3.5, bg = "white")


###############################
##### APPENDICES ######
###############################

pred_df <- data.frame(
  observed = m1$data$mean_terminal_velocity,
  fitted   = fitted(m1))

ggplot(pred_df, aes(x = observed, y = fitted)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Terminal velocity observations",
       y = "Model predictions") +
  theme_cowplot() +
  panel_border(colour = "black")

ggsave("output/Appendix/Terminal_velocity_model_predictions.png", 
       width = 5, height = 3,  
       bg = "white",
       dpi = 300)


###############################
##### END ######
###############################

#What version of R are you using?
sessionInfo() 

