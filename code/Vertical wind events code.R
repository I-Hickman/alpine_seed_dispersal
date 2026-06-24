##########
### ALPINE SEED DISPERSAL PROJECT ########
##########
#####
## Q: Proortion of vertical wind events  above the terminal velocity of seeds
######

##########
### WIND UPDRAFT ANALYSIS ########
##########

library(tidyverse)
library(cowplot)
library(mgcv)
library(DHARMa)
library(ggeffects)
library(gridExtra)

### Step 1) Load and prepare data
wind_data2   <- read.csv("data/Vertical wind data (cleaned).csv")
species_deets <- read.csv("data/Species details.csv")

wind_data3 <- merge(wind_data2, species_deets, by = "Species")
wind_data3$perc_threshold_events <- as.numeric(wind_data3$perc_threshold_events)
wind_data3$Tvel <- as.numeric(wind_data3$Tvel)

glimpse(wind_data3)

### Step 2) Data visualization
ggplot(wind_data3, aes(x = Tvel, y = perc_threshold_events)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Terminal velocity (m/s)",
       y = "Proportion of updraft events") +
  theme_cowplot()

### Step 3) Fit and compare models
# Simple binomial GLM
m_wind_final <- glm(cbind(Events.above.threshold, Total.events - Events.above.threshold) ~
                      Tvel,
                    family = binomial(),
                    data = wind_data3)

# GAM with spline
m_wind <- gam(cbind(Events.above.threshold, Total.events - Events.above.threshold) ~
                s(Tvel, k = 5),
              family = binomial(),
              data = wind_data3)

# Polynomial GLM
m_wind_poly <- glm(cbind(Events.above.threshold, Total.events - Events.above.threshold) ~
                     poly(Tvel, 2),
                   family = binomial(),
                   data = wind_data3)

# Model comparison
AIC(m_wind_final, m_wind, m_wind_poly)

### Step 4) Diagnostics on final model
summary(m_wind_final)

overdisp_test <- sum(residuals(m_wind_final, type = "pearson")^2) / df.residual(m_wind_final)
cat("Overdispersion test statistic:", round(overdisp_test, 3), "\n")

res <- simulateResiduals(m_wind_final)
plot(res)

### Step 5) Extract fixed effects and confidence intervals
fixed_effects <- summary(m_wind_final)$coefficients
wind_effect    <- fixed_effects[2, 1]
wind_effect_se <- fixed_effects[2, 2]
wind_effect_z  <- fixed_effects[2, 3]
wind_effect_p  <- fixed_effects[2, 4]

ci <- confint(m_wind_final)
intercept_conf.low  <- ci[1, 1]
intercept_conf.high <- ci[1, 2]
wind_conf.low       <- ci[2, 1]
wind_conf.high      <- ci[2, 2]

# Odds ratios
cat("Odds ratio for Tvel:", round(exp(wind_effect), 3), "\n")
cat("Z value:", round(wind_effect_z, 3), "\n")
cat("P-value:", ifelse(wind_effect_p < 0.001, "< 0.001", round(wind_effect_p, 4)), "\n")

### Step 6) Coefficient plot
coef_data <- data.frame(
  term      = c("Intercept", "Terminal velocity"),
  estimate  = c(fixed_effects[1, 1], wind_effect),
  conf.low  = c(intercept_conf.low, wind_conf.low),
  conf.high = c(intercept_conf.high, wind_conf.high)) %>%
  mutate(term = factor(term, levels = c("Terminal velocity", "Intercept")))

coef_plot <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', linetype = "dashed") +
  labs(x = "Coefficient (log odds)", y = "", tag = "a)") +
  theme_cowplot()

### Step 7) Predicted fit plot
plot_data <- data.frame(Tvel = seq(min(wind_data3$Tvel),
                                   max(wind_data3$Tvel),
                                   length.out = 200))

preds <- predict(m_wind_final, newdata = plot_data,
                 type = "response", se.fit = TRUE)
plot_data$fit   <- preds$fit
plot_data$upper <- preds$fit + 1.96 * preds$se.fit
plot_data$lower <- preds$fit - 1.96 * preds$se.fit

pred_plot <- ggplot() +
  geom_ribbon(data = plot_data,
              aes(x = Tvel, ymin = lower, ymax = upper),
              alpha = 0.2) +
  geom_line(data = plot_data,
            aes(x = Tvel, y = fit), linewidth = 1) +
  geom_point(data = wind_data3,
             aes(x = Tvel, y = perc_threshold_events),
             size = 3) +
  labs(x = "Terminal velocity (m/s)",
       y = "Proportion of updraft events",
       tag = "b)") +
  theme_cowplot()

### Step 8) Combine and save
combined_plot <- grid.arrange(coef_plot, pred_plot, ncol = 2)

ggsave("output/Vertical wind events model/Wind_GLM_combined.png",
       plot = combined_plot,
       width = 10, height = 5,
       bg = "white", dpi = 300)

### Step 9) Results summary
cat("\n=== WIND UPDRAFT RESULTS ===\n")
cat("n species:", nrow(wind_data3), "\n")
cat("Coefficient (Tvel):", round(wind_effect, 3), "\n")
cat("Odds ratio:", round(exp(wind_effect), 3), "\n")
cat("95% CI:", round(exp(wind_conf.low), 3), "-", round(exp(wind_conf.high), 3), "\n")
cat("Z value:", round(wind_effect_z, 3), "\n")
cat("P-value:", ifelse(wind_effect_p < 0.001, "< 0.001", round(wind_effect_p, 4)), "\n")
cat("Null deviance:", round(m_wind_final$null.deviance, 2), "\n")
cat("Residual deviance:", round(m_wind_final$deviance, 2), "\n")
cat("McFadden R²:", round(1 - m_wind_final$deviance / m_wind_final$null.deviance, 3), "\n")


###############################
##### APPENDICES ######
###############################

pred_df_wind <- data.frame(
  observed = wind_data3$perc_threshold_events,
  fitted   = fitted(m_wind_final))

ggplot(pred_df_wind, aes(x = observed, y = fitted)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Observed proportion of updraft events",
       y = "Model predictions") +
  theme_cowplot() +
  panel_border(colour = "black")

ggsave("output/Appendix/Wind_model_predictions.png", 
       width = 5, height = 3,  
       bg = "white",
       dpi = 300)




########################
############################# OLD ######
########################

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
ggsave("output/Vertical wind events model/Vertical wind correlation plot (loess).png", 
       plot = cor_plot3, width = 8, height = 3.5, bg = "white", dpi = 300) 


###############################
#### END
###############################

#R and package versions
sessionInfo()
