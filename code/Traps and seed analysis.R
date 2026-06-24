##########
### ALPINE SEED DISPERSAL PROJECT ########
##########

library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(performance)
library(DHARMa)
library(modelsummary)
library(gridExtra)
library(glmmTMB)
library(ggeffects)
library(MuMIn)
library(stringr)

### Step 1) Load and explore data
traps <- read.csv("data/Traps and species caught2.csv")

glimpse(traps)

traps <- traps %>%
  mutate(
    Trap_position = factor(case_when(
      Trap_position == "t" ~ "Top",
      Trap_position == "b" ~ "Bottom")),
    Species = factor(Species),
    Site    = factor(Trap_Number),
    Year    = factor(Year))

sum(traps$Collection_Sample)

traps %>%
  group_by(Year) %>%
  summarise(Total_diaspores = sum(Collection_Sample))

cat("Number of species:", length(unique(traps$Species)), "\n")
cat("Number of sites:", length(unique(traps$Site)), "\n")
cat("Years:", paste(unique(traps$Year), collapse = ", "), "\n")
cat("Total observations:", nrow(traps), "\n")

### Step 2) Join species details
Species_details <- read.csv("data/Species details.csv")

traps <- traps %>% mutate(Species = trimws(as.character(Species)))
Species_details <- Species_details %>% mutate(Species = trimws(Species))

anti_join(traps, Species_details, by = "Species") %>% distinct(Species)

traps <- traps %>% left_join(Species_details, by = "Species")

### Step 3) Data visualization
hist(traps$Collection_Sample, main = "Distribution of diaspore collections")

ggplot(traps, aes(x = Collection_Sample)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~Trap_position) +
  labs(title = "Diaspore collections by trap position")

### Step 4) Aggregate data and fit model
# Aggregate to total diaspores per trap position per site per year
traps_agg <- traps %>%
  group_by(Trap_position, Site, Year) %>%
  summarise(Total = sum(Collection_Sample), .groups = "drop")

cat("Aggregated observations per trap position:\n")
print(table(traps_agg$Trap_position))

# Fit model on aggregated data
m1 <- glmmTMB(Total ~ Trap_position + (1|Site),
              family = nbinom2(),
              data = traps_agg)

### Step 5) Model diagnostics
overdisp_test <- sum(residuals(m1, type = "pearson")^2) / df.residual(m1)
cat("Overdispersion test statistic:", round(overdisp_test, 3), "\n")

res <- simulateResiduals(m1)
plot(res)

r2_values <- r2(m1)
print(r2_values)

summary(m1)

### Step 6) Extract fixed effects
fixed_effects <- summary(m1)$coefficients$cond
trap_effect    <- fixed_effects[2, 1]
trap_effect_se <- fixed_effects[2, 2]
trap_effect_z  <- fixed_effects[2, 3]
trap_effect_p  <- fixed_effects[2, 4]

ci <- confint(m1, parm = "beta_", method = "profile")
intercept_conf.low  <- ci[1, 1]
intercept_conf.high <- ci[1, 2]
trap_conf.low       <- ci[2, 1]
trap_conf.high      <- ci[2, 2]

### Step 7) Summary statistics for plotting
plot_summary <- ggpredict(m1, terms = "Trap_position") %>%
  as.data.frame() %>%
  rename(Trap_position = x,
         mean          = predicted)

print(plot_summary)

### Step 8) Plots

# Coefficient plot
coef_data <- data.frame(
  term      = c("Intercept", "Top vs bottom trap"),
  estimate  = c(fixed_effects[1, 1], trap_effect),
  conf.low  = c(intercept_conf.low, trap_conf.low),
  conf.high = c(intercept_conf.high, trap_conf.high)) %>%
  mutate(term = factor(term, levels = c("Top vs bottom trap", "Intercept")))

coef_plot <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', linetype = "dashed") +
  labs(x = 'Log rate ratio', y = '', tag = "a)") +
  theme_cowplot()

# Data plot using model predictions
data_plot <- ggplot(plot_summary,
                    aes(x = Trap_position, y = mean, fill = Trap_position)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Trap position",
       y = "Predicted total diaspores per trap",
       fill = "Trap position",
       tag = "b)") +
  scale_fill_manual(values = c("Bottom" = "#56B4E9", "Top" = "#E69F00")) +
  theme_cowplot() +
  theme(legend.position = "none")

### Step 9) Random effects plot
re <- ranef(m1, condVar = TRUE)

site_condvar <- attr(re$cond$Site, "condVar")
re_data <- data.frame(
  level    = rownames(re$cond$Site),
  estimate = re$cond$Site[, 1],
  se       = sqrt(site_condvar[1, 1, ]),
  group    = "Site"
) %>%
  mutate(conf.low  = estimate - 1.96 * se,
         conf.high = estimate + 1.96 * se)

re_plot <- ggplot(re_data, aes(x = estimate, y = level)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', linetype = "dashed") +
  facet_wrap(~group, scales = "free_y") +
  labs(x = "Random effect estimate", y = "", tag = "c)") +
  theme_cowplot()

ggsave("output/Appendix/Trap and seed model random_effects_plot.png", 
       plot = re_plot, width = 6, height = 4, bg = "white")

### Step 10) Results summary
cat("\n=== GLMM TRAP POSITION RESULTS ===\n")
cat("Sites:", length(unique(traps_agg$Site)), "\n")
cat("Years:", length(unique(traps_agg$Year)), "\n")
cat("Species:", length(unique(traps$Species)), "\n")
cat("Total diaspores:", sum(traps$Collection_Sample), "\n")
cat("Aggregated records - Bottom:", sum(traps_agg$Trap_position == "Bottom"),
    "Top:", sum(traps_agg$Trap_position == "Top"), "\n")
cat("Predicted mean bottom trap:", round(plot_summary$mean[plot_summary$Trap_position == "Bottom"], 2), "\n")
cat("Predicted mean top trap:", round(plot_summary$mean[plot_summary$Trap_position == "Top"], 2), "\n")
cat("Coefficient (log scale):", round(trap_effect, 3), "\n")
cat("Z value:", round(trap_effect_z, 3), "\n")
cat("P-value:", ifelse(trap_effect_p < 0.001, "< 0.001", round(trap_effect_p, 4)), "\n")
cat("R² (marginal):", round(r2_values$R2_marginal, 3), "\n")
cat("R² (conditional):", round(r2_values$R2_conditional, 3), "\n")

### Step 11) Observed vs fitted - uses aggregated data
pred_df <- data.frame(
  observed = traps_agg$Total,
  fitted   = fitted(m1))

ggplot(pred_df, aes(x = observed, y = fitted)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Observed total diaspores per trap",
       y = "Model predictions") +
  theme_cowplot() +
  panel_border(colour = "black")

ggsave("output/Appendix/Trap_model_predictions.png", 
       width = 5, height = 3, bg = "white", dpi = 300)

### Step 12) DS summary plot - uses original traps data
ds_summary <- traps %>%
  filter(!is.na(DS)) %>%
  group_by(DS, Trap_position) %>%
  summarise(Total_caught = sum(Collection_Sample, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trap_position, values_from = Total_caught, values_fill = 0) %>%
  mutate(
    Total_all   = Bottom + Top,
    Prop_bottom = Bottom / Total_all,
    Prop_top    = Top / Total_all
  ) %>%
  arrange(desc(Total_all))

print(ds_summary)

ds_plot <- ds_summary %>%
  dplyr::select(DS, Bottom, Top) %>%
  pivot_longer(cols = c(Bottom, Top), names_to = "Trap_type", values_to = "Count") %>%
  ggplot(aes(x = reorder(DS, Count), y = Count, fill = Trap_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(x = "Dispersal syndrome", y = "Total number of diaspores",
       fill = "Trap position", tag = "c)") +
  scale_fill_manual(values = c("Bottom" = "#56B4E9", "Top" = "#E69F00")) +
  theme_cowplot() +
  theme(axis.text.y = element_text(size = 9),
        plot.margin = margin(5, 5, 5, 10, "mm"))

### Step 13) Combined figure
extended_plots <- grid.arrange(
  coef_plot, data_plot,
  ds_plot,
  nrow = 2,
  layout_matrix = rbind(c(1, 2), c(3, 3)))

ggsave("output/Traps and seed output/Trap_analysis_with_ds.png", 
       plot = extended_plots, width = 12, height = 10, bg = "white", dpi = 300)

### Step 14) Species and growth form summaries - use original traps data
gf_summary <- traps %>%
  filter(!is.na(GF)) %>%
  group_by(GF, Trap_position) %>%
  summarise(Total_caught = sum(Collection_Sample, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trap_position, values_from = Total_caught, values_fill = 0) %>%
  mutate(
    Total_all   = Bottom + Top,
    Prop_bottom = Bottom / Total_all,
    Prop_top    = Top / Total_all
  ) %>%
  arrange(desc(Total_all))

print(gf_summary)

species_summary <- traps %>%
  group_by(Species, Trap_position) %>%
  summarise(Total_caught = sum(Collection_Sample, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trap_position, values_from = Total_caught, values_fill = 0) %>%
  mutate(
    Total_all   = Bottom + Top,
    Prop_bottom = Bottom / Total_all,
    Prop_top    = Top / Total_all
  ) %>%
  arrange(desc(Total_all))

print(species_summary)

species_plot <- species_summary %>%
  dplyr::select(Species, Bottom, Top) %>%
  pivot_longer(cols = c(Bottom, Top), names_to = "Trap_type", values_to = "Count") %>%
  filter(!is.na(Species)) %>%
  ggplot(aes(x = reorder(Species, Count), y = Count, fill = Trap_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(x = "Species", y = "Total number of diaspores", fill = "Trap position") +
  scale_fill_manual(values = c("Bottom" = "#56B4E9", "Top" = "#E69F00")) +
  theme_cowplot() +
  theme(axis.text.y = element_text(size = 9, face = "italic"),
        plot.margin = margin(5, 5, 5, 10, "mm"))

ggsave("output/Traps and seed output/Common_species_trap_summary.png", 
       plot = species_plot, width = 10, height = 8, bg = "white")


##################################
##### OTHER EXPLORATORY ANALYSIS ##########
##################################


##### GROWTH FORM ######
# Create growth form summary
gf_summary <- traps %>%
  filter(!is.na(GF)) %>%
  group_by(GF, Trap_position) %>%
  summarise(Total_caught = sum(Collection_Sample, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trap_position, values_from = Total_caught, values_fill = 0) %>%
  mutate(
    Total_all   = Bottom + Top,
    Prop_bottom = Bottom / Total_all,
    Prop_top    = Top / Total_all
  ) %>%
  arrange(desc(Total_all))

print(gf_summary)

# Create growth form plot
gf_plot <- gf_summary %>%
  dplyr::select(GF, Bottom, Top) %>%
  pivot_longer(cols = c(Bottom, Top),
               names_to = "Trap_type", values_to = "Count") %>%
  filter(!is.na(GF)) %>%
  ggplot(aes(x = reorder(GF, Count), y = Count, fill = Trap_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(x = "Growth form",
       y = "Total number of diaspores",
       fill = "Trap position") +
  scale_fill_manual(values = c("Bottom" = "#56B4E9", "Top" = "#E69F00")) +
  theme_cowplot() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 10, "mm"))

print(gf_plot)

ggsave("output/Traps and seed output/GF_trap_summary.png",
       plot = gf_plot, width = 8, height = 4, bg = "white", dpi = 300)


##### SPECIES #####
# Create species summary
species_summary <- traps %>%
  group_by(Species, Trap_position) %>%
  summarise(Total_caught = sum(Collection_Sample, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trap_position, values_from = Total_caught, values_fill = 0) %>%
  mutate(
    Total_all   = Bottom + Top,
    Prop_bottom = Bottom / Total_all,
    Prop_top    = Top / Total_all
  ) %>%
  arrange(desc(Total_all))

print(species_summary)

# Create species plot with ALL species
species_plot <- species_summary %>%
  dplyr::select(Species, Bottom, Top) %>%
  pivot_longer(cols = c(Bottom, Top),
               names_to = "Trap_type", values_to = "Count") %>%
  filter(!is.na(Species)) %>%
  ggplot(aes(x = reorder(Species, Count), y = Count, fill = Trap_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(x = "Species", y = "Total number of diaspores", fill = "Trap position") +
  scale_fill_manual(values = c("Bottom" = "#56B4E9", "Top" = "#E69F00")) +
  theme_cowplot() +
  theme(
    axis.text.y = element_text(size = 9, face = "italic"),
    plot.margin = margin(5, 5, 5, 10, "mm"))

print(species_plot)

ggsave("output/Traps and seed output/Common_species_trap_summary.png", 
       plot = species_plot, width = 10, height = 8,  # increased height for all species
       bg = "white")



###############################
##### END ######
###############################

#What version of R are you using?
sessionInfo() 
