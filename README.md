# Field observations confirm that Australian alpine grassland species have limited long-distance seed dispersal capacity

Code and data accompanying the manuscript:

**Field observations confirm that Australian alpine grassland species have limited long-distance seed dispersal capacity**

## Summary

Many estimates of seed dispersal rely on simple plant traits, such as dispersal mode. Including hard traits, such as terminal velocity, allows the role of updraft in dispersal to be interpreted. This study improves existing models of alpine grassland species dispersal in Australia and quantifies dispersal distances in situ.

Published models of dispersal distance in alpine grassland species are extended by incorporating terminal velocity. Seed traps were deployed in the field over two dispersal seasons to quantify diaspore movement. Traps captured near-surface dispersal (45 cm above ground) and wind-column dispersal (150 cm above ground). Vertical wind speed was measured at trap sites, and the probability of dispersal by updraft was estimated.

Modelled diaspore dispersal indicated short-distance dispersal (< 40 m) for many alpine species, a result confirmed by field measurements. Trapping detected 31 to 33% of species flowering within 15 m of traps. Most diaspores (64 to 77% across years) were captured at 45 cm above ground, while many species (64% across years) were captured at 1.5 m, including species with low terminal velocity (< 1 m/s) that experience the greatest proportion of wind events exceeding their terminal velocity thresholds. Dispersal was constrained by low release heights and high terminal velocity, suggesting limited capacity to track rapid climate change over short timescales. Updraft events nonetheless permitted longer-distance dispersal for several low terminal velocity species.

## Repository structure

```
alpine_seed_dispersal/
├── alpine_seed_dispersal.Rproj
├── code/
│   ├── Terminal velocity vs mass GLM code.R
│   ├── Modelled dispersal GLM code.R
│   ├── Traps and seed analysis.R
│   ├── Vertical wind events code.R
│   ├── Wind speed code.R
│   └── Traps code (possible not used).R
├── data/
│   ├── Traps and species caught2.csv
│   ├── Traps and species caught.csv
│   ├── Species details.csv
│   ├── Seed mass data.csv
│   ├── Vertical wind data (cleaned).csv
│   ├── Wind speed.csv
│   ├── Minimum distance dispersed.csv
│   ├── Average distance dispersed.csv
│   ├── Species dispersal data.csv
│   └── Floristics_PA_long.csv
├── output/
│   ├── Terminal vs mass model/
│   ├── Dispersal model/
│   ├── Traps and seed output/
│   ├── Vertical wind events model/
│   ├── Wind speed/
│   └── Appendix/
├── LICENSE
└── README.md
```

The project is organised as an RStudio project. Opening `alpine_seed_dispersal.Rproj` sets the working directory to the repository root, so all `read.csv("data/...")` and `ggsave("output/...")` paths in the scripts resolve without modification.

## Data

All data files are in `data/`.

| File | Description |
|------|-------------|
| `Traps and species caught2.csv` | Diaspore counts per trap, with trap position (top/bottom), trap number, year and species. |
| `Species details.csv` | Species-level traits, including terminal velocity (`Tvel`), dispersal syndrome (`DS`), growth form (`GF`) and family. |
| `Seed mass data.csv` | Diaspore mass (mg) and mean terminal velocity (m/s) per species. |
| `Vertical wind data (cleaned).csv` | Per-species counts of vertical wind events, events exceeding the terminal velocity threshold, and the proportion above threshold. |
| `Wind speed.csv` | Horizontal and vertical wind speed measurements by height above ground. |
| `Minimum distance dispersed.csv` | Minimum observed dispersal distance per species. |
| `Average distance dispersed.csv` | Mean observed dispersal distance per species. |
| `Species dispersal data.csv` | Dispersal distances and release heights per species. |
| `Floristics_PA_long.csv` | Presence/absence floristics records in long format. |

A note on the trap data: duplicated records in the 2018 trap data had inflated diaspore counts. `Traps and species caught2.csv` is the corrected version and is the file loaded by the analysis scripts. `Traps and species caught.csv` is kept only for provenance.

## Code

Scripts are in `code/` and can be run independently. Each reads from `data/` and writes figures to the matching subfolder in `output/`.

**`Terminal velocity vs mass GLM code.R`**
Tests the relationship between diaspore terminal velocity and seed mass. Fits a Gaussian GLM (terminal velocity as a function of seed mass), with a family covariate compared as an alternative. Outputs the coefficient and fitted-line figure to `output/Terminal vs mass model/` and a prediction-check figure to `output/Appendix/`.

**`Modelled dispersal GLM code.R`**
Relates modelled dispersal distance to observed mean dispersal distance and release height. Variables are standardised and the outlier *Acaena novae-zelandiae* is removed. Three Gaussian `glmmTMB` models are compared by AIC (no random effect, species random effect, dispersal-syndrome random effect); the simplest model is retained. Outputs a coefficient plot to `output/Dispersal model/` and a prediction-check figure to `output/Appendix/`.

**`Traps and seed analysis.R`**
Analyses diaspore capture by trap position. Counts are aggregated to trap-level totals and fitted with a negative binomial GLMM (`glmmTMB`, `nbinom2`) with trap position as a fixed effect and site as a random effect. Also produces summaries and figures by dispersal syndrome, growth form and species. Outputs to `output/Traps and seed output/` and `output/Appendix/`.

**`Vertical wind events code.R`**
Estimates the probability that vertical wind (updraft) events exceed a species' terminal velocity. Fits a binomial GLM (proportion of events above threshold as a function of terminal velocity) and compares it against a GAM smooth and a polynomial term; the simpler GLM is retained. Outputs combined coefficient and fitted-line figures to `output/Vertical wind events model/` and a prediction-check figure to `output/Appendix/`.

**`Wind speed code.R`**
Compares vertical and horizontal wind speed using a paired t-test on square-root transformed data, and summarises wind speed by height above ground. Outputs the figure to `output/Wind speed/`.

**`Traps code (possible not used).R`**
Exploratory script for the wind speed comparison. Superseded by `Wind speed code.R` and not used in the manuscript. Retained for transparency.

## Requirements

Analyses were run in R. The scripts use the following packages:

```r
install.packages(c(
  "tidyverse", "cowplot", "glmmTMB", "lme4", "lmerTest",
  "DHARMa", "performance", "ggeffects", "mgcv", "emmeans",
  "MuMIn", "modelsummary", "gridExtra", "effsize",
  "arm", "report", "Matrix", "stringr"))
```

Each script calls `sessionInfo()` at the end, so the exact R and package versions used can be recorded in the console output.

## Reproducing the analyses

1. Clone or download the repository.
2. Open `alpine_seed_dispersal.Rproj` in RStudio.
3. Install the packages listed above.
4. Run any script in `code/`. Figures are written to the corresponding `output/` subfolder.

## Licence

See the `LICENSE` file in the repository root.

## Contact

For questions about the data or analyses, please contact the corresponding author or open an issue on this repository.
