# ==============================================================================
# Project: Prediction of groin pain/injury in female footballers
# Author: Marcos Quintana Cepedal
# Contact: marcosquintana99@gmail.com
# Last update: 2026-06-29
# Description: Data loading, descriptive statistics, and group comparisons.
# ==============================================================================

# 1. Load Packages -------------------------------------------------------------
library(readxl)
library(dplyr)
library(skimr)
library(compareGroups)
library(MOTE)
library(epitools)

# 2. Load Data and Preprocessing -----------------------------------------------
df <- read_excel("C:/Users/Marcos/OneDrive/Escritorio/Investigación/Toni_Bailen/df_phys_ther_sport.xlsx", 
                 col_types = c("numeric", "numeric", "numeric", "text", 
                               "numeric", "text", "text", "text", "text", 
                               "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "text", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))

# Calculate BMI
df$bmi <- df$bmass / ((df$height / 100) ^ 2)

# 3. Descriptive & Exploratory Data Analysis ----------------------------------
# Count previous TL and NTL
table(df$prevgptl)
table(df$prevgpntl)

# Baseline descriptive statistics
df %>% skim()

# Explore dataset grouped by injury status
df %>% 
  group_by(isgp) %>% 
  skim()

# Total count of injured participants
table(df$isgp)

# Compare overall groups (uninjured vs injured)
compareGroups(isgp ~ ., data = df)

# Recode response variable for clarity in cross-tabulations
df$isgp_label <- ifelse(df$isgp == 0, "No", "Sí")

# Cross-tabulations for categorical variables
table(df$isgp_label, df$limb_dominance)
table(df$isgp_label, df$prevgptl)
table(df$isgp_label, df$prevgpntl)

# 4. Effect Size Calculations (Hedge's g) --------------------------------------
# Format: g.ind.t(mean1, mean2, sd1, sd2, n1, n2, a = 0.05)
g.ind.t(22.1, 18.1, 5.4, 3.1, 14, 23, a = 0.05)       # Age
g.ind.t(167, 165, 5, 5, 14, 23, a = 0.05)             # Height
g.ind.t(62.4, 56.8, 8, 5, 14, 23, a = 0.05)           # Body mass
g.ind.t(22.4, 20.9, 2.7, 1.4, 14, 23, a = 0.05)       # BMI
g.ind.t(14.4, 9, 6, 3, 14, 23, a = 0.05)              # Football years
g.ind.t(0.84, 0.83, 0.04, 0.04, 14, 23, a = 0.05)     # Limb length
g.ind.t(13.9, 13.5, 3.2, 2.9, 14, 23, a = 0.05)       # BKFO
g.ind.t(151, 156, 31, 26, 14, 23, a = 0.05)           # N
g.ind.t(2, 2.3, 0.42, 0.4, 14, 23, a = 0.05)           # T
g.ind.t(19, 21, 9, 9, 14, 23, a = 0.05)               # BAPT
g.ind.t(86.4, 89.4, 15, 16, 14, 23, a = 0.05)         # HAGOS-S
g.ind.t(1.03, 0.97, 0.08, 0.1, 14, 23, a = 0.05)      # BKFO asymmetry
g.ind.t(0.94, 0.93, 0.11, 0.1, 14, 23, a = 0.05)      # Torque asymmetry
g.ind.t(1.04, 0.95, 0.27, 0.21, 14, 23, a = 0.05)     # BAPT asymmetry

# 5. Risk Ratio (RR) Analyses --------------------------------------------------
# Time-loss groin injury matrix
groin_pain_table <- matrix(c(22, 1,   # Uninjured (Reference) row
                             7, 7),   # Past injury row
                           nrow = 2, 
                           byrow = TRUE,
                           dimnames = list(
                             Exposure = c("Uninjured (ref)", "Injured"),
                             Outcome = c("No past GPTL", "Past GPTL")
                           ))

print(groin_pain_table)
rr_result <- riskratio(groin_pain_table, method = "small", conf.level = 0.95)
print(rr_result)

# Non-time-loss groin injury matrix
groin_pain_table_ntl <- matrix(c(18, 5,   # Uninjured (Reference) row
                                 6, 8),   # Past injury row
                               nrow = 2, 
                               byrow = TRUE,
                               dimnames = list(
                                 Exposure = c("Uninjured (ref)", "Injured"),
                                 Outcome = c("No past GPNTL", "Past GPNTL")
                               ))

print(groin_pain_table_ntl)
rr_resultb <- riskratio(groin_pain_table_ntl, method = "small", conf.level = 0.95)
print(rr_resultb)