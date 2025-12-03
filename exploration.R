#Project: Prediction of groin pain/injury in female footballers
#Author: Marcos Quintana Cepedal
#Contact: marcosquintana99@gmail.com
#Last update: 2/07/25

# Load packages and data
library(skimr)
library(dplyr)
library(readxl)
library(compareGroups)

df <- read_excel("dfgpf.xlsx", col_types = c("numeric", "numeric", "numeric", "text", 
                                                "numeric", "text", "text", "text", "text", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric", "text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric"))

# Create BMI df
df$bmi <- df$bmass / ((df$height / 100) ^ 2)

# Calculate descriptive baseline statistics
df %>%
  skim()

# Explore dataset by groups (injured and non-injured)
df %>% 
  group_by(isgp) %>% 
  skim()

# Count of injured participants
table(df$isgp)

# Compare groups (uninjured vs injured)
compareGroups(isgp ~ ., data = df)

# Count of limb dominance and history of past injury
df$isgp <- ifelse(df$isgp == 0, "No", "Sí")

table(df$isgp, df$limb_dominance)
table(df$isgp, df$prevgptl)
table(df$isgp, df$prevgpntl)
  
# Compare groups with Hedge's g
library(MOTE)

# Age
g.ind.t(22.1, 18.1, 5.4, 3.1, 14, 23, a = 0.05)

# Height
g.ind.t(167, 165, 5, 5, 14, 23, a = 0.05)

# Body mass
g.ind.t(62.4, 56.8, 8, 5, 14, 23, a = 0.05)

# BMI
g.ind.t(22.4, 20.9, 2.7, 1.4, 14, 23, a = 0.05)

# Football years
g.ind.t(14.4, 9, 6, 3, 14, 23, a = 0.05)

# Limb length
g.ind.t(0.84, 0.83, 0.04, 0.04, 14, 23, a = 0.05)

# BKFO
g.ind.t(13.9, 13.5, 3.2, 2.9, 14, 23, a = 0.05)

# N
g.ind.t(151, 156, 31, 26, 14, 23, a = 0.05)

# T
g.ind.t(2, 2.3, 0.42, 0.4, 14, 23, a = 0.05)

# BAPT
g.ind.t(19, 21, 9, 9, 14, 23, a = 0.05)

# HAGOS-S
g.ind.t(86.4, 89.4, 15, 16, 14, 23, a = 0.05)

# BKFO asymmetry
g.ind.t(1.03, 0.97, 0.08, 0.1, 14, 23, a = 0.05)

# Torque asymmetry
g.ind.t(0.94, 0.93, 0.11, 0.1, 14, 23, a = 0.05)

# BAPT asymmetry
g.ind.t(1.04, 0.95, 0.27, 0.21, 14, 23, a = 0.05)

# RR between groups
library(epitools)

# Time-loss injury
groin_pain_table <- matrix(c(7, 7,  # Injured (Reference) row
                                     7, 16),  # Uninjured row
                                   nrow = 2, 
                                   byrow = TRUE,
                                   dimnames = list(
                                     Exposure = c("Past injury (ref)", "Uninjured"),
                                     Outcome = c("Groin Pain", "No Groin Pain")
                                   ))

# Print the table to verify the structure
print(groin_pain_table)

# 3. Calculate the Risk Ratio (RR)
# The default behavior now compares Row 2 (Intervention) to Row 1 (Control).
rr_result <- riskratio(groin_pain_table,
                       conf.level = 0.95,
                       method = "wald",
                      rev = c("both"))

# 4. View the results
rr_result

# Non-time-loss groin injury
groin_pain_table_ntl <- matrix(c(8, 6,  # Injured (Reference) row
                                     6, 17),  # Uninjured row
                                   nrow = 2, 
                                   byrow = TRUE,
                                   dimnames = list(
                                     Exposure = c("Past injury (ref)", "Uninjured"),
                                     Outcome = c("Groin Pain", "No Groin Pain")
                                   ))

# Print the table to verify the structure
print(groin_pain_table_ntl)

# 3. Calculate the Risk Ratio (RR)
# The default behavior now compares Row 2 (Intervention) to Row 1 (Control).
rr_resultb <- riskratio(groin_pain_table_ntl,
                       conf.level = 0.95,
                       method = "wald",
                      rev = c("both"))

# 4. View the results
rr_resultb



