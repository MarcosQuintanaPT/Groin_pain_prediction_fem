# ==============================================================================
# Project: Prediction of groin pain/injury in female footballers
# Author: Marcos Quintana Cepedal
# Contact: marcosquintana99@gmail.com
# Last update: 2026-06-29
# Description: Correlation analysis and predictive univariate modeling (LOOCV).
# ==============================================================================

# 1. Load Packages and Data ----------------------------------------------------
library(readxl)
library(dplyr)
library(pROC)

# Reload data ensuring structural consistency for regression
df <- read_excel("C:/Users/Marcos/OneDrive/Escritorio/Investigación/Toni_Bailen/df_phys_ther_sport.xlsx", 
                 col_types = c("numeric", "numeric", "numeric", "text", 
                               "numeric", "text", "text", "text", "text", 
                               "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "text", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))

# Calculate BMI
df$bmi <- df$bmass / ((df$height / 100) ^ 2)

# Filter out variables unused in the regression analyses
dfgp <- df %>% select(-team, -position, -isgptl)

# 2. Correlation Analysis ------------------------------------------------------
cor.test(dfgp$age, dfgp$footyears, method = "pearson")
cor.test(dfgp$bmi, dfgp$bmass, method = "pearson")

# 3. Univariate Modeling via LOOCV (No SMOTE) ----------------------------------
set.seed(123)

predictors <- setdiff(names(dfgp), "isgp")
n <- nrow(dfgp)

# Initialize results storage dataframe
results <- data.frame(
  variable = character(),
  AUC = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each predictor variable using Leave-One-Out Cross-Validation
for (var in predictors) {
  predicted_probs <- numeric(n)
  actual <- as.numeric(as.character(dfgp$isgp))
  
  for (i in 1:n) {
    train_data <- dfgp[-i, ]
    test_data  <- dfgp[i, , drop = FALSE]
    
    formula <- as.formula(paste("isgp ~", var))
    
    model <- glm(formula, data = train_data, family = binomial)
    predicted_probs[i] <- predict(model, newdata = test_data, type = "response")
  }
  
  # Calculate ROC metrics
  roc_obj  <- roc(actual, predicted_probs, ci = TRUE)
  auc_val  <- auc(roc_obj)
  ci_vals  <- ci.auc(roc_obj, method = "bootstrap", boot.n = 2000)
  
  # Extract optimal thresholds via Youden's Index
  best_coords <- coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
  sens <- best_coords["sensitivity"]
  spec <- best_coords["specificity"]
  
  # Append to final results data frame
  results <- rbind(results, data.frame(
    variable    = var,
    AUC         = auc_val,
    CI_lower    = ci_vals[1],
    CI_upper    = ci_vals[3],
    Sensitivity = sens,
    Specificity = spec
  ))
}

# 4. Print Summary Table -------------------------------------------------------
print(results)