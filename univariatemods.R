#Project: Prediction of groin pain/injury in female footballers
#Author: Marcos Quintana Cepedal
#Contact: marcosquintana99@gmail.com
#Last update: 27/10/25

# Load packages and data
library(dplyr)
library(readxl)
library(pROC)

library(readxl)
df <- read_excel("dfgpf.xlsx", col_types = c("numeric", "numeric", "numeric", "text", 
                                             "numeric", "text", "text", "text", "text", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric"))

# Create BMI df
df$bmi <- df$bmass / ((df$height / 100) ^ 2)

# Eliminate variables unused in the regression analyses
dfgp <- select(df, -team, -position,-isgptl, -sever, -lost)

# Set seed
set.seed(123)

# Univariate models for groin pain (ntl and tl) without SMOTE
predictors <- setdiff(names(dfgp), "isgp")

results <- data.frame(
  variable = character(),
  AUC = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

n <- nrow(dfgp)

for (var in predictors) {
  predicted_probs <- numeric(n)
  actual <- as.numeric(as.character(dfgp$isgp))
  
  for (i in 1:n) {
    train_data <- dfgp[-i, ]
    test_data <- dfgp[i, , drop = FALSE]
    
    formula <- as.formula(paste("isgp ~", var))
    
    model <- glm(formula, data = train_data, family = binomial)
    predicted_probs[i] <- predict(model, newdata = test_data, type = "response")
  }
  
  roc_obj <- roc(actual, predicted_probs, ci = TRUE)
  auc_val <- auc(roc_obj)
  ci_vals <- ci.auc(roc_obj, method = c("bootstrap"), boot.n = 2000)
  
  best_coords <- coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
  sens <- best_coords["sensitivity"]
  spec <- best_coords["specificity"]
  
  results <- rbind(results, data.frame(
    variable = var,
    AUC = auc_val,
    CI_lower = ci_vals[1],
    CI_upper = ci_vals[3],
    Sensitivity = sens,
    Specificity = spec
  ))
}

print(results)
