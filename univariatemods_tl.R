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

# Filter dataset to include only relevant groups
df_filtered <- df %>%
  filter(
    (isgp == "0" & isgptl == "0") |  
    (isgp == "1" & isgptl == "1") 
  )

# Eliminate variables unused in the regression analyses
dfgptl <- select(df_filtered, -team, -position,-isgp, -sever, -lost)

table(dfgptl$isgptl)

# Ensure your outcome variable is explicitly numeric (0 or 1).
dfgptl$isgptl <- as.numeric(dfgptl$isgptl) 

# Set seed
set.seed(123)

# Define predictors by excluding the outcome variable
predictors <- setdiff(names(dfgptl), "isgptl")

# Initialize the results data frame
results <- data.frame(
  variable = character(),
  AUC = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

n <- nrow(dfgptl)

for (var in predictors) {
  # Initialize vectors to store results
  predicted_probs <- numeric(n)
  # Actual outcome vector (already 0/1 from step 2)
  actual <- dfgptl$isgptl 
  
  # --- Leave-One-Out Cross-Validation (LOOCV) loop ---
  for (i in 1:n) {
    train_data <- dfgptl[-i, ]
    test_data <- dfgptl[i, , drop = FALSE]
    
    # Formula for univariate logistic regression model
    formula <- as.formula(paste("isgptl ~", var))
    
    # glm now uses the numeric 0/1 outcome variable
    model <- glm(formula, data = train_data, family = binomial)
    
    # Predict probability for the left-out observation
    predicted_probs[i] <- predict(model, newdata = test_data, type = "response")
  }
  
  # --- ROC Metrics Calculation ---
  
  # Calculate ROC curve object
  roc_obj <- roc(actual, predicted_probs, ci = TRUE) 
  
  # Calculate AUC
  auc_val <- auc(roc_obj)
  
  # Calculate Bootstrap Confidence Interval for AUC (using 2000 replicates)
  # This may take a moment due to the 2000 iterations.
  ci_vals <- ci.auc(roc_obj, method = "bootstrap", boot.n = 2000)
  
  # Find optimal cut-off based on Youden's J statistic
  best_coords <- coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
  sens <- best_coords["sensitivity"]
  spec <- best_coords["specificity"]
  
  # Store results
  results <- rbind(results, data.frame(
    variable = var,
    AUC = auc_val,
    CI_lower = ci_vals[1], # Lower bound
    CI_upper = ci_vals[3], # Upper bound
    Sensitivity = sens,
    Specificity = spec
  ))
}


# Describe sample
library(skimr)

dfgptl |> 
  subset(isgptl == 1) |>
  skim(age, height, bmass, bmi, footyears, limb_lenght,
        bkfo, n, t, bapt, hagos_s, bkfo_asym, torque_asym,
      bapt_asym)

# Counts for prior injury (time-loss)
table(dfgptl$prevgptl, dfgptl$isgptl)

# Counts for prior injury (non-time-loss)
table(dfgptl$prevgpntl, dfgptl$isgptl)
