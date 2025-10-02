# Clear Environment
rm(list = ls())
# Clear console
cat("\014")

if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

if (!require(pROC)) install.packages("pROC")
library(pROC)

if (!dir.exists("results\\")) {
  dir.create("results\\", recursive = TRUE)
}

load("data\\data.RData")

# FUNCTIONS --------------------------------------------------------------------
impute_mean_mode <- function(training_set, test_set, varnames_mean, varnames_mode) {
  
  imputed_training_set <- training_set
  imputed_test_set <- test_set
  
  # mean imputation
  for (var in varnames_mean) {
    mean_val <- mean(imputed_training_set[[var]], na.rm = TRUE)
    imputed_training_set[[var]][is.na(imputed_training_set[[var]])] <- mean_val
    imputed_test_set[[var]][is.na(imputed_test_set[[var]])] <- mean_val
  }
  
  # mode imputation
  for (var in varnames_mode) {
    freq <- table(imputed_training_set[[var]])
    all_vals <- sort(unique(imputed_training_set[[var]]))
    mode_val <- all_vals[which.max(freq)]
    
    imputed_training_set[[var]][is.na(imputed_training_set[[var]])] <- mode_val
    imputed_test_set[[var]][is.na(imputed_test_set[[var]])] <- mode_val
  }
  
  return(list(imputed_training_set = imputed_training_set,
              imputed_test_set = imputed_test_set))
}

normalize_min_max <- function(training_set, test_set, varnames_to_normalize) {
  
  normalized_training_set <- training_set
  normalized_test_set <- test_set
  
  for (var in varnames_to_normalize) {
    min_val <- min(training_set[[var]])
    max_val <- max(training_set[[var]])
    
    normalized_training_set[[var]] <- (training_set[[var]] - min_val) / (max_val - min_val)
    normalized_test_set[[var]] <- (test_set[[var]] - min_val) / (max_val - min_val)
  }
  
  return(list(normalized_training_set = normalized_training_set,
              normalized_test_set = normalized_test_set))
}

# ====================================
# DATA PREPROCESSING
# ====================================
# DATA INSPECTION

# Number of rows (subjects) and columns (variables)
n_rows <- nrow(data)
n_cols <- ncol(data)

cat("Number of subjects:", n_rows, "\n")
cat("Number of variables:", n_cols, "\n")

# Data Inspection
cat("Dataset Summary: \n")
print(summary(data))

# TRAIN/TEST SPLIT

set.seed(0)

# Indices of subjects with outcome 0 and 1
ind_0 <- which(data$outcome == 0)
ind_1 <- which(data$outcome == 1)

# Stratified sampling: 80% training, 20% test
train_ind_0 <- sample(ind_0, size = round(0.8 * length(ind_0)))
train_ind_1 <- sample(ind_1, size = round(0.8 * length(ind_1)))

# Test indices = remaining subjects
test_ind_0 <- setdiff(ind_0, train_ind_0)
test_ind_1 <- setdiff(ind_1, train_ind_1)

# Create the two datasets
ind_training <- c(train_ind_0, train_ind_1)
training_set <- data[c(train_ind_0, train_ind_1), ]
test_set <- data[c(test_ind_0, test_ind_1), ]

# Required output
cat("--- TRAINING SET ---\n")
cat("Total subjects:", nrow(training_set), "\n")
cat("Outcome = 0:", sum(training_set$outcome == 0), "\n")
cat("Outcome = 1:", sum(training_set$outcome == 1), "\n\n")

cat("--- TEST SET ---\n")
cat("Total subjects:", nrow(test_set), "\n")
cat("Outcome = 0:", sum(test_set$outcome == 0), "\n")
cat("Outcome = 1:", sum(test_set$outcome == 1), "\n\n")

# RULE OF THUMB CHECK

# Convert categorical variables to factor
categorical_vars <- c("gender", "smoking", "chest_pain", "phlegm",
                      "short_breath_walking", "wheezing", "wake_breath", 
                      "asthma_hx", "outcome")
training_set[categorical_vars] <- lapply(training_set[categorical_vars], as.factor)
test_set[categorical_vars] <- lapply(test_set[categorical_vars], as.factor)

# Count minority class samples
minority_n <- sum(training_set$outcome == 1)
cat("Minority class size (outcome = 1):", minority_n, "\n")

# Estimate number of coefficients: numeric vars + (k - 1) per categorical
X <- training_set[, names(training_set) != "outcome"]
n_numeric <- sum(sapply(X, is.numeric))
numeric_vars <- names(X)[sapply(X, is.numeric)]
n_factor <- sum(sapply(X[, sapply(X, is.factor), drop = FALSE], function(x) nlevels(x) - 1))
n_coeff <- n_numeric + n_factor

cat("Number of coefficients (n_coeff):", n_coeff, "\n")

# Compute ratio and check condition
ratio <- minority_n / n_coeff
cat("Rule of thumb ratio (minority_n / n_coeff):", round(ratio, 2), "\n")
cat(ifelse(ratio >= 10, "Condition satisfied: ratio >= 10\n\n", "Condition NOT satisfied: ratio < 10\n\n"))

# OUTLIER DETECTION

for (var in numeric_vars) {
  vals <- training_set[[var]]
  
  # Save boxplot
  png(filename = paste0("results\\boxplot_", var, ".png"), width = 300, height = 350)
  boxplot(vals, main = paste("Boxplot of", var), ylab = var)
  dev.off()
  
  # Replace outliers with NA (outside mean +- 4*sd)
  lims <- mean(vals, na.rm = TRUE) + c(-4, 4) * sd(vals, na.rm = TRUE)
  idx_outliers <- which(vals < lims[1] | vals > lims[2])
  training_set[idx_outliers, var] <- NA
  
  cat(paste("Replaced", length(idx_outliers), "extreme values in", var, "\n"))
}
cat("\n")

# MULTICOLLINEARITY CHECK

cor_matrix <- cor(na.omit(training_set[, numeric_vars]))

png(filename = paste0("results\\corrplot.png"), width = 500, height = 500)
corrplot(cor_matrix, method = "number")
dev.off()

high_corr <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (is.null(dim(high_corr))) {
  high_corr <- matrix(high_corr, ncol = 2)
  colnames(high_corr) <- c("row", "col")
}
high_corr <- high_corr[high_corr[, "row"] < high_corr[, "col"], , drop = FALSE]

if (nrow(high_corr) > 0) {
  cat("Highly correlated variable pairs (|correlation| > 0.80):\n")
  for (i in seq_len(nrow(high_corr))) {
    v1 <- colnames(cor_matrix)[high_corr[i, "row"]]
    v2 <- colnames(cor_matrix)[high_corr[i, "col"]]
    rho <- cor_matrix[v1, v2]
    cat(paste0("- ", v1, " and ", v2, ": ", round(rho * 100, 1), "%\n"))
  }
  
  vars_to_remove <- unique(colnames(cor_matrix)[high_corr[, "row"]])
  training_set <- training_set[, !(names(training_set) %in% vars_to_remove)]
  test_set <- test_set[, !(names(test_set) %in% vars_to_remove)]
  numeric_vars <- setdiff(numeric_vars, vars_to_remove)  # update numeric_vars
  
  cat("\nRemoved due to high correlation:\n", vars_to_remove, "\n")
} else {
  cat("No variable pairs with correlation > 0.80 found.\n\n")
}

# MISSING VALUE IMPUTATION

# percentage of missing values for each variable
cat("NAs percentages for each variable (training_set):\n")
print(round(sapply(training_set, function(x) mean(is.na(x)) * 100), 2))

# quantitative vars to impute with mean
mean_vars <- c("age", "bmi", "fvc", "pf", "fev1_fvc_ratio")

# other variables, to impute with mode
all_vars <- colnames(training_set)
mode_vars <- setdiff(all_vars, mean_vars)

# imputation
imputed_data <- impute_mean_mode(training_set, test_set, mean_vars, mode_vars)
training_set <- imputed_data$imputed_training_set
test_set <- imputed_data$imputed_test_set

# verify there are no NAs left
cat("NAs in training set after imputation:\n")
print(colSums(is.na(training_set)))

cat("\nNAs in test set after imputation:\n")
print(colSums(is.na(test_set)))
cat("\n")

# NORMALIZATION

# numeric variables
is_numeric <- sapply(training_set, is.numeric)
numeric_vars <- names(training_set)[is_numeric]

# min-max scaling using function
normalized_data <- normalize_min_max(training_set, test_set, numeric_vars)
training_set <- normalized_data$normalized_training_set
test_set <- normalized_data$normalized_test_set

# check that all values are in the range [0, 1]
cat("Normalized value ranges in training_set:\n")
for (var in numeric_vars) {
  range_vals <- range(training_set[[var]])
  cat(paste0(var, ": [", round(range_vals[1], 3), ", ", round(range_vals[2], 3), "]\n"))
}

cat("\nNormalized value ranges in test_set:\n")
for (var in numeric_vars) {
  range_vals <- range(test_set[[var]])
  cat(paste0(var, ": [", round(range_vals[1], 3), ", ", round(range_vals[2], 3), "]\n"))
}

# ====================================
# MODEL TRAINING & EVALUATION
# ====================================

# LOGISTIC REGRESSION (Full Model)

# Logistic regression full model
full_model <- glm(outcome ~ ., data = training_set, family = "binomial")

cat("Model Summary: \n")
print(summary(full_model))

# ROC CURVE & AUC

# Prediction on test set
predicted_prob <- predict(full_model, newdata = test_set, type = "response")

# AUC calculation and ROC curve plot
roc_obj <- roc(response = test_set$outcome, predictor = predicted_prob)

cat("AUC on test set: ", roc_obj$auc, "\n")

png(filename = paste0("results\\ROC_curve.png"), width = 500, height = 500)
plot(roc_obj, main = "ROC Curve - Full Model", col = "blue")
dev.off()

# REDUCED MODEL

# Full e null model
full_model <- glm(outcome ~ ., data = training_set, family = "binomial")
null_model <- glm(outcome ~ 1, data = training_set, family = "binomial")
model_range <- list(lower = formula(null_model), upper = formula(full_model))

cat("--- Feature Selection: Backward ---\n")
reduced_model <- step(full_model, scope = model_range, direction = "backward", trace = TRUE)

cat("\n--- Reduced Model Summary ---\n")
print(summary(reduced_model))

# predictions and AUC
pred_prob_reduced <- predict(reduced_model, newdata = test_set, type = "response")
roc_reduced <- roc(response = test_set$outcome, predictor = pred_prob_reduced)

cat("AUC - Reduced model: ", roc_reduced$auc, "\n")

# ROC curve plot
png(filename = paste0("results\\ROC_curve_reduced.png"), width = 500, height = 500)
plot(roc_reduced, main = "ROC Curve - Reduced Model", col = "red")
dev.off()

cat("AUC - Full model: ", auc(roc_obj), "\n")

# ROC curves comparison --------------------------------------------------------
cat("\n--- Feature Selection: Forward ---\n")
forward_model <- step(null_model, scope = model_range, direction = "forward", trace = TRUE)
cat("\n--- Forward Model Summary ---\n")
print(summary(forward_model))

cat("\n--- Feature Selection: Stepwise Backward ---\n")
stepwise_bwd_model <- step(full_model, scope = model_range, direction = "both", trace = TRUE)
cat("\n--- Stepwise Backward Model Summary ---\n")
print(summary(stepwise_bwd_model))

cat("\n--- Feature Selection: Stepwise Forward ---\n")
stepwise_fwd_model <- step(null_model, scope = model_range, direction = "both", trace = TRUE)
cat("\n--- Stepwise Forward Model Summary ---\n")
print(summary(stepwise_fwd_model))

# ROC Calculation for the new models
pred_prob_forward <- predict(forward_model, newdata = test_set, type = "response")
roc_forward <- roc(response = test_set$outcome, predictor = pred_prob_forward)

pred_prob_stepwise_bwd <- predict(stepwise_bwd_model, newdata = test_set, type = "response")
roc_stepwise_bwd <- roc(response = test_set$outcome, predictor = pred_prob_stepwise_bwd)

pred_prob_stepwise_fwd <- predict(stepwise_fwd_model, newdata = test_set, type = "response")
roc_stepwise_fwd <- roc(response = test_set$outcome, predictor = pred_prob_stepwise_fwd)

png(filename = "results\\ROC_4_models.png", width = 2000, height = 500)
par(mfrow = c(1, 4))

plot(roc_reduced, main = "Backward", col = "red")
plot(roc_forward, main = "Forward", col = "green")
plot(roc_stepwise_bwd, main = "Stepwise Backward", col = "purple")
plot(roc_stepwise_fwd, main = "Stepwise Forward", col = "blue")

dev.off()

## ====================================
# FEATURE SELECTION & STABILITY
# ====================================

# BOOTSTRAP RESAMPLING

B <- 50
threshold <- 0.60
set.seed(0)

varnames_mean <- c("age", "bmi", "fvc", "pf", "fev1_fvc_ratio")
varnames_mode <- setdiff(colnames(training_set), c(varnames_mean, "outcome"))

selected_vars_list <- list()

for (b in 1:B) {
  cat("Bootstrap iteration:", b, "\n")
  
  # bootstrap stratified indexes
  idx_0 <- sample(which(training_set$outcome == 0), replace = TRUE)
  idx_1 <- sample(which(training_set$outcome == 1), replace = TRUE)
  boot_idx <- c(idx_0, idx_1)
  internal_train <- training_set[boot_idx, ]
  
  # imputation and normalization
  imputed_data <- impute_mean_mode(internal_train, internal_train, varnames_mean, varnames_mode)
  norm_data <- normalize_min_max(imputed_data$imputed_training_set, imputed_data$imputed_training_set, varnames_mean)
  internal_train_preproc <- norm_data$normalized_training_set
  
  # backward selection
  full_model <- glm(outcome ~ ., data = internal_train_preproc, family = "binomial")
  null_model <- glm(outcome ~ 1, data = internal_train_preproc, family = "binomial")
  model_range <- list(lower = formula(null_model), upper = formula(full_model))
  reduced_model <- step(full_model, scope = model_range, direction = "backward", trace = FALSE)
  
  selected_vars <- attr(terms(reduced_model), "term.labels")
  selected_vars_list[[b]] <- selected_vars
}

# VARIABLE SELECTION FREQUENCIES

# selection frequency
selected_freq <- table(unlist(selected_vars_list)) / B
selected_freq_sorted <- sort(selected_freq, decreasing = TRUE)
cat("Frequency of selection: \n")
print(round(selected_freq_sorted, 2))

png(filename = paste0("results\\var_selection_freq.png"), width = 500, height = 500)
par(mar = c(10, 4, 4, 2))
barplot(selected_freq_sorted, las=2, col="steelblue",
        main="Variables frequency of selection",
        ylab="Frequency", ylim=c(0,1))
abline(h = threshold, col = "red", lty = 2)
dev.off()

final_vars <- names(selected_freq[selected_freq >= threshold])

# FINAL STABLE MODEL

imputed_all <- impute_mean_mode(training_set, test_set, varnames_mean, varnames_mode)
norm_all <- normalize_min_max(imputed_all$imputed_training_set, imputed_all$imputed_test_set, varnames_mean)

final_train <- norm_all$normalized_training_set
final_test <- norm_all$normalized_test_set

# final formula
final_formula <- as.formula(paste("outcome ~", paste(final_vars, collapse = " + ")))
final_model <- glm(final_formula, data = final_train, family = "binomial")

# prediction
P_final <- predict(final_model, newdata = final_test, type = "response")
roc_obj <- roc(response = final_test$outcome, predictor = P_final)
cat("AU-ROC in final model:", round(roc_obj$auc, 3))
png(filename = paste0("results\\ROC_curve_final_4.png"), width = 500, height = 500)
plot(roc_obj, main = "ROC curve - Final model", col="blue")
dev.off()

print(summary(final_model))
