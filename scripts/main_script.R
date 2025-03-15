# Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(stringr)
library(e1071)
library(nnet)
library(corrplot)
library(GGally)
library(Kendall)
library(lsr)
library(rcompanion)
library(MLmetrics)
library(rpart)
library(rpart.plot)
library(partykit)
library(pROC)
library(randomForest)

# Importing the CSV data:
Dados_Original <- read.csv('CreditDataset.csv',
                           sep = ",",
                           quote = "\"",
                           dec = ".",
                           header = TRUE)
Dados <- Dados_Original

# Summary of the initial data:
summary(Dados)

# To check for non-numeric values in the columns, we created this code that lists up to 30 non-numeric values for each column to avoid overwhelming the console:
for (coluna in colnames(Dados)) {
  valores_nao_numericos <- unique(Dados[[coluna]][!grepl("^[0-9]*\\.?[0-9]+$", Dados[[coluna]])])
  if (length(valores_nao_numericos) > 0) {
    cat("Non-numeric values found in the column", coluna, ":\n")
    print(head(valores_nao_numericos, 30))
  } else {
    cat("All values in the column", coluna, "are numeric.\n")
  }
  cat("\n***************************************************\n\n")
}

# Two auxiliary scripts were used: one with all the steps for cleaning, and the other with helper functions.
source("aux_functions.R")
source("data_cleaning.R")

# Training/test set division:
set.seed(100)
index_1 <- createDataPartition(Dados$Credit_Score, p = 0.67, list = FALSE)
train_1 <- Dados[index_1, ]
test_1 <- Dados[-index_1, ]

# Data preprocessing using standardization:
preProc <- preProcess(train_1, method = c("center", "scale"))
train_1_processed <- predict(preProc, train_1)
test_1_processed <- predict(preProc, test_1)


# The correlation between features was studied for both numerical and categorical values to analyze if any are highly correlated:

# Numeric Features (Pearson and Spearman Coefficients):
exclude_columns <- c("ID", "Month", "Payment_Value", "Spending_Level", "Credit_MixStandard", "Credit_MixGood", 
                     "Credit_MixBad", "Credit_Mix", "Payment_of_Min_Amount", "Age_Group", "Credit_Score",
                     "Payment_of_Min_AmountYes", "Payment_of_Min_AmountNo")

# Addition (for later removal) of the "Occupation" column to the auxiliary variable exclude_columns:
occupation_columns <- grep("^Occupation", names(train_1_processed), value = TRUE)
exclude_columns <- c(exclude_columns, occupation_columns)
high_corr_pearson  <- plot_corr_heatmap(train_1_processed, corr_limits = c(-0.4, 0.4), exclude_columns = exclude_columns, method = "pearson")
high_corr_spearman <- plot_corr_heatmap(train_1_processed, corr_limits = c(-0.4, 0.4), exclude_columns = exclude_columns, method = "spearman")

# Checking pairs of variables with a difference >= 0.1 between Pearson and Spearman coefficients:
merged_corr <- merge(high_corr_pearson, high_corr_spearman, by = c("Variable1", "Variable2"), 
                     suffixes = c("_pearson", "_spearman"), all = TRUE)

# Calculating the absolute difference between Pearson and Spearman correlations:
merged_corr$correlation_diff <- abs(merged_corr$Correlation_pearson - merged_corr$Correlation_spearman) * 100 
colnames(merged_corr)[colnames(merged_corr) == "correlation_diff"] <- "correlation_diff %"
if (any(!is.na(merged_corr$`correlation_diff %`) & merged_corr$`correlation_diff %` >= 10)) {
  print(merged_corr[!is.na(merged_corr$`correlation_diff %`) & 
                      merged_corr$`correlation_diff %` >= 10, c("Variable1", "Variable2", "correlation_diff %")])
} else {
  print("No correlations found")
}


# Categorical Features (using Cramér's V and Kendall's Tau):
Occupation_df <- train_1 %>%
  select("Credit_Score", "Occupation")
TOL_df <- train_1 %>%
 select("Credit_Score", "Type_of_Loan")

# Cramér's V for the "Occupation" variable:
Tab_Occupation_CS <- table(train_1$Occupation, train_1$Credit_Score)
cramerV_Occupation <- cramerV(Tab_Occupation_CS)

cat("Cramér's V for Occupation and Credit_Score:", cramerV_Occupation, "\n")

df_Occupation_CS <- as.data.frame(as.table(Tab_Occupation_CS))

ggplot(df_Occupation_CS, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = paste("Heatmap of Occupation and Credit_Score\nCramér's V =", round(cramerV(Tab_Occupation_CS), 4)),
       x = "Occupation",
       y = "Credit Score",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Result: Occupation // Credit_Score V=0.03351
train_1_processed$Occupation <- NULL
test_1_processed$Occupation <- NULL


# Cramér's V for the "Type_of_Loan" variable:
Tab_TOL_CS <- table(train_1$Type_of_Loan, train_1$Credit_Score)
cramerV_TOL <- cramerV(Tab_TOL_CS)

cat("Cramér's V for Type_of_Loan and Credit_Score:", cramerV_TOL, "\n")

df_TOL_CS <- as.data.frame(as.table(Tab_TOL_CS))

ggplot(df_TOL_CS, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = paste("Heatmap of Type_of_Loan and Credit_Score\nCramér's V =", round(cramerV(Tab_TOL_CS), 4)),
       x = "Type of Loan",
       y = "Credit Score",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Result: Correlation between TOL and NOL n=0.963737:
correlation_ratio <- function(x, y) {
  groups <- split(x, y)
  ssw <- sum(sapply(groups, function(group) sum((group - mean(group))^2)))
  ssb <- sum(sapply(groups, function(group) length(group) * (mean(group) - mean(x))^2))
  ssb / (ssw + ssb)
}

# Result: Type_of_Loan // Credit_Score V=0.6026:
train_1_processed$Type_of_Loan <- NULL
test_1_processed$Type_of_Loan <- NULL

# Kendall's Tau for the "Credit_Mix" variable:
tau_Credit_Mix <- Kendall(Dados$Credit_Score, Dados$Credit_Mix)
cat("Kendall's Tau for Credit_Score and Credit_Mix:\n")
print(tau_Credit_Mix)
# Credit_Score e Credit_Mix (tau = -0.433, 2-sided pvalue =< 2.22e-16)

# Kendall's Tau for the "Age_Group" variable:
tau_Age_Group <- Kendall(Dados$Credit_Score, Dados$Age_Group)
cat("Kendall's Tau for Credit_Score and Age_Group:\n")
print(tau_Age_Group)
# Result: Credit_Score and Age_Group (tau = -0.0264, 2-sided pvalue =1.6042e-11):

tau_Month <- Kendall(Dados$Credit_Score, Dados$Month)
cat("Kendall's Tau for Credit_Score and Month:\n")
print(tau_Month)
# Result: Credit_Score and Month (tau = -0.0328, 2-sided pvalue =< 2.22e-16):

train_1_processed$Age_Group <- NULL # Low correlation with Credit_Score
train_1_processed$Credit_Mix <- NULL # Medium-low correlation with Credit_Score
train_1_processed$Month <- NULL # Low correlation with Credit_Score
test_1_processed$Age_Group <- NULL # Low correlation with Credit_Score
test_1_processed$Credit_Mix <- NULL # Medium-low correlation with Credit_Score
test_1_processed$Month <- NULL # Low correlation with Credit_Score


### DATA MODELING ###
# Calculation of proportions for "Poor", "Good", and "Standard" in the target variable (Credit_Score):
prop.table(table(train_1_processed$Credit_Score)) %>% round(4)

# Graphical representation of the target variable (Credit_Score):
train_1_processed$Credit_Score <- as.factor(train_1$Credit_Score)

# Create the plot for Credit_Score
plot(train_1_processed$Credit_Score, 
     main = "Distribuição da Variável Credit_Score", 
     ylim = c(0, 30000),  
     xlab = "Níveis da variável Credit_Score", 
     ylab = "Nº de Observações", 
     col = "blue")

# Undersampling of the training data:
set.seed(100)
# The target variable is number 19, we use [[]] on y because the variable is a tibble and we want a vector:
train_down <- downSample(x = train_1_processed[,-19], y = train_1_processed[[19]])

# Checking the new structure of downSample:
str(train_down)
print(table(train_down$Class))

# Renaming 'Class' to 'Credit_Score':
colnames(train_down)[colnames(train_down) == "Class"] <- "Credit_Score"

# Graphical representation of the target variable (y) with the balanced training set data (train_down):
plot(train_down$Credit_Score,main = "Variável Alvo (y) - Dados de Treino após Operação de Equilíbrio (under-sampling)",
     xlab = "Valores da variável Credit Score", ylab = "nº de observações",col = "blue")

# Oversampling of the training data:
# The target variable is number 24, we use [[]] on 'y' because the variable is a tibble and we want a vector:
train_up <- upSample(x = train_1_processed[,-19], y = train_1_processed[[19]])

# Checking the new structure of upSample:
str(train_up)
print(table(train_up$Class)) #Good=17782

# Renaming 'Class' to 'Credit_Score':
colnames(train_up)[colnames(train_up) == "Class"] <- "Credit_Score"

# Representação gráfica da variável alvo (y) com os dados do conjunto train após o equilíbrio (train_up)
plot(train_up$Credit_Score, main = "Variável Alvo (y) - Dados de Treino após Operação de Equilíbrio (over-sampling)",
     xlab = "Valores da variável Credit Score", ylab = "nº de observações", col = "blue")


# Implementation of the models:
# Decision trees: Creation of the decision tree model using the training data (train_down):
model_tree_train <- rpart(formula = Credit_Score ~ ., data = train_down, method = "class",
                          control = rpart.control(minsplit = 2))
model_tree_train

# Graphical representation of the decision tree:
prp(model_tree_train, main="Decision Tree for Credit Evaluation", type=2, extra=104, fallen.leaves=TRUE, roundint=FALSE)
plot(as.party(model_tree_train), main = "Decision Tree for Credit Evaluation")
rpart.rules(model_tree_train, roundint = FALSE)

# Predictions on the training set:
train_predictions_DT <- predict(model_tree_train, newdata = train_down, type = "class")

# Confusion matrix for the training data:
conf_matrix_train_DT <- confusionMatrix(train_predictions_DT, train_down$Credit_Score)
print(conf_matrix_train_DT)

# Calculate the sensitivity for the training set:
conf_matrix <- matrix(c(5124, 1161, 1643, 
                        159, 4379, 1410, 
                        690, 433, 2920),
                      nrow = 3, byrow = TRUE)

# Calculation of True Positives:
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

# Calculate the specificity for the training set:
conf_matrix <- matrix(c(5124, 1161, 1643,
                        159,  4379, 1410,
                        690,   433, 2920), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP)
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP)
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN)
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN)
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate the overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)
joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("Overall (Joint) Specificity:", round(joint_specificity, 4)))

# AUC for the training data:
train_probs_DT <- predict(model_tree_train, newdata = train_down, type = "prob")
(multiclass_auc_train_DT <- multiclass.roc(train_down$Credit_Score, train_probs_DT))

# Summary of the created Decision Tree model (model_tree), allows determining the total number of nodes in the tree:
model_tree_party<-as.party(model_tree_train)
model_tree_party

# Creation of the decision tree for the test set:
model_tree_test<-rpart(formula = Credit_Score~., data=test_1_processed, method="class",
                  control=rpart.control(minsplit = 2))
model_tree_test

# Graphical representation of the decision tree for the test set:
prp(model_tree_test, main="Decision Tree for Credit Evaluation", type=2, extra=104, fallen.leaves=TRUE, roundint=FALSE)
plot(as.party(model_tree_test), main = "Decision Tree for Credit Evaluation")
rpart.rules(model_tree_test, roundint = FALSE)

# Predictions on the test set:
test_predictions_DT <- predict(model_tree_test, newdata = test_1_processed, type = "class")

# Creation of the confusion matrix for the test set:
conf_matrix_test_DT <- confusionMatrix(test_predictions_DT, test_1_processed$Credit_Score)
print(conf_matrix_test_DT)

# Calculating the sensitivity for the test set:
conf_matrix <- matrix(c(1958, 622, 1435, 
                        42, 2731, 1064, 
                        941, 1431, 6274),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

### Calculation of specificity for the test set:
conf_matrix <- matrix(c(1958, 622, 1435,
                        42,  2731, 1064,
                        941,   1431, 6274), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

# Print the results:
print(paste("Overall Specificity (Joint Specificity):", round(joint_specificity, 4)))

# AUC for the test data:
test_probs_DT <- predict(model_tree_test, newdata = test_1_processed, type = "prob")
(multiclass_auc_test_DT <- multiclass.roc(test_1_processed$Credit_Score, test_probs_DT))

# Neural Networks
# Step 1: Definition of cross-validation parameters
train_control <- trainControl(
  method = "repeatedcv",  # Repeated cross-validation method
  number = 2,            # Number of folds
  repeats = 1,            # Number of cross-validation repeats
  classProbs = TRUE,      # Class probabilities
  summaryFunction = multiClassSummary # Metric for multi-class evaluation
)

# Step 2: Train the neural network model
set.seed(100)
NN_model <- train(
  Credit_Score ~ .,             # Credit_Score is the dependent variable (target)
  data = train_down,            # Training data
  method = "nnet",              # Neural network method
  trControl = train_control,    # Training control
  linout = TRUE,                # For regression problems (not needed here if it's classification)
  trace = FALSE,                # Disable messages during training
  tuneLength = 5                # Number of attempts to optimize the model
)

# Step 3: Predictions and evaluation
# Predictions on the training set:
NN_train_predictions <- predict(NN_model, newdata = train_down) # Predictions for training data
confMat_train_NN <- confusionMatrix(NN_train_predictions, train_down$Credit_Score)
print(confMat_train_NN)

# Calculate sensitivity for the training set:
conf_matrix <- matrix(c(4902, 778, 1187, 
                        181, 4534, 1250, 
                        890, 661, 3536),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

# Calculate specificity for the training set:
conf_matrix <- matrix(c(4902, 778, 1187,
                        181,  4534, 1250,
                        890,   661, 3536), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity (joint specificity):
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

# Print the results:
print(paste("Overall Specificity (Joint Specificity):", round(joint_specificity, 4)))

# Predictions on the test set:
NN_test_predictions <- predict(NN_model, newdata = test_1_processed) # Predictions for test data
confMat_test_NN <- confusionMatrix(NN_test_predictions, test_1_processed$Credit_Score)
print(confMat_test_NN)

### Calculate sensitivity for the test set
conf_matrix <- matrix(c(2376, 618, 1817, 
                        127, 3533, 1920, 
                        438, 633, 5036),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

# Visualize the confusion matrix - Training data:
train_cm_NN <- as.data.frame(confMat_train_NN$table)
ggplot(train_cm_NN, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix: NN (Training)", x = "Predicted", y = "Actual")

### Calculate specificity for the test set:
conf_matrix <- matrix(c(2376, 618, 1817,
                        127,  3533, 1920,
                        438,   633, 5036), 
                      nrow = 3, byrow = TRUE)
# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity (joint specificity):
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("Overall Specificity (Joint Specificity):", round(joint_specificity, 4)))


# Visualization of the confusion matrix - Test data:
test_cm_NN <- as.data.frame(confMat_test_NN$table)
ggplot(test_cm_NN, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix: NN (Test)", x = "Predicted", y = "Actual")

# Step 4: Calculate AUC:
train_probs_NN <- predict(NN_model, train_down, type = "prob")
(multiclass_auc_train_NN <- multiclass.roc(train_down$Credit_Score, train_probs_NN))
test_probs_NN <- predict(NN_model, test_1_processed, type = "prob")
(multiclass_auc_test_NN <- multiclass.roc(test_1_processed$Credit_Score, test_probs_NN))

print(NN_model)
plot(NN_model, rep="best")


### SELECTION OF THE RELEVANT FEATURES ###

# Defining the control for RFE with cross-validation. The 5 is the number of stratified cross-validations:
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# RFE on the scaled dataset with downsampling. The sizes represent the number of features we want to evaluate (in this case, I tested from 5 to 15):
rfe_results <- rfe(x = train_down[, !(names(train_down) %in% c("Credit_Score"))],
                   y = train_down$Credit_Score,
                   sizes = c(5:15),
                   rfeControl = ctrl)

# Accuracy results, and the most important variables. We can see that the optimal number of columns to consider is 15:
print(rfe_results)

# Most important columns:
print(rfe_results$optVariables)

# To see the importance of each column for the model, we use "Overall" as the criterion:
unique_ranked_variables <- rfe_results$variables %>%
  arrange(desc(Overall)) %>%
  distinct(var, .keep_all = TRUE)
print(unique_ranked_variables)

# Since RFE produced different results for each group member, we decided to create an auxiliary table that includes all the variables that were commonly identified as the most important:
data_list <- list(train_down = train_down, test_1_processed = test_1_processed)

# Selected most important variables:
selected_vars <- c(
  "Num_Credit_Card",
  "Delay_from_due_date",
  "Interest_Rate",
  "Total_EMI_per_month",
  "Num_of_Delayed_Payment",
  "Annual_Income",
  "Outstanding_Debt",
  "Changed_Credit_Limit",
  "Monthly_Inhand_Salary",
  "Num_Credit_Inquiries",
  "Credit_MixStandard",
  "Num_Bank_Accounts",
  "Age",
  "Credit_MixGood",
  "Credit_Score"
)
data_list <- lapply(data_list, function(x) select(x, all_of(selected_vars)))

# Train and test sets with only the most important columns + the target variable:
train_down_rfe <- data_list$train_down
test_1_processed_rfe <- data_list$test_1_processed
# NOTE: The following train and test sets, containing only the most important features, will be used for model training and evaluation.

# Study of the correlation of numerical variables:
numeric_columns_rfe <- train_down_rfe %>%
  select(where(is.numeric)) %>%
  select(-Credit_MixGood, -Credit_MixStandard)

# Calculating the correlation matrix:
correlation <- cor(numeric_columns_rfe, use = "complete.obs", method = "spearman") 

# Visual adjustments and creation of the corrplot:
par(oma = c(1, 1, 1, 1))  
corrplot.mixed(
  correlation,
  order = "hclust",  
  tl.pos = "lt",     
  upper = "ellipse", 
  number.cex = 1.1,       
  tl.col = "black",
  cl.cex = 0.9,
  tl.cex = 0.9
)
mtext("Correlation Matrix - Spearman", side = 3, line = 2, cex = 2, font = 2, adj = 0.5)

# Excluding the column Monthly_Inhand_Salary, considering the results from the RFE and the corrplot:
train_down_rfe <- train_down_rfe %>% select(-Monthly_Inhand_Salary)
test_1_processed_rfe <- test_1_processed_rfe %>% select(-Monthly_Inhand_Salary)


### DATA MODELING (USING THE RELEVANT COLUMNS ONLY) ###

# Logistic regression with Grid Search:
# Step 1: Set up training control with cross-validation:
train_control_log <- trainControl(
  method = "cv",               # Cross-validation:
  number = 10,                 # 10 folds:
  classProbs = TRUE,           # Enable class probabilities:
  summaryFunction = multiClassSummary # Evaluation metric:
)

# Step 2: Create a hyperparameter grid for Grid Search:
grid_log <- expand.grid(decay = c(0.01, 0.1, 0.5)) # Penalty to avoid overfitting:

# Step 3: Train the logistic regression model with grid search:
set.seed(123)
model_log_grid <- train(
  Credit_Score ~ ., 
  data = train_down_rfe,  # Use the train_down_rfe dataset:
  method = "multinom",  # Using multinomial logistic regression:
  trControl = train_control_log, 
  tuneGrid = grid_log
)

# Step 4: Model Evaluation:
print(model_log_grid)

# Predictions and Confusion Matrix - Training Data:
train_predictions_RL <- predict(model_log_grid, newdata = train_down_rfe)
conf_matrix_train_RL <- confusionMatrix(train_predictions_RL, train_down_rfe$Credit_Score)
print(conf_matrix_train_RL)

## Sensitivity calculation for the training dataset:
conf_matrix <- matrix(c(4877, 939, 1248, 
                        158, 4042, 1199, 
                        938, 992, 3526),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Specificity calculation for the training dataset:
conf_matrix <- matrix(c(4877, 939, 1248,
                        158,  4042, 1199,
                        938,   992, 3526), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

# Print results:
print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# Predictions and Confusion Matrix - Test Data:
test_predictions_RL <- predict(model_log_grid, newdata = test_1_processed_rfe)
conf_matrix_test_RL <- confusionMatrix(test_predictions_RL, test_1_processed_rfe$Credit_Score)
print(conf_matrix_test_RL)

## Sensitivity calculation for the test dataset:
conf_matrix <- matrix(c(2374, 728, 1913, 
                        81, 3210, 1796, 
                        486, 846, 5064),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Specificity calculation for the test dataset:
conf_matrix <- matrix(c(2374, 728, 1913,
                        81,  3210, 1796,
                        486,   846, 5064), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# AUC for the data:
train_probs_RL <- predict(model_log_grid, newdata = train_down_rfe, type = "prob")
(multiclass_auc_train_RL <- multiclass.roc(train_down_rfe$Credit_Score, train_probs_RL))
test_probs_RL <- predict(model_log_grid, newdata = test_1_processed_rfe, type = "prob")
(multiclass_auc_RL <- multiclass.roc(test_1_processed_rfe$Credit_Score, test_probs_RL))


# Random Forest with Grid Search
# Step 1: Configure training control with cross-validation:
cv.control <- trainControl(
  method = "cv",                # Cross-validation
  number = 10,                  # Number of folds (10-fold cross-validation)
  savePredictions = "final",    # Save the predictions of each fold
  classProbs = TRUE,            # Enable class probabilities
  summaryFunction = multiClassSummary # Evaluate metrics for multiple classes
)

# Step 2: Create hyperparameter grid for Grid Search:
grid_rf <- expand.grid(
  mtry = c(3, 4, 5),           # Number of variables to consider
  splitrule = "gini",          # Split criterion
  min.node.size = c(1, 3, 5)   # Minimum node size
)

# Step 3: Train the random forest model with grid search:
set.seed(123)
model_rf_grid <- train(
  Credit_Score ~ ., 
  data = train_down_rfe,        # Use the train_down_rfe dataset
  method = "ranger", 
  trControl = cv.control,
  tuneGrid = grid_rf,
  num.trees = c(100),           # Number of trees in the forest
  max.depth = 8                 # Maximum depth of the trees
)

# Step 4: Evaluation:
print(model_rf_grid)

# Predictions and Confusion - Training Data:
train_predictions_FA <- predict(model_rf_grid, newdata = train_down_rfe)
conf_matrix_train_FA <- confusionMatrix(train_predictions_FA, train_down_rfe$Credit_Score)
print(conf_matrix_train_FA)

## Calculation of sensitivity for the training set:
conf_matrix <- matrix(c(4934, 688, 1140, 
                        164, 4844, 1207, 
                        875, 441, 3626),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Calculation of specificity for the training set:
conf_matrix <- matrix(c(4934, 688, 1140,
                        164,  4844, 1207,
                        875,   441, 3626), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

# Print the results:
print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# AUC for the training data:
train_probs_FA <- predict(model_rf_grid, newdata = train_down_rfe, type = "prob")
(multiclass_auc_train_FA <- multiclass.roc(train_down_rfe$Credit_Score, train_probs_FA)) # AUC for multiple classes

# Predictions and Confusion - Test Data:
test_predictions_FA <- predict(model_rf_grid, newdata = test_1_processed_rfe)
conf_matrix_test_FA <- confusionMatrix(test_predictions_FA, test_1_processed_rfe$Credit_Score)
print(conf_matrix_test_FA)

## Calculation of sensitivity for the test set:
conf_matrix <- matrix(c(2367, 571, 1773, 
                        119, 3772, 2048, 
                        455, 441, 4952),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Calculation of specificity for the test set:
conf_matrix <- matrix(c(2367, 571, 1773,
                        119,  3772, 2048,
                        455,   441, 4952), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# AUC for the test data:
test_probs_FA <- predict(model_rf_grid, newdata = test_1_processed_rfe, type = "prob")
(multiclass_auc_test_FA <- multiclass.roc(test_1_processed_rfe$Credit_Score, test_probs_FA)) # AUC for multiple classes

# Best model parameters:
print(model_rf_grid$bestTune)
print(model_rf_grid$finalModel)


# Support Vector Machine Model (SVM - Polynominal kernel)
# Step 1: Train the SVM model:
SVModel <- train(Credit_Score ~ ., data = train_down_rfe,
                 method = "svmPoly",
                 trControl= train_control,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1),
                 na.action = na.omit)

# Step 2: Predictions and evaluation
# Predictions on the training set:
SVM_train_predictions <- predict(SVModel, newdata = train_down_rfe) # Predictions for the training data
confMat_train_SVM <- confusionMatrix(SVM_train_predictions, train_down_rfe$Credit_Score)
print(confMat_train_SVM)

# Visualize the confusion matrix - Training data:
train_cm_SVM <- as.data.frame(confMat_train_SVM$table)
ggplot(train_cm_SVM, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix: SVM (Training)", x = "Predicted", y = "Actual")

## Calculate sensitivity for the training set:
conf_matrix <- matrix(c(5014, 1034, 1371, 
                        124, 4213, 1224, 
                        835, 726, 3378),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Calculate specificity for the training set:
conf_matrix <- matrix(c(5014, 1034, 1371,
                        124,  4213, 1224,
                        835,   726, 3378), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# Predictions on the test set:
SVM_test_predictions <- predict(SVModel, newdata = test_1_processed_rfe) # Predictions for the test data
confMat_test_SVM <- confusionMatrix(SVM_test_predictions, test_1_processed_rfe$Credit_Score)
print(confMat_test_SVM)

# Visualize the confusion matrix - Test data:
test_cm_SVM <- as.data.frame(confMat_test_SVM$table)
ggplot(test_cm_SVM, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix: SVM (Test)", x = "Predicted", y = "Actual")

## Calculate sensitivity for the test set:
conf_matrix <- matrix(c(2433, 800, 2125, 
                        78, 3345, 1860, 
                        430, 639, 4788),
                      nrow = 3, byrow = TRUE)

# Extract true positives (diagonal elements):
true_positives <- diag(conf_matrix)

# Calculate false negatives for each class (row sums minus diagonal):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate total true positives and false negatives:
total_true_positives <- sum(true_positives)
total_false_negatives <- sum(false_negatives)

# Calculate joint sensitivity:
joint_sensitivity <- total_true_positives / (total_true_positives + total_false_negatives)

# Print joint sensitivity:
joint_sensitivity

## Calculate specificity for the test set:
conf_matrix <- matrix(c(2433, 800, 2125,
                        78,  3345, 1860,
                        430,   639, 4788), 
                      nrow = 3, byrow = TRUE)

# Calculate True Positives (TP):
true_positives <- diag(conf_matrix)

# Calculate False Positives (FP):
false_positives <- colSums(conf_matrix) - true_positives

# Calculate False Negatives (FN):
false_negatives <- rowSums(conf_matrix) - true_positives

# Calculate True Negatives (TN):
total_samples <- sum(conf_matrix)
true_negatives <- total_samples - (true_positives + false_positives + false_negatives)

# Calculate overall specificity:
total_fp <- sum(false_positives)
total_tn <- sum(true_negatives)

joint_specificity <- total_tn / (total_tn + total_fp)

print(paste("General Specificity (Joint Specificity):", round(joint_specificity, 4)))

# Step 3: Calculate AUC:
train_probs_SVM <- predict(SVModel, train_down_rfe, type = "prob")
(multiclass_auc_train_SVM <- multiclass.roc(train_down_rfe$Credit_Score, train_probs_SVM))
test_probs_SVM <- predict(SVModel, test_1_processed_rfe, type = "prob")
(multiclass_auc_test_SVM <- multiclass.roc(test_1_processed_rfe$Credit_Score, test_probs_SVM))

# Best model parameters:
print(SVModel$bestTune)
print(SVModel$finalModel)
