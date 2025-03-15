# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Import of auxiliary and cleaning scripts:
source("aux_functions.R")


# Data cleaning process:
Dados <- Dados %>%
  # Remove unwanted characters and convert data to numeric values:
  mutate(
    Age = as.numeric(gsub("_", "", Age)),
    Annual_Income = round(as.numeric(gsub("_", "", Annual_Income)), digits = 2),
    Monthly_Inhand_Salary = round(as.numeric(gsub("_", "", Dados$Monthly_Inhand_Salary)), digits = 2),
    Num_Bank_Accounts = as.numeric(gsub("_", "", Num_Bank_Accounts)),
    Num_of_Loan = as.numeric(gsub("_", "", Num_of_Loan)),
    # Since the variable is expressed as a percentage, we divide by 100 to normalize the data:
    Changed_Credit_Limit = round(as.numeric(gsub("_", "", Changed_Credit_Limit)), digits = 2) / 100,
    Num_of_Delayed_Payment = as.numeric(gsub("_", "", Num_of_Delayed_Payment)),
    Credit_Mix = gsub("_", "", Credit_Mix),
    Outstanding_Debt = as.numeric(gsub("_", "", Outstanding_Debt)),
    Delay_from_due_date = as.numeric(Delay_from_due_date),
    Num_Credit_Inquiries = as.numeric(Num_Credit_Inquiries),
    Credit_History_Age = (as.numeric(str_extract(Credit_History_Age, "\\d+")) * 365) + 
      (as.numeric(str_extract(str_extract(Credit_History_Age, "and \\d+"), "\\d+")) * 30),
    # We assume only the categories "No" and "Yes" as the negation of both means lack of information. Thus, we opted for one-hot encoding and integrated the new columns into the Dados dataset.
    Payment_of_Min_Amount = case_when(
      Payment_of_Min_Amount == "Yes" ~ 1,
      Payment_of_Min_Amount == "No" ~ 0,
      Payment_of_Min_Amount == "NM" ~ 2,
      TRUE ~ NA_real_),
    Total_EMI_per_month = round(Total_EMI_per_month, digits = 2),
    Amount_invested_monthly = gsub("__10000__", "", Amount_invested_monthly),
    Amount_invested_monthly = as.numeric(Amount_invested_monthly),
    Amount_invested_monthly = round(Amount_invested_monthly, digits = 2),
    Payment_Behaviour = gsub("!@9#%8", "", Payment_Behaviour),
    Payment_Behaviour = ifelse(Payment_Behaviour == "", NA, Payment_Behaviour),
    Monthly_Balance = ifelse(Monthly_Balance == "", NA, Monthly_Balance),
    Monthly_Balance = ifelse(grepl("_", Monthly_Balance), NA, Monthly_Balance),
    Monthly_Balance = as.numeric(Monthly_Balance),
    Monthly_Balance = round(Monthly_Balance, digits = 2),
    Num_Credit_Card = as.numeric(Num_Credit_Card),
    ID = as.numeric(ID)
  )

# Handle missing/incorrect values in Occupation and Type_of_Loan by Customer_ID (groupby should not be inside mutate):
Dados <- Dados %>%
  group_by(Customer_ID) %>%
  mutate(Occupation = ifelse(Occupation == "_______", first(Occupation[Occupation != "_______"]), Occupation)) %>%
  mutate(Occupation = ifelse(Occupation == "", first(Occupation[Occupation != ""]), Occupation)) %>%
  mutate(Type_of_Loan = ifelse(Type_of_Loan == "", first(Type_of_Loan[Type_of_Loan != ""]), Type_of_Loan)) %>%
  ungroup()

# Count the number of entries with Type_of_Loan as "Not Specified":
Not_Specified_TOL1 <- Dados %>%
  filter(Type_of_Loan == "Not Specified") %>%
  nrow()

# Verify that there are no different values for Occupation for the same Customer_ID:
Dados %>%
  group_by(Customer_ID) %>%
  filter(n_distinct(Occupation) > 1) %>%
  ungroup()

# Remove unnecessary columns like Name, since Customer_ID contains all the information and will be used for later analysis:
Dados$Name <- NULL
Dados$SSN <- NULL


# Month column
# Sort unique values for Month:
sort(unique(Dados$Month))

# Labeling months as numeric values:
Dados <- Dados %>%
  mutate(Month = case_when(
    Month == "January" ~ "1",
    Month == "February" ~ "2",
    Month == "March" ~ "3",
    Month == "April" ~ "4",
    Month == "May" ~ "5",
    Month == "June" ~ "6",
    Month == "July" ~ "7",
    Month == "August" ~ "8"
  ))

# Confirm that the months have been correctly labeled:
unique(Dados$Month)

# Convert Month to numeric values:
Dados$Month = as.numeric(Dados$Month)


# Age column
# The lower limit is defined as 18 (BPI/ActivoBank), as this is the minimum age for a loan request in America.
# For the upper limit, based on data analysis, this seems to be the most realistic upper limit (the age limit is usually 70 in BPI/ActivoBank, but I couldn't find anything about a maximum age limit in the US).
# The use of the multiplier does not seem to apply in this case, as defining it as 2 (which is already high),
# the upper limit would be set at 68 (which is low), causing the maximum age in the dataset to be 56.
Age_lower_bound <- 18
Age_upper_bound <- 95
Age_q1 <- quantile(Dados$Age, 0.25, na.rm = TRUE)
Age_median <- median(Dados$Age, na.rm = TRUE)
Age_q3 <- quantile(Dados$Age, 0.75, na.rm = TRUE)
Age_mean <- mean(Dados$Age, na.rm = TRUE)

# Visualization of outliers using the function defined above:
Age_outliers <- plot_outliers(Dados, "Age", Age_lower_bound, Age_upper_bound, "Age Outliers")

# Analysis of outliers through the dataframe:
(outliers_df_Age <- find_outliers(Dados, "Age", Age_lower_bound, Age_upper_bound))

# Counting outliers:
Age_outliers_count <- Dados %>%
  filter(Age < Age_lower_bound | Age > Age_upper_bound) %>%
  nrow()
(Age_outliers_count)

# Replacing outliers with the first non-outlier value associated with the group:
Dados <- replace_outliers(Dados, "Customer_ID", "Age", Age_lower_bound, Age_upper_bound)

# Recounting outliers after replacement:
Age_outliers_recount <- Dados %>%
  filter(Age < Age_lower_bound | Age > Age_upper_bound) %>%
  nrow()
(Age_outliers_recount)

# Visualization of outliers again:
Age_outliers <- plot_outliers(Dados, "Age", Age_lower_bound, Age_upper_bound, "Age Outliers")

# Analysis of outliers through the dataframe again:
(outliers_df_Age <- find_outliers(Dados, "Age", Age_lower_bound, Age_upper_bound))

# Calculation of median age:
median_age <- median(Dados$Age[Dados$Age >= Age_lower_bound & Dados$Age <= Age_upper_bound], na.rm = TRUE)

# Replacing outliers with the median:
Dados <- Dados %>%
  mutate(Age = ifelse(Age < Age_lower_bound | Age > Age_upper_bound, median_age, Age))

# Creating a new dataframe to represent the data without outliers, and visualizing the distribution of the variable after update,
# identifying "acceptable" outliers, based on the limits we defined earlier, and displaying these limits, Q1, median, and Q3:
new_age_data_frame <- data.frame(Age = Dados$Age)

# Updating values for mean, median, Q1, and Q3:
Age_q1 <- quantile(Dados$Age, 0.25, na.rm = TRUE)
Age_median <- median(Dados$Age, na.rm = TRUE)
Age_q3 <- quantile(Dados$Age, 0.75, na.rm = TRUE)
Age_mean <- mean(Dados$Age, na.rm = TRUE)

# Creating the box plot graphic representation:
Age_boxplot <- plot_boxplot(new_age_data_frame, "Age", Age_lower_bound, Age_upper_bound, 
                            Age_q1, Age_median, Age_mean, Age_q3, 
                            title = "Box Plot of Age without Outliers")

# Creating the histogram graphic representation:
Age_histogram <- plot_histogram(Dados, "Age", binwidth = 2, title = "Histogram of Age", overlay_normal = TRUE)

# Creating age groups:
Dados <- Dados %>%
  mutate(Age_Group = case_when(
    Age >= 18 & Age <= 25 ~ "18-25",
    Age >= 26 & Age <= 35 ~ "26-35",
    Age >= 36 & Age <= 45 ~ "36-45",
    Age >= 46 & Age <= 55 ~ "46-55",
    Age >= 56 & Age <= 65 ~ "56-65",
    Age > 65 ~ "65+"
  ))

# Creating bar plot for age groups:
Age_barplot <- plot_bar(Dados, "Age_Group", "Count of Individuals by Age Group")

# Converting Age_Group to numeric ordinal values:
Dados <- Dados %>%
  mutate(
    Age_Group = case_when(
      Age_Group == "18-25" ~ 1,
      Age_Group == "26-35" ~ 2,
      Age_Group == "36-45" ~ 3,
      Age_Group == "46-55" ~ 4,
      Age_Group == "56-65" ~ 5,
      Age_Group == "65+" ~ 6
    )
  )

# Converting Age_Group to numeric ordinal:
Dados$Age_Group <- as.numeric(factor(Dados$Age_Group, levels = c("1", "2", "3", "4", "5", "6")))

#  Occupation column:
# Counting NA values (56), as there were no available Occupation values in other rows to replace the field with "_______" (56 NA entries)
(na_count_Occupation <- sum(is.na(Dados$Occupation)))
na_Occupation <- Dados %>%
  filter(is.na(Occupation))
(na_Occupation)

# Replacing NA with "Not Specified":
Dados <- Dados %>%
  mutate(Occupation = replace_na(Occupation, "Not_Specified"))

# One-hot encoding for Occupation variable and integrating new columns into the dataset:
dummy <- dummyVars(" ~ Occupation", data = Dados)
Occupation_df <- data.frame(predict(dummy, newdata = Dados))
Dados <- cbind(Dados, Occupation_df)
Dados$OccupationNot_Specified <- NULL


#  Annual Income column
# Counting NA values (0), as there were no available Occupation values in other rows to replace the field with "_":
(na_count_Annual_Income <- sum(is.na(Dados$Annual_Income)))
na_Annual_Income <- Dados %>%
  filter(is.na(Annual_Income))
(na_Annual_Income) # entries
(na_count_Annual_Income) # number of NAs

# IQR (2861 outliers):
AI_lower_bound <- calculate_lower_bound(Dados, "Annual_Income", multiplier = 1.5)
AI_upper_bound <- calculate_upper_bound(Dados, "Annual_Income", multiplier = 1.5)

# Function to visualize outliers:
AI_plot_outliers <- plot_outliers(Dados, "Annual_Income", AI_lower_bound, AI_upper_bound, "AI Outliers") 
print(AI_plot_outliers)

# Quartiles:
AI_q1 <- quantile(Dados$Annual_Income, 0.25, na.rm = TRUE)
AI_q3 <- quantile(Dados$Annual_Income, 0.75, na.rm = TRUE)

# Annual Income mean:
AI_mean <- mean(Dados$Annual_Income)

# Calculating median for Annual Income:
AI_median <- median(Dados$Annual_Income[Dados$Annual_Income >= AI_lower_bound 
                                        & Dados$Annual_Income <= AI_upper_bound], na.rm = TRUE)

# Replacing outliers with median:
Dados <- replace_outliers_with_median(Dados, "Occupation", "Annual_Income", AI_lower_bound, AI_upper_bound, AI_median)

# Annual Income mean after outlier correction:
AI_mean_corrected <- mean(Dados$Annual_Income)

# BoxPlot for Annual Income.
AI_boxplot <- plot_boxplot(Dados, "Annual_Income", AI_lower_bound, AI_upper_bound, AI_q1, AI_median, AI_mean, AI_q3, "AI Boxplot")

#  Monthly Inhand Salary column:
# Counting NA values (7523 NA entries):
(na_count_MIS <- sum(is.na(Dados$Monthly_Inhand_Salary)))
na_rows_MIS <- Dados %>%
  filter(is.na(Monthly_Inhand_Salary))

# IQR for Monthly Inhand Salary (2861 outliers):
MIS_lower_bound <- calculate_lower_bound(Dados, "Monthly_Inhand_Salary")
MIS_upper_bound <- calculate_upper_bound(Dados, "Monthly_Inhand_Salary")

# Replacing missing values with the median salary of the same occupation:
Dados <- replace_na_with_median(Dados, "Occupation", "Monthly_Inhand_Salary", MIS_lower_bound, MIS_upper_bound)

# Function to visualize outliers:
MIS_plot_outliers <- plot_outliers(Dados, "Monthly_Inhand_Salary", MIS_lower_bound, MIS_upper_bound, "MIS Outliers")
print(MIS_plot_outliers)

# Quartiles for Monthly Inhand Salary:
MIS_q1 <- quantile(Dados$Monthly_Inhand_Salary, 0.25, na.rm = TRUE)
MIS_q3 <- quantile(Dados$Monthly_Inhand_Salary, 0.75, na.rm = TRUE)

# Monthly Inhand Salary mean:
MIS_mean <- mean(Dados$Monthly_Inhand_Salary, na.rm = TRUE)

# Calculating median for Monthly Inhand Salary:
MIS_median <- median(Dados$Monthly_Inhand_Salary[Dados$Monthly_Inhand_Salary >= MIS_lower_bound 
                                                 & Dados$Monthly_Inhand_Salary <= MIS_upper_bound], na.rm = TRUE)

# Replacing outliers with median (should it be by age or occupation? Or calculated by AI?):
Dados <- replace_outliers_with_median(Dados, "Occupation", "Monthly_Inhand_Salary", MIS_lower_bound, MIS_upper_bound, MIS_median)

# BoxPlot for Monthly Inhand Salary:
MIS_boxplot <- plot_boxplot(Dados, "Monthly_Inhand_Salary", MIS_lower_bound, MIS_upper_bound, MIS_q1, MIS_median, MIS_mean, MIS_q3, "MIS Boxplot")

#  Num_Bank_Accounts column:
# Counting NA values (0):
(na_count_NBA <- sum(is.na(Dados$Num_Bank_Accounts)))

# Counting empty string values (0):
Null_NBA <- Dados %>%
  filter(Num_Bank_Accounts == "") %>%
  nrow()

# Unique values (553 between -1 and 1798, no legal maximum, but at least one is required for a loan):
unique_NBA <- Dados %>%
  select(Num_Bank_Accounts) %>%  
  distinct() %>%  
  arrange(desc(Num_Bank_Accounts)) %>%  
  head(553) 
unique_NBA

# Mean NBA with original values (including negatives/nulls):
NBA_mean <- mean(Dados$Num_Bank_Accounts)

# IQR, considering lower bound as 1 because at least one bank account is required for a loan:
NBA_lower_bound <- 1
NBA_upper_bound <- calculate_upper_bound(Dados, "Num_Bank_Accounts", multiplier = 1.5)

# Function to visualize outliers:
NBA_plot_outliers <- plot_outliers(Dados, "Num_Bank_Accounts", NBA_lower_bound, NBA_upper_bound, "NBA Outliers")

# Quartiles
NBA_q1 <- quantile(Dados$Num_Bank_Accounts, 0.25, na.rm = TRUE)
NBA_q3 <- quantile(Dados$Num_Bank_Accounts, 0.75, na.rm = TRUE)

# Mean NBA replacing -1/0 with 1:
mean_NBA <- mean(Dados$Num_Bank_Accounts)

# Calculating median NBA:
NBA_median <- median(Dados$Num_Bank_Accounts[Dados$Num_Bank_Accounts >= NBA_lower_bound 
                                             & Dados$Num_Bank_Accounts <= NBA_upper_bound], na.rm = TRUE)

# NBA histogram:
NBA_plot_histogram <- plot_histogram(Dados, "Num_Bank_Accounts", 1, "Frequency NBA", TRUE, c(1, 50))

# NBA frequency table (47154/50000 entries in the top 10 most frequent values):
NBA_tabela_freq <- Dados %>%
  count(Num_Bank_Accounts) %>%
  arrange(desc(n)) 
print(NBA_tabela_freq)

# Replacing outliers with median:
Dados <- Dados %>%
  mutate(Num_Bank_Accounts = ifelse(Num_Bank_Accounts < NBA_lower_bound | Num_Bank_Accounts > NBA_upper_bound, NBA_median, Num_Bank_Accounts))

# NBA BoxPlot:
NBA_boxplot <- plot_boxplot(Dados, "Num_Bank_Accounts", NBA_lower_bound, NBA_upper_bound, NBA_q1, NBA_median, NBA_mean, NBA_q3, "NBA Boxplot")
summary(Dados$Num_Bank_Accounts)


#  Num_Credit_Card column
# Counting NA values (0):
(na_count_NCC <- sum(is.na(Dados$Num_Credit_Card)))

# Counting empty string values (0):
Null_NCC <- Dados %>%
  filter(Num_Credit_Card == "") %>%
  nrow()
print(Null_NCC)

# Unique values (553, same as unique_NBA without corrections, no legal maximum, 1499 is unrealistic but possible, lowest value is 0):
summary(Dados$Num_Credit_Card)

unique_NCC <- Dados %>%
  select(Num_Credit_Card) %>%  
  distinct() %>%  
  arrange(desc(Num_Credit_Card))  
print(head(unique_NCC, 553))

#  IQR:
NCC_lower_bound <- calculate_lower_bound(Dados, "Num_Credit_Card", multiplier = 1.5)
NCC_upper_bound <- calculate_upper_bound(Dados, "Num_Credit_Card", multiplier = 1.5)

# Function to visualize outliers:
NCC_plot_outliers <- plot_outliers(Dados, "Num_Credit_Card", NCC_lower_bound, NCC_upper_bound, "NCC Outliers")
print(NCC_plot_outliers)

# Quartiles:
NCC_q1 <- quantile(Dados$Num_Credit_Card, 0.25, na.rm = TRUE)
NCC_q3 <- quantile(Dados$Num_Credit_Card, 0.75, na.rm = TRUE)

# Mean NCC:
NCC_mean <- mean(Dados$Num_Credit_Card)

# Calculating NCC median:
NCC_median <- median(Dados$Num_Credit_Card[Dados$Num_Credit_Card >= NCC_lower_bound 
                                           & Dados$Num_Credit_Card <= NCC_upper_bound], na.rm = TRUE)

# Replacing outliers with median:
Dados <- Dados %>%
  mutate(Num_Credit_Card = ifelse(Num_Credit_Card < NCC_lower_bound | Num_Credit_Card > NCC_upper_bound, NCC_median, Num_Credit_Card))

# Calculating NCC median after corrections:
NCC_median <- median(Dados$Num_Credit_Card[Dados$Num_Credit_Card >= NCC_lower_bound 
                                           & Dados$Num_Credit_Card <= NCC_upper_bound], na.rm = TRUE)

# BoxPlot NCC:
NCC_boxplot <- plot_boxplot(Dados, "Num_Credit_Card", NCC_lower_bound, NCC_upper_bound, NCC_q1, NCC_median, NCC_mean, NCC_q3, "NCC Boxplot")


#  Interest_Rate column
# Counting NA values (0):
(na_count_IR <- sum(is.na(Dados$Interest_Rate)))

# Unique values (970 between 1 and 5797, possible missing commas? 
# Even 57.97 seems very high):
unique_IR <- Dados %>%
  select(Interest_Rate) %>%  
  distinct() %>%  
  arrange(desc(Interest_Rate))
print(head(unique_IR, 970))

# Maximum Interest_Rate per occupation (several have entries above 5000 interest rate):
Max_IR_por_occupation <- Dados %>%
  group_by(Occupation) %>%              
  summarise(Max_Interest_Rate = max(Interest_Rate, na.rm = TRUE))
print(Max_IR_por_occupation)

# IQR:
IR_lower_bound <- calculate_lower_bound(Dados, "Interest_Rate", multiplier = 1.5)
IR_upper_bound <- calculate_upper_bound(Dados, "Interest_Rate", multiplier = 1.5)

# Function to visualize outliers:
IR_plot_outliers <- plot_outliers(Dados, "Interest_Rate", IR_lower_bound, IR_upper_bound, "IR Outliers")
print(IR_plot_outliers)

# Quartiles:
IR_q1 <- quantile(Dados$Interest_Rate, 0.25, na.rm = TRUE)
IR_q3 <- quantile(Dados$Interest_Rate, 0.75, na.rm = TRUE)

# Mean IR:
IR_mean <- mean(Dados$Interest_Rate)

# Calculating IR median:
IR_median <- median(Dados$Interest_Rate[Dados$Interest_Rate >= IR_lower_bound 
                                        & Dados$Interest_Rate <= IR_upper_bound], na.rm = TRUE)

# Replacing outliers with the median:
Dados <- Dados %>%
  mutate(Interest_Rate = ifelse(Interest_Rate < IR_lower_bound | Interest_Rate > IR_upper_bound, IR_median, Interest_Rate))

# BoxPlot IR:
IR_boxplot <- plot_boxplot(Dados, "Interest_Rate", IR_lower_bound, IR_upper_bound, IR_q1, IR_median, IR_mean, IR_q3, "IR Boxplot")
summary(Dados$Interest_Rate)


# Num_of_Loan column
# Counting NA values (0):
(na_count_NOL <- sum(is.na(Dados$Num_of_Loan)))

# Counting empty values ("") (0):
Null_NOL <- Dados %>%
  filter(Num_of_Loan == "") %>%
  nrow()
print(Null_NOL)
summary(Dados$Num_of_Loan)

# Unique values (229):
unique_NOL <- Dados %>%
  select(Num_of_Loan) %>%  
  distinct() %>%  
  arrange(Num_of_Loan)
print(head(unique_NOL, 229))

# Number of entries with NOL = -100 (1948):
Neg_NOL <- Dados %>%
  filter(Num_of_Loan == -100) %>%
  nrow()
print(Neg_NOL)

# Number of entries with NOL = 0 (5426):
Null_NOL <- Dados %>%
  filter(Num_of_Loan == 0) %>%
  nrow()
print(Null_NOL)

# IQR:
NOL_lower_bound <- calculate_lower_bound(Dados, "Num_of_Loan", multiplier = 1.5)
NOL_upper_bound <- calculate_upper_bound(Dados, "Num_of_Loan", multiplier = 1.5)

# Function to visualize outliers:
NOL_outliers <- plot_outliers(Dados, "Num_of_Loan", NOL_lower_bound, NOL_upper_bound, "NOL Outliers")
print(NOL_outliers)

# Quartiles:
NOL_q1 <- quantile(Dados$Num_of_Loan, 0.25, na.rm = TRUE)
NOL_q3 <- quantile(Dados$Num_of_Loan, 0.75, na.rm = TRUE)

# Mean NOL:
NOL_mean <- mean(Dados$Num_of_Loan)

# Median NOL calculation:
NOL_median <- median(Dados$Num_of_Loan[Dados$Num_of_Loan >= NOL_lower_bound 
                                       & Dados$Num_of_Loan <= NOL_upper_bound], na.rm = TRUE)

# Replacing outliers with the median:
Dados <- Dados %>%
  mutate(Num_of_Loan = ifelse(Num_of_Loan < NOL_lower_bound | Num_of_Loan > NOL_upper_bound, NOL_median, Num_of_Loan))

# BoxPlot NOL:
NOL_boxplot <- plot_boxplot(Dados, "Num_of_Loan", NOL_lower_bound, NOL_upper_bound, NOL_q1, NOL_median, NOL_mean, NOL_q3, "NOL Boxplot")
summary(Dados$Num_of_Loan)


# Type_of_Loan column
# Unique values in TOL (6220):
unique_TOL_aggregate <- Dados %>%
  select(Type_of_Loan) %>%
  distinct()
print(unique_TOL_aggregate)

# Counting NA values (5647):
(na_count_TOL <- sum(is.na(Dados$Type_of_Loan)))

# Counting empty values ("") (0):
Null_TOL <- Dados %>%
  filter(Type_of_Loan == "") %>%
  nrow()
print(Null_TOL)

# Checking if there are entries without TOL but with TOL in other entries for the same Customer_ID (0):
TOL_null_and_string <- Dados %>%
  group_by(Customer_ID) %>%
  filter(any(!is.na(Type_of_Loan) & Type_of_Loan != "") 
         & any(is.na(Type_of_Loan) | Type_of_Loan == "")) %>%
  summarise(count = n())
entries_wo_TOL_w_TOL_on_other_entries <- sum(TOL_null_and_string$count)
print(entries_wo_TOL_w_TOL_on_other_entries)

# Replacing NA with "Not Specified":
Dados <- Dados %>%
  mutate(Type_of_Loan = ifelse(is.na(Type_of_Loan), "Not Specified", Type_of_Loan))

# Unique values of any loan type (18, excluding NA):
Unique_TOL <- Dados %>%
  mutate(TOL_split = strsplit(as.character(Type_of_Loan), ",")) %>% 
  unnest(TOL_split) %>%  
  mutate(TOL_split = trimws(TOL_split)) %>%  
  distinct(TOL_split) %>% 
  filter(TOL_split != "") %>%  
  pull(TOL_split) 

# Function to count the number of loans in each row:
count_loans <- function(loan_string) {
  # Remove words like "and" and split using list separators (comma)
  loan_list <- strsplit(gsub("and", "", loan_string), ",")[[1]]
  # Remove extra spaces and count the resulting elements
  length(trimws(loan_list))
}

# Apply the function to each row and create a new vector with the number of loans:
loan_counts <- sapply(Dados$Type_of_Loan, count_loans)

# Combine the results into a data frame if necessary:
loan_summary <- data.frame(OriginalText = Dados$Type_of_Loan, LoanCount = loan_counts)

# Counting NA values (0):
(na_count_TOL <- sum(is.na(Dados$Type_of_Loan)))


# Delay_from_due_date column
# Unique Values DFDD (negative values: -5, -4, -3, -2, -1 -- max value 67):
unique_DFDD <- Dados %>%
  select(Delay_from_due_date) %>%
  distinct() %>%
  arrange((Delay_from_due_date))
print(unique_DFDD)

# Counting NA values (0):
(na_count_DFDD <- sum(is.na(Dados$Delay_from_due_date)))

# Counting empty values ("") (0):
Null_DFDD <- Dados %>%
  filter(Delay_from_due_date == "") %>%
  nrow()
print(Null_DFDD)

# IQR (1993 outliers):
DFDD_lower_bound <- calculate_lower_bound(Dados, "Delay_from_due_date", multiplier = 1.5)
DFDD_upper_bound <- calculate_upper_bound(Dados, "Delay_from_due_date", multiplier = 1.5)

# Function to visualize outliers:
DFDD_outliers <- plot_outliers(Dados, "Delay_from_due_date", DFDD_lower_bound, DFDD_upper_bound, "DFDD Outliers")

# Quartiles:
DFDD_q1 <- quantile(Dados$Delay_from_due_date, 0.25, na.rm = TRUE)
DFDD_q3 <- quantile(Dados$Delay_from_due_date, 0.75, na.rm = TRUE)

# Mean DFDD:
DFDD_mean <- mean(Dados$Delay_from_due_date, na.rm = TRUE)

# Median DFDD calculation:
DFDD_median <- median(Dados$Delay_from_due_date[Dados$Delay_from_due_date >= DFDD_lower_bound 
                                                & Dados$Delay_from_due_date <= DFDD_upper_bound], na.rm = TRUE)

# Replacing outliers with the median:
Dados <- Dados %>%
  mutate(Delay_from_due_date = ifelse(Delay_from_due_date < DFDD_lower_bound | Delay_from_due_date > DFDD_upper_bound, DFDD_median, Delay_from_due_date))

# BoxPlot DFDD:
DFDD_plot_boxplot <- plot_boxplot(Dados, "Delay_from_due_date", DFDD_lower_bound, DFDD_upper_bound, DFDD_q1, DFDD_median, DFDD_mean, DFDD_q3, "DFDD Boxplot")


# Num_of_Delayed_Payment column
# Counting NA values:
sum(is.na(Dados$Num_of_Delayed_Payment))

# Median calculation for the variable:
mediana_variavel_Num_of_Delayed_Payment <- median(Dados$Num_of_Delayed_Payment, na.rm = TRUE)

# Calculating outlier bounds:
NDP_lower_bound <- calculate_lower_bound(Dados, "Num_of_Delayed_Payment")
NDP_upper_bound <- calculate_upper_bound(Dados, "Num_of_Delayed_Payment")

# Replacing NA values with the median delay of the same Customer_ID:
Dados <- replace_na_with_median(Dados, "Customer_ID", "Num_of_Delayed_Payment", NDP_lower_bound, NDP_upper_bound)

# Replacing remaining NA values with the median of the variable:
Dados <- replace_na_with_given_value(Dados, "Num_of_Delayed_Payment", mediana_variavel_Num_of_Delayed_Payment)

# Checking if NA values were correctly eliminated:
sum(is.na(Dados$Num_of_Delayed_Payment))

# Visualizing outliers using the defined function:
NDP_outliers <- plot_outliers(Dados, "Num_of_Delayed_Payment", NDP_lower_bound, NDP_upper_bound, "Num_of_Delayed_Payment")

# Analyzing outliers using a dataframe:
(outliers_df_NDP <- find_outliers(Dados, "Num_of_Delayed_Payment", NDP_lower_bound, NDP_upper_bound))

# After analyzing the data, it is evident that customers with outliers also have other non-outlier rows in the dataset:
Dados <- replace_outliers_with_median(Dados, "Customer_ID", "Num_of_Delayed_Payment", NDP_lower_bound, NDP_upper_bound, mediana_variavel_Num_of_Delayed_Payment)


# Changed_Credit_Limit column
# Median calculation for the variable:
mediana_variavel_Changed_Credit_Limit <- median(Dados$Changed_Credit_Limit, na.rm = TRUE)

# Calculating outlier bounds:
CCL_lower_bound <- calculate_lower_bound(Dados, "Changed_Credit_Limit")
CCL_upper_bound <- calculate_upper_bound(Dados, "Changed_Credit_Limit")

# Replacing null values with the median changes of the same Customer_ID:
Dados <- replace_na_with_median(Dados, "Customer_ID", "Changed_Credit_Limit", CCL_lower_bound, CCL_upper_bound)

# Replacing remaining NA values with the median of the variable:
Dados <- replace_na_with_given_value(Dados, "Changed_Credit_Limit", mediana_variavel_Changed_Credit_Limit)

# Visualizing outliers using the defined function:
CCL_outliers <- plot_outliers(Dados, "Changed_Credit_Limit", CCL_lower_bound, CCL_upper_bound, "Changed_Credit_Limit")

# Analyzing outliers using a dataframe:
(outliers_df_CCL <- find_outliers(Dados, "Changed_Credit_Limit", CCL_lower_bound, CCL_upper_bound))

# After analyzing the data, it is evident that customers with outliers also have other non-outlier rows in the dataset:
Dados <- replace_outliers_with_median(Dados, "Customer_ID", "Changed_Credit_Limit", CCL_lower_bound, CCL_upper_bound, mediana_variavel_Changed_Credit_Limit)


# Num_Credit_Inquiries column
# Median calculation for the variable:
mediana_variavel_Num_Credit_Inquiries <- median(Dados$Num_Credit_Inquiries, na.rm = TRUE)

# Calculating bounds:
NCI_lower_bound <- calculate_lower_bound(Dados, "Num_Credit_Inquiries")
NCI_upper_bound <- calculate_upper_bound(Dados, "Num_Credit_Inquiries")

# Replace missing values with the median number of inquiries for the same Customer_ID:  
Dados <- replace_na_with_median(Dados, "Customer_ID", "Num_Credit_Inquiries", NCI_lower_bound, NCI_upper_bound)

# Replace remaining missing values with the variable median:  
Dados <- replace_na_with_given_value(Dados, "Num_Credit_Inquiries", mediana_variavel_Num_Credit_Inquiries)

# Visualize outliers using the previously defined function:  
NCI_outliers <- plot_outliers(Dados, "Num_Credit_Inquiries", NCI_lower_bound, NCI_upper_bound, "Num_Credit_Inquiries")

# Analyze outliers in the dataframe:  
(outliers_df_NCI <- find_outliers(Dados,"Num_Credit_Inquiries", NCI_lower_bound, NCI_upper_bound))

# After analyzing the data, it is observed that customers with outliers also have other non-outlier records in the dataset:  
Dados <- replace_outliers_with_median(Dados, "Customer_ID", "Num_Credit_Inquiries", NCI_lower_bound, NCI_upper_bound, mediana_variavel_Num_Credit_Inquiries)


# Credit_Mix column
# Replace "_" values with the mode associated with the same Customer_ID:  
Dados <- Dados %>%
  group_by(Customer_ID) %>%
  mutate(Credit_Mix = ifelse(Credit_Mix == "_", 
                             get_mode(Credit_Mix[Credit_Mix != "_"]),  # Calculate the mode of values that do not contain "_":  
                             Credit_Mix)) %>%
  ungroup()

# Check for missing values:  
sum(is.na(Dados$Credit_Mix))

# Define one-hot encoding (dummy encoding) for the variable and integrate the new columns into the dataset:  
dummy <- dummyVars(" ~ Credit_Mix", data = Dados)
Credit_Mix_df <- data.frame(predict(dummy, newdata = Dados))
Dados <- cbind(Dados, Credit_Mix_df)

# Remove the original Credit_Mix column (Check why an additional column named CreditMix appears):  
Dados$Credit_Mix <- NULL


# Outstanding_Debt column
summary(Dados$Outstanding_Debt)

# Calculate outlier limits:  
OD_lower_bound <- calculate_lower_bound(Dados, "Outstanding_Debt")
OD_upper_bound <- calculate_upper_bound(Dados, "Outstanding_Debt")

# Visualize outliers using the previously defined function:  
OD_outliers <- plot_outliers(Dados, "Outstanding_Debt", OD_lower_bound, OD_upper_bound, "Outstanding_Debt")

# Analyze outliers in the dataframe:  
(outliers_df_OD <- find_outliers(Dados,"Outstanding_Debt", OD_lower_bound, OD_upper_bound))

# Calculate OD median:  
OD_median <- median(Dados$Outstanding_Debt[Dados$Outstanding_Debt >= OD_lower_bound 
                                           & Dados$Outstanding_Debt <= OD_upper_bound], na.rm = TRUE)

# Replace outliers with the median:  
Dados <- Dados %>%
  mutate(Outstanding_Debt = ifelse(Outstanding_Debt < OD_lower_bound | Outstanding_Debt > OD_upper_bound, OD_median, Outstanding_Debt))


# Credit_Utilization_Ratio column
# Calculate outlier limits:  
CUR_lower_bound <- calculate_lower_bound(Dados, "Credit_Utilization_Ratio")
CUR_upper_bound <- calculate_upper_bound(Dados, "Credit_Utilization_Ratio")

# Visualize outliers using the previously defined function:  
CUR_outliers <- plot_outliers(Dados, "Credit_Utilization_Ratio", CUR_lower_bound, CUR_upper_bound, "Credit_Utilization_Ratio")

# Analyze outliers in the dataframe:  
(outliers_df_CUR <- find_outliers(Dados,"Credit_Utilization_Ratio", CUR_lower_bound, CUR_upper_bound))

# Calculate CUR median:  
CUR_median <- median(Dados$Credit_Utilization_Ratio[Dados$Credit_Utilization_Ratio >= CUR_lower_bound 
                                                    & Dados$Credit_Utilization_Ratio <= CUR_upper_bound], na.rm = TRUE)

# Replace outliers with the median:  
Dados <- Dados %>%
  mutate(Credit_Utilization_Ratio = ifelse(Credit_Utilization_Ratio < CUR_lower_bound | Credit_Utilization_Ratio > CUR_upper_bound, CUR_median, Credit_Utilization_Ratio))


### Credit_History_Age column
# Count missing values: 4475  
sum(is.na(Dados$Credit_History_Age))

# Calculate outlier limits:  
CHA_lower_bound <- calculate_lower_bound(Dados, "Credit_History_Age")
CHA_upper_bound <- calculate_upper_bound(Dados, "Credit_History_Age")

# Calculate CHA median:  
CHA_median <- median(Dados$Credit_Utilization_Ratio[Dados$Credit_Utilization_Ratio >= CHA_lower_bound 
                                                    & Dados$Credit_Utilization_Ratio <= CHA_upper_bound], na.rm = TRUE)

# Update missing values (NA) where information exists for the Customer_ID:  
Dados <- replace_na_with_median(Dados, "Customer_ID", "Credit_History_Age", CHA_lower_bound, CHA_upper_bound)

# Recount missing values: 58  
sum(is.na(Dados$Credit_History_Age))

# Data description for the column:  
# Min.: 60; 1st Qu.: 4410; Median: 6690; Mean: 6734; 3rd Qu.: 9166; Max.: 12285

# Replace missing values using the median for the corresponding age group:  
Dados <- replace_na_with_median(Dados, "Age_Group", "Credit_History_Age", CHA_lower_bound, CHA_upper_bound)

# After replacing missing values, descriptive statistics remain almost unchanged, preserving data integrity:  
# Only the median and the third quartile decreased slightly (by a few dozen units).  
# Min.: 60; 1st Qu.: 4410; Median: 6660; Mean: 6734; 3rd Qu.: 9155; Max.: 12285

# Graphical representation:  
plot_histogram(Dados, "Credit_History_Age", binwidth = 365, title = "Histogram of Credit History (in days)", overlay_normal = TRUE)


# Total_EMI_per_month column
# In this case, it is observed that up to the 90th percentile, values remain relatively homogeneous:  
# However, extreme outliers appear in the 100th percentile, with a maximum value of 82,236.00:  
quantile(Dados$Total_EMI_per_month, probs = seq(0, 1, 0.1))

# Definition of outlier limits:
Q1_EMI <- quantile(Dados$Total_EMI_per_month, 0.25)
Q3_EMI <- quantile(Dados$Total_EMI_per_month, 0.75)
IQR_EMI <- Q3_EMI - Q1_EMI
lower_bound_EMI <- 0

# The upper limit calculation is effective, as it includes values above the 90th percentile
# while ensuring the exclusion of extreme values:
upper_bound_EMI <- calculate_upper_bound(Dados, "Total_EMI_per_month", multiplier = 1.5)
median_EMI <- median(Dados$Total_EMI_per_month, na.rm = TRUE)

# Create a dataframe to represent outliers and visualize them:
outliers_df_EMI <- find_outliers(Dados, "Total_EMI_per_month", lower_bound_EMI, upper_bound_EMI)
outliers_EMI <- plot_outliers(Dados, "Total_EMI_per_month", lower_bound_EMI, upper_bound_EMI, "Scatter Plot with Outliers - EMI per month")

# Description:
# 1st Qu.: 30.49; Median: 69.43; 3rd Qu.: 161.56
summary(Dados$Total_EMI_per_month)

# Replace outliers with the median for the same customer:
Dados <- replace_outliers_with_median(Dados, "Customer_ID", "Total_EMI_per_month", lower_bound_EMI, upper_bound_EMI, median_EMI)

# Verify that no outliers remain:
outliers_df_EMI <- find_outliers(Dados, "Total_EMI_per_month", lower_bound_EMI, upper_bound_EMI)

# The distribution remains largely unchanged:
# 1st Qu.: 29.03; Median: 66.15; 3rd Qu.: 131.56
summary(Dados$Total_EMI_per_month)


# Amount_invested_monthly column
# Summary statistics:
# 1st Qu.: 72.55; Median: 129.26; Mean: 195.26; 3rd Qu.: 237.59; NAs: 4404
summary(Dados$Amount_invested_monthly)

# Visualize data distribution excluding NAs:
plot_histogram(Dados, "Amount_invested_monthly", binwidth = 100, title = "Distribution of Amount Invested Monthly", overlay_normal = FALSE)

# Calculate outlier limits:
AIM_lower_bound <- calculate_lower_bound(Dados, "Credit_History_Age")
AIM_upper_bound <- calculate_upper_bound(Dados, "Credit_History_Age")

# Replace NAs with the median for the respective age group:
Dados <- replace_na_with_median(Dados, "Age_Group", "Amount_invested_monthly", AIM_lower_bound, AIM_upper_bound)

# Updated summary statistics:
# 1st Qu.: 77.31; Median: 125.80; Mean: 189.50; 3rd Qu.: 220.67
summary(Dados$Amount_invested_monthly)

# Visualize data distribution excluding NAs:
plot_histogram(Dados, "Amount_invested_monthly", binwidth = 100, title = "Distribution of Amount Invested Monthly", overlay_normal = FALSE)


# Payment_Behaviour column
# Replace NAs with the mode for the respective customer:
Dados <- replace_na_with_mode(Dados, "Customer_ID", "Payment_Behaviour")

# Remaining NAs: 51
sum(is.na(Dados$Payment_Behaviour))

# Replace remaining NAs with the mode for the respective age group:
Dados <- replace_na_with_mode(Dados, "Age_Group", "Payment_Behaviour")
sum(is.na(Dados$Payment_Behaviour))

# One-hot encoding for Payment_Behaviour:
Dados$Spending_Level <- ifelse(grepl("Low_spent", Dados$Payment_Behaviour), 1,
                               ifelse(grepl("Medium_spent", Dados$Payment_Behaviour), 2,
                                      ifelse(grepl("High_spent", Dados$Payment_Behaviour), 3, NA)))

Dados$Payment_Value <- ifelse(grepl("Small_value_payments", Dados$Payment_Behaviour), 1,
                              ifelse(grepl("Medium_value_payments", Dados$Payment_Behaviour), 2,
                                     ifelse(grepl("Large_value_payments", Dados$Payment_Behaviour), 3, NA)))

# Visual representation of encoded variables:
plot_bar(Dados, "Spending_Level", "Spending Level:\n1: Low; 3: High")
plot_bar(Dados, "Payment_Value", "Payment Value:\n1: Small; 2: Medium; 3: Large")

# Remove the original Payment_Behaviour column:
Dados$Payment_Behaviour <- NULL


# Monthly_Balance column
# Summary:
# Min: 0.01; 1st Qu.: 269.67; Median: 337.16; Mean: 402.45; 3rd Qu.: 470.57; Max: 1576.29
# Remaining NAs: 609
summary(Dados$Monthly_Balance)

# Calculate outlier limits:
MB_lower_bound <- calculate_lower_bound(Dados, "Monthly_Balance")
MB_upper_bound <- calculate_upper_bound(Dados, "Monthly_Balance")

# Replace NAs with the median for the respective customer:
Dados <- replace_na_with_median(Dados, "Customer_ID", "Monthly_Balance", MB_lower_bound, MB_upper_bound)

# After imputation, remaining NAs: 6
summary(Dados$Monthly_Balance)

# Replace remaining NAs with the median for the respective age group:
Dados <- replace_na_with_median(Dados, "Age_Group", "Monthly_Balance", MB_lower_bound, MB_upper_bound)

# Final summary confirms data integrity:
summary(Dados$Monthly_Balance)

# Remove unnecessary variables:
Dados$Customer_ID <- NULL
Dados$ID <- NULL

# Convert categorical variables to factors:
Dados$Occupation <- as.factor(Dados$Occupation)
Dados$Credit_Score <- as.factor(Dados$Credit_Score)
