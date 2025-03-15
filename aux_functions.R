# Helper Functions


# Function to replace missing values and NAs with the median of the group:
replace_na_with_median <- function(data, id_column, target_column, lower_bound, upper_bound) {
  data %>%
    group_by(.data[[id_column]]) %>%
    mutate(
      # Calculate the median considering only values within the range:
      median_value = median(
        .data[[target_column]][.data[[target_column]] >= lower_bound & 
                                 .data[[target_column]] <= upper_bound], 
        na.rm = TRUE),
      # Replace NAs with the median calculated above:
      {{ target_column }} := ifelse(.data[[target_column]] == "" | is.na(.data[[target_column]]), 
                                    median_value, 
                                    .data[[target_column]])) %>%
    ungroup() %>%
    select(-median_value)  # Delete the column created for the median:
}


# Function to replace the remaining values after the previous substitution with the group's median (no values for the group):
replace_na_with_given_value <- function(data, target_column, replacement_value) {
  data %>%
    mutate(
      {{ target_column }} := ifelse(is.na(.data[[target_column]]), 
                                    replacement_value, 
                                    .data[[target_column]]))  # Replace NAs with a given value.
}


# Function to calculate outlier limits based on the IQR method:
calculate_lower_bound <- function(data, column, multiplier = 1.5) {
  Q1 <- quantile(Dados[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(Dados[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  # Define the lower_bound for columns that accept negative values:
  lower_bound <- if (column == "Changed_Credit_Limit") Q1 - multiplier * IQR else max(0, Q1 - multiplier * IQR)
}


# Function to calculate the upper bound:
calculate_upper_bound <- function(data, column, multiplier = 1.5) {
  Q1 <- quantile(Dados[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(Dados[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  upper_bound <- Q3 + multiplier * IQR  # Calculate the upper bound.
}


# Function to visualize the outliers:
plot_outliers <- function(data, column, lower_bound, upper_bound, title) {
  data <- data %>%
    mutate(OutlierStatus = ifelse(.data[[column]] < lower_bound | .data[[column]] > upper_bound, "Outliers", "Within Bounds"))
  
  p <- ggplot(data, aes(x = seq_along(.data[[column]]), y = .data[[column]])) +
    geom_point(aes(color = OutlierStatus), size = 3) +
    scale_color_manual(
      values = c("Outliers" = "red", "Within Bounds" = "black"),
      labels = c("Outliers", "Within Bounds")
    ) +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "orange") + 
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "orange") +
    labs(title = title, x = "Index", y = column, color = "Status") +
    theme_minimal()
  print(p) 
}


# Function to create a dataframe to represent the outliers:
find_outliers <- function(data, column, lower_bound, upper_bound) {
  # Filter data to contain only the outliers:
  outliers_df <- data %>%
    filter(.data[[column]] < lower_bound | .data[[column]] > upper_bound)
  # Select distinct outliers:
  sorted_unique_outliers <- sort(unique(outliers_df[[column]]))
  return(sorted_unique_outliers)
}


# Function to create a histogram:
plot_histogram <- function(data, column, binwidth, title, overlay_normal = TRUE, x_limits = NULL) {
  # Filter NAs, if necessary:
  data <- data %>% filter(!is.na(.data[[column]]))
  
  # Specify the mean and standard deviation in the range values:
  if (!is.null(x_limits)) {
    data <- data %>% filter(.data[[column]] >= x_limits[1] & .data[[column]] <= x_limits[2])
  }
  
  # Calculate the mean and standard deviation of the column:
  mean_val <- mean(data[[column]], na.rm = TRUE)
  sd_val <- sd(data[[column]], na.rm = TRUE)
  
  # Create the base histogram plot:
  p <- ggplot(data, aes_string(x = column)) + 
    geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = column, y = "Density") +
    theme_minimal()
  
  # Add x limits if necessary:
  if (!is.null(x_limits)) {
    p <- p + coord_cartesian(xlim = x_limits)
  }
  
  # Add the normal curve if needed:
  if (overlay_normal) {
    # Define the range for the normal curve based on x limits, if provided:
    if (!is.null(x_limits)) {
      # If x_limits is defined, use this range for normal_range:
      normal_range <- seq(x_limits[1], x_limits[2], length.out = 100)
    } else {
      # If x_limits is not defined, use the data range:
      normal_range <- seq(min(data[[column]], na.rm = TRUE), max(data[[column]], na.rm = TRUE), length.out = 100)
    }
    
    # Create a dataframe for the normal curve within the x range:
    normal_data <- data.frame(
      x = normal_range,
      y = dnorm(normal_range, mean = mean_val, sd = sd_val)
    )
    
    # Add the normal curve as a plotline, constrained to x_limits:
    p <- p + geom_line(data = normal_data, aes(x = x, y = y), color = "red", linetype = "dashed", size = 1)
  }
  print(p) 
}


# Function to create a bar plot:
plot_bar <- function(data, column, title) {
  summary_df <- data %>%
    group_by(.data[[column]]) %>%
    summarise(Count = n(), .groups = 'drop')
  
  ggplot(summary_df, aes(x = .data[[column]], y = Count, fill = .data[[column]])) +
    geom_bar(stat = "identity") +
    labs(title = title, x = column, y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
}


# Function to create a box plot:
plot_boxplot <- function(data, column, lower_bound, upper_bound, q1, median, mean, q3, title) {
  p <- ggplot(data, aes(y = !!sym(column))) + 
    geom_boxplot(outlier.color = "red", fill = "lightblue") +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "pink") + 
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "pink") +
    geom_hline(yintercept = q1, linetype = "dotted", color = "blue", size = 1) +
    geom_hline(yintercept = median, linetype = "solid", color = "cyan", size = 1.2) +
    geom_hline(yintercept = mean, linetype = "solid", color = "orange", size = 1.2) +
    geom_hline(yintercept = q3, linetype = "dotted", color = "blue", size = 1) +
    labs(title = title, x ="Column", y = "Values") +
    theme_minimal() +
    annotate("text", x = 1, y = q1, label = "Q1", vjust = -0.3, hjust = 1.2, color = "blue") +
    annotate("text", x = 1, y = lower_bound, label = "Lower Bound", vjust = -0.2, hjust = 1.2, color = "pink") +
    annotate("text", x = 1, y = upper_bound, label = "Upper Bound", vjust = -0.2, hjust = 1.2, color = "pink") +
    annotate("text", x = 1, y = median, label = "Median", vjust = +1.2, hjust = 1.2, color = "cyan") +
    annotate("text", x = 1, y = mean, label = "Mean", vjust = -1, hjust = 1.2, color = "orange") +
    annotate("text", x = 1, y = q3, label = "Q3", vjust = -0.3, hjust = 1.2, color = "blue")
  print(p)
}


# Function to replace outliers with the first non-outlier value associated with the Customer_ID:
replace_outliers <- function(data, id_column, target_column, lower_bound, upper_bound) {
  data %>%
    group_by(.data[[id_column]]) %>%
    mutate({{ target_column }} := ifelse(.data[[target_column]] < lower_bound | .data[[target_column]] > upper_bound,
                                         # Replace with the first non-outlier value of the group:
                                         ifelse(
                                           any(.data[[target_column]] >= lower_bound & .data[[target_column]] <= upper_bound),
                                           first(.data[[target_column]][.data[[target_column]] >= lower_bound & .data[[target_column]] <= upper_bound]),
                                           .data[[target_column]]),
                                         .data[[target_column]])) %>%
    ungroup()
}


# Function to replace outliers with the group median:
replace_outliers_with_median <- function(data, id_column, target_column, lower_bound, upper_bound, overall_median) {
  data %>%
    group_by(.data[[id_column]]) %>%
    mutate(
      # Calculate the group median excluding the outliers:
      median_value = ifelse(any(.data[[target_column]] <= upper_bound & .data[[target_column]] >= lower_bound), 
                            median(.data[[target_column]][.data[[target_column]] <= upper_bound & .data[[target_column]] >= lower_bound], na.rm = TRUE), 
                            overall_median),  # If not possible to associate with a group, use the overall median of the variable:
      # Replace outlier values with the group median:
      {{ target_column }} := ifelse(.data[[target_column]] < lower_bound | .data[[target_column]] > upper_bound, 
                                    median_value, 
                                    .data[[target_column]])
    ) %>%
    ungroup() %>%
    select(-median_value)
}


# Function to calculate the mode:
get_mode <- function(x) {
  # Remove NA values:
  x <- na.omit(x)
  # Get the most frequent value:
  uniq_x <- unique(x)
  mode_value <- uniq_x[which.max(tabulate(match(x, uniq_x)))]
  return(mode_value)
}

# Function to replace NAs with the mode:
replace_na_with_mode <- function(data, id_column, target_column) {
  data %>%
    group_by(.data[[id_column]]) %>%
    mutate(
      # Calculate the group mode, ignoring NA values:
      mode_value = get_mode(.data[[target_column]]),
      # Replace NAs and empty strings with the mode calculated above:
      {{ target_column }} := ifelse(.data[[target_column]] == "" | is.na(.data[[target_column]]), 
                                    mode_value, 
                                    .data[[target_column]])) %>%
    ungroup() %>%
    select(-mode_value)
}


# Function to plot the correlation heatmap and also the percentage of correlations outside the defined intervals:
plot_corr_heatmap <- function(data, corr_limits, exclude_columns = NULL, method = "pearson") {
  # Select only numeric columns:
  numeric_columns <- data[, sapply(data, is.numeric) | sapply(data, is.integer)]
  
  # Remove columns if necessary:
  if (!is.null(exclude_columns)) {
    numeric_columns <- numeric_columns[, !colnames(numeric_columns) %in% exclude_columns]
  }
  
  # Calculate the correlation matrix using the specified method (Pearson by default):
  cor_matrix <- cor(numeric_columns, use = "complete.obs", method = method)
  
  # Plot the correlation matrix using the corrplot package:
  corrplot(cor_matrix, 
           method = "color", 
           col = colorRampPalette(c("blue", "white", "red"))(200),
           addCoef.col = "black", 
           tl.col = "black", 
           tl.pos = "lt",   
           tl.srt = 90,    
           number.cex = 0.7,  
           cex.main = 1.2,  
           tl.cex = 0.7,    
           cl.cex = 0.8)   
  
  # Title for the plot:
  title_text <- paste("Correlation Heatmap -", method, "method")
  mtext(title_text, side = 3, line = 2, cex = 2, font = 2, adj = 0.5)
  
  # Remove duplicate values ("mirror" of the heatmap), calculate the total number of unique correlations 
  # and identify the pairs outside the defined limits:
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  high_correlation_pairs <- which(cor_matrix > corr_limits[2] | cor_matrix < corr_limits[1], arr.ind = TRUE)
  
  # Extract the correlation values and the corresponding variable names:
  correlation_values <- cor_matrix[high_correlation_pairs]
  var1 <- rownames(cor_matrix)[high_correlation_pairs[, 1]]
  var2 <- colnames(cor_matrix)[high_correlation_pairs[, 2]]
  
  # Create a dataframe to store the pairs with correlation outside the limits:
  high_corr_df <- data.frame(Variable1 = var1, 
                             Variable2 = var2, 
                             Correlation = correlation_values)
  
  # Remove duplicate pairs ("mirror" of the Heatmap):
  high_corr_df <- high_corr_df[!duplicated(t(apply(high_corr_df[, c("Variable1", "Variable2")], 1, sort))), ]
  
  # If no significant correlations found:
  if (nrow(high_corr_df) == 0) {
    print("No significant correlations found within the given limits.")
    return(NULL) 
  }
  
  # Calculate the number and percentage of correlations outside the limits:
  outside_limits_count <- nrow(high_corr_df)
  total_correlations <- (ncol(numeric_columns) * (ncol(numeric_columns) - 1)) / 2
  outside_limits_percentage <- (outside_limits_count / total_correlations) * 100
  
  # Print results:
  print(paste("Intervals (", corr_limits[1], ",", corr_limits[2], "):", 
              "Number of correlations found:", outside_limits_count, 
              "/", total_correlations, 
              "(", round(outside_limits_percentage, 2), "%)"))
  return(high_corr_df)
}
