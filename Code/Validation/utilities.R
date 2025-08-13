# G. Smith-Vidaurre
# 13 August 2025

# Purpose: Initialize functions for calculating traditional performance metrics.

####################
# Precision = true positives / (true positives + false positives). Out of all of the positive predictions by a method, which were truly positive relative to false positives? 

# In which X is a data frame in which rows correspond to unique video recording events, and columns indicating the number of true positives and false positives. The following two arguments are the column names for the true and false positives
precision <- function(X, tps_col_nm, fps_col_nm){
  # Make sure to return 0 if all values are 0 (which return NaN)
  prc <- round((X[[tps_col_nm]] / (X[[tps_col_nm]] + X[[fps_col_nm]])) * 100, 2)
  prc[is.na(prc)] <- 0
  return(prc)
}

# Example usage:

# # Perfect precision
# X <- data.frame(true_pos = c(1, 2, 2, 3), false_pos = c(0, 0, 0, 0))
# precision(X, "true_pos", "false_pos")

# # Reduced precision
# X <- data.frame(true_pos = c(1, 0, 2, 1), false_pos = c(1, 1, 0, 0))
# precision(X, "true_pos", "false_pos")


####################
# Sensitivity = true positives / (true positives + false negatives). This is the same as the recall, or the percentage of the true positives that a method correctly identifies relative to false negatives
# In which X is a data frame in which rows correspond to unique video recording events, and columns indicating the number of true positives and false negatives. The following two arguments are the column names for the true and false negatives
sensitivity <- function(X, tps_col_nm, fns_col_nm){
  # Make sure to return 0 if all values are 0
  rec <- round((X[[tps_col_nm]] / (X[[tps_col_nm]] + X[[fns_col_nm]])) * 100, 2)
  rec[is.na(rec)] <- 0
  return(rec)
}

# Example usage:

# # Perfect sensitivity
# X <- data.frame(true_pos = c(1, 1, 1, 1), false_neg = c(0, 0, 0, 0))
# sensitivity(X, "true_pos", "false_neg")

# # Reduced sensitivity
# X <- data.frame(true_pos = c(0, 1, 1, 1), false_neg = c(0, 2, 3, 0))
# sensitivity(X, "true_pos", "false_neg")


####################
# Specificity = true negatives / (true negatives + false positives). This captures how well a method calls true negatives and avoids false positives (e.g. false alarms).

# In which X is a data frame in which rows correspond to unique video recording events, and columns indicating the number of true negatives and false positives. The following two arguments are the column names for the true negatives and false positives
specificity <- function(X, tns_col_nm, fps_col_nm){
  # Make sure to return 0 if all values are 0
  rec <- round((X[[tns_col_nm]] / (X[[tns_col_nm]] + X[[fps_col_nm]])) * 100, 2)
  rec[is.na(rec)] <- 0
  return(rec)
}

# # Perfect specificity
# X <- data.frame(true_neg = c(1, 1, 1, 1), false_pos = c(0, 0, 0, 0))
# specificity(X, "true_neg", "false_pos")

# # Reduced specificity
# X <- data.frame(true_neg = c(0, 1, 1, 1), false_pos = c(0, 2, 3, 0))
# specificity(X, "true_neg", "false_pos")
