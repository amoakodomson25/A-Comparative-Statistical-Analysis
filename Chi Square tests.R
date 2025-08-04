# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(MASS)

tryCatch({
  data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)
  
  # Fix column names if needed
  if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
    colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
  }
  
  # Clean the data
  clean_data <- data_2024 %>%
    filter(!is.na(Education), !Education %in% c("Total", "")) %>%
    mutate(
      Education = factor(Education),
      Locality = factor(Locality),
      Male = as.numeric(Male),
      Female = as.numeric(Female)
    ) %>%
    filter(!is.na(Male), !is.na(Female), !is.na(Locality))
  
  # Convert wide Male/Female into long format (Sex)
  long_data <- clean_data %>%
    pivot_longer(cols = c(Male, Female), names_to = "Sex", values_to = "Count")
  
  # Create contingency tables and run chi-square tests
  
  # Education vs Sex
  edu_sex_table <- xtabs(Count ~ Education + Sex, data = long_data)
  cat("Education vs Sex:\n")
  print(chisq.test(edu_sex_table))
  
  # Education vs Locality
  edu_loc_table <- xtabs(Count ~ Education + Locality, data = long_data)
  cat("\nEducation vs Locality:\n")
  print(chisq.test(edu_loc_table))
  
  # Locality vs Sex
  loc_sex_table <- xtabs(Count ~ Locality + Sex, data = long_data)
  cat("\nLocality vs Sex:\n")
  print(chisq.test(loc_sex_table))
  
  # Create 3-way contingency table
  three_way_table <- xtabs(Count ~ Education + Locality + Sex, data = long_data)
  
  cat("Three-way table:\n")
  print(three_way_table)
  
  # 1. Mutual Independence: [Education][Locality][Sex]
  cat("\nTest for mutual (complete) independence:\n")
  mutual_indep <- loglm(~ Education + Locality + Sex, data = three_way_table)
  print(mutual_indep)
  
  
  
  
  
  
  
}, error = function(e) {
  message("An error occurred: ", e$message)
  if (exists("data_2024")) {
    print(head(data_2024))
    print(colnames(data_2024))
  } else {
    message("data_2024 was not successfully created.")
  }
})

