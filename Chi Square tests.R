# Load required packages
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)

# 1. Import the Excel file
tryCatch({
  data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)
  
  # View column names to check if renaming is needed
  print("Original column names:")
  print(colnames(data_2024))
  
  # Manually set column names if necessary
  if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
    colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
  }
  
  # 2. Clean the data
  clean_data <- data_2024 %>%
    filter(!is.na(Education), !Education %in% c("Total", "")) %>%
    mutate(
      Education = factor(Education),
      Locality = factor(Locality),
      Male = as.numeric(Male),
      Female = as.numeric(Female)
    ) %>%
    filter(!is.na(Male), !is.na(Female), !is.na(Locality))
  
  # 3. Perform chi-square tests
  
  # Education vs Sex
  edu_sex_table <- clean_data %>%
    group_by(Education) %>%
    summarise(Male = sum(Male), Female = sum(Female), .groups = 'drop') %>%
    column_to_rownames("Education") %>%
    as.matrix()
  
  cat("Education Attainment vs Sex:\n")
  print(chisq.test(edu_sex_table))
  
  # Education vs Locality
  edu_loc_table <- clean_data %>%
    group_by(Education) %>%
    summarise(
      Urban = sum((Male + Female)[Locality == "Urban"]),
      Rural = sum((Male + Female)[Locality == "Rural"]),
      .groups = 'drop'
    ) %>%
    column_to_rownames("Education") %>%
    as.matrix()
  
  cat("\nEducation Attainment vs Locality:\n")
  print(chisq.test(edu_loc_table))
  
  # Locality vs Sex
  loc_sex_table <- clean_data %>%
    group_by(Locality) %>%
    summarise(Male = sum(Male), Female = sum(Female), .groups = 'drop') %>%
    column_to_rownames("Locality") %>%
    as.matrix()
  
  cat("\nLocality vs Sex:\n")
  print(chisq.test(loc_sex_table))
  
  
  
  
  
  
}, error = function(e) {
  message("An error occurred: ", e$message)
  message("\nDebugging info:")
  if (exists("data_2024")) {
    print(head(data_2024))
    print(colnames(data_2024))
  } else {
    message("data_2024 was not successfully created.")
  }
})

