# Load required packages
library(readxl)
library(dplyr)
library(tidyr)

tryCatch({
  data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)
  
  # Fix column names if needed
  if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
    colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
  }
  
  # Clean the data and order education levels
  clean_data <- data_2024 %>%
    filter(!is.na(Education), !Education %in% c("", "Total")) %>%
    mutate(
      Education = factor(Education, 
                         levels = c("Never attended", 
                                    "Basic education", 
                                    "Secondary/vocational", 
                                    "Tertiary"),
                         ordered = TRUE),
      Locality = factor(Locality),
      Male = as.numeric(Male),
      Female = as.numeric(Female)
    ) %>%
    filter(!is.na(Male), !is.na(Female), !is.na(Locality))
  
  
  
  # Calculate standard deviation by locality
  locality_sd <- clean_data %>%
    group_by(Locality) %>%
    summarise(
      `SD(Male)` = format(round(sd(Male), 1), nsmall = 1),
      `SD(Female)` = format(round(sd(Female), 1), nsmall = 1),
      `SD_Ratio` = format(round(sd(Male)/sd(Female), 1), nsmall = 1),
      .groups = 'drop'
    )
  
  # Print results
  cat("Standard Deviation of Individuals by Locality:\n")
  print(locality_sd, n = Inf)
  
  
  
  
}, error = function(e) {
  message("An error occurred: ", e$message)
  if (exists("data_2024")) {
    print(head(data_2024))
    print(colnames(data_2024))
  } else {
    message("data_2024 was not successfully created.")
  }
})