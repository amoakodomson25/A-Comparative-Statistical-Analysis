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
  
  
  
  gender_sd <- clean_data %>%
    summarise(
      `SD(Male)` = format(round(sd(Male), 1), nsmall = 1),
      `SD(Female)` = format(round(sd(Female), 1), nsmall = 1),
      SD_Ratio = format(round(sd(Male)/sd(Female), 2), nsmall = 2)
    )
  

  # Print comprehensive results
  cat("Gender Statistics (SD):\n")
  print(gender_sd)
  
  
  
}, error = function(e) {
  message("An error occurred: ", e$message)
  if (exists("data_2024")) {
    print(head(data_2024))
    print(colnames(data_2024))
  } else {
    message("data_2024 was not successfully created.")
  }
})