library(dplyr)
library(rcompanion)
library(readxl)
library(tibble)   # for column_to_rownames

data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)

# Fix column names if needed
if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
  colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
}

# Create contingency table
gender_edu_table <- data_2024 %>%
  group_by(Education) %>%
  summarise(
    Male = sum(Male),
    Female = sum(Female),
    .groups = "drop"
  ) %>%
  column_to_rownames(var = "Education")
# Run Chi-square test
chi_test <- chisq.test(gender_edu_table)

#Cramer's V
chi2 <- chi_test$statistic   # Chi-square value
n <- sum(gender_edu_table)   # Total sample size
k <- min(dim(gender_edu_table))  # Smaller dimension

cramers_v <- sqrt(chi2 / (n * (k - 1)))
cramers_v

# Print results
cat("Chi-Square Test of Independence:\n")
print(chi_test)

