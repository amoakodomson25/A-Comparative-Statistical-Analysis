library(dplyr)
library(tidyr)

data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)

# Fix column names if needed
if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
  colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
}

# Remove "Total" row if it exists
data_2024 <- data_2024 %>% 
  filter(Education != "Total")


# Create a gender × locality contingency table
gender_locality_table <- data_2024 %>%
  group_by(Locality) %>%
  summarise(
    Male = sum(Male),
    Female = sum(Female)
  ) %>%
  column_to_rownames(var = "Locality")  # Set locality as row names

chi_test_gender_locality <- chisq.test(gender_locality_table)
phi <- sqrt(chi_test_gender_locality$statistic / sum(gender_locality_table))

# Print results
cat("Chi-Square Test of Independence (Gender × Locality):\n")
print(chi_test_gender_locality)
print(phi)


