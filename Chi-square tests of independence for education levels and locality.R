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


# Create a contingency table: Education Level × Locality
locality_edu_table <- data_2024 %>%
  group_by(Education, Locality) %>%
  summarise(
    Count = sum(Male + Female),  # Combine genders
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = Locality,
    values_from = Count
  ) %>%
  column_to_rownames(var = "Education")  # Set education levels as row names

chi_test_locality <- chisq.test(locality_edu_table)

# Print results
cat("Chi-Square Test of Independence (Education × Locality):\n")
print(chi_test_locality)
