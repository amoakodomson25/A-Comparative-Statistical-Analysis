library(dplyr)
library(tidyr)
library(MASS)


data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024-figures", col_names = TRUE)

# Fix column names if needed
if(length(colnames(data_2024)) != 4 || any(is.na(colnames(data_2024)))) {
  colnames(data_2024) <- c("Education", "Locality", "Male", "Female")
}

# Remove "Total" row if it exists
data_2024 <- data_2024 %>% 
  filter(Education != "Total")

# Create a 3D table: Education × Gender × Locality
three_way_table <- data_2024 %>%
  pivot_longer(
    cols = c(Male, Female),
    names_to = "Gender",
    values_to = "Count"
  ) %>%
  xtabs(Count ~ Education + Gender + Locality, data = .)

# Print the table (flattened for readability)
ftable(three_way_table)

# Fit a hierarchical log-linear model
loglin_model <- loglm(~ Education + Gender + Locality + 
                        Education:Gender + Education:Locality + Gender:Locality, 
                      data = three_way_table)

# Test for the three-way interaction (full model vs. without interaction)
three_way_test <- anova(
  loglm(~ Education * Gender * Locality, data = three_way_table),  # Full model
  loglm(~ Education * Gender + Education * Locality + Gender * Locality, 
        data = three_way_table),  # Model without 3-way interaction
  test = "Chisq"
)

# Print results
cat("Three-Way Interaction Test (Education × Gender × Locality):\n")
print(three_way_test)
