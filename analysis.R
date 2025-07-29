# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the datasets
data_2017_2018 <- read_excel("2017-2018 data set.xlsx", sheet = "17-18")
data_2024 <- read_excel("Cleaned 2024 data.xlsx", sheet = "Sheet1")

# Clean and prepare the 2017-2018 data
clean_2017_2018 <- data_2017_2018 %>%
  rename(
    Characteristic = `A`,
    Locality = `C`,
    Male = `D`,
    Female = `E`
  ) %>%
  filter(!is.na(Characteristic) & Characteristic != "") %>%
  mutate(
    Year = "2017-2018",
    Male = as.numeric(Male),
    Female = as.numeric(Female)
  ) %>%
  select(Year, Characteristic, Locality, Male, Female)

# Clean and prepare the 2024 data
clean_2024 <- data_2024 %>%
  rename(
    Characteristic = `A`,
    Locality = `D`,
    Male = `E`,
    Female = `F`
  ) %>%
  filter(!is.na(Characteristic) & Characteristic != "") %>%
  mutate(
    Year = "2024",
    Male = as.numeric(Male),
    Female = as.numeric(Female)
  ) %>%
  select(Year, Characteristic, Locality, Male, Female)

# Combine both datasets
combined_data <- bind_rows(clean_2017_2018, clean_2024)
