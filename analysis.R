# Load required libraries
library(tidyverse)
library(ggplot2)
library(readxl)

# Read the Excel file
data <- read_excel("2017-2018 data set.xlsx", sheet = "17-18")

# Clean and prepare the data
# First, let's extract the education level names which are in column A
education_levels <- data$A[!is.na(data$A) & data$A != ""]
education_levels <- education_levels[education_levels != "Education Level"]

# Create a clean data frame
clean_data <- data.frame(
  Education = rep(education_levels, each = 2),
  Locality = rep(c("Urban", "Rural"), times = length(education_levels)),
  Male = as.numeric(c(data$D[2:3], data$D[4:5], data$D[6:7], data$D[8:9], data$D[10:11])),
  Female = as.numeric(c(data$E[2:3], data$E[4:5], data$E[6:7], data$E[8:9], data$E[10:11]))
)

# For Total row, female was calculated as 100 - male, so we need to fix that
clean_data$Female[clean_data$Education == "Total"] <- 100 - clean_data$Male[clean_data$Education == "Total"]

# 1. Comparison 1: Total Urban male vs Total Urban female
p1 <- clean_data %>% 
  filter(Education == "Total", Locality == "Urban") %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
  ggplot(aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Population by Gender in Urban Areas",
       subtitle = "Male vs Female",
       y = "Percentage") +
  theme_minimal()

# 2. Comparison 2: Total Rural male vs Total Rural female
p2 <- clean_data %>% 
  filter(Education == "Total", Locality == "Rural") %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
  ggplot(aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Population by Gender in Rural Areas",
       subtitle = "Male vs Female",
       y = "Percentage") +
  theme_minimal()

# 3. Comparison 3: Total Urban male vs Total Rural male
p3 <- clean_data %>% 
  filter(Education == "Total") %>%
  ggplot(aes(x = Locality, y = Male, fill = Locality)) +
  geom_bar(stat = "identity") +
  labs(title = "Male Population Comparison",
       subtitle = "Urban vs Rural",
       y = "Percentage") +
  theme_minimal()

# 4. Comparison 4: Total Urban female vs Total Rural female
p4 <- clean_data %>% 
  filter(Education == "Total") %>%
  ggplot(aes(x = Locality, y = Female, fill = Locality)) +
  geom_bar(stat = "identity") +
  labs(title = "Female Population Comparison",
       subtitle = "Urban vs Rural",
       y = "Percentage") +
  theme_minimal()

# 5. Comparison 5: Combined categories (excluding total & Never attended) Urban male vs Never attended urban male
combined_urban_male <- clean_data %>% 
  filter(Education != "Total", Education != "Never attended", Locality == "Urban") %>%
  summarise(Combined = sum(Male)) %>%
  pull(Combined)

p5_data <- data.frame(
  Category = c("Combined Education", "Never Attended"),
  Percentage = c(combined_urban_male, 
                 clean_data %>% filter(Education == "Never attended", Locality == "Urban") %>% pull(Male))
)

p5 <- ggplot(p5_data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Urban Male: Education Attainment vs Never Attended",
       y = "Percentage") +
  theme_minimal()

# 6. Comparison 6: Combined categories (excluding total & Never attended) rural female vs Never attended rural female
combined_rural_female <- clean_data %>% 
  filter(Education != "Total", Education != "Never attended", Locality == "Rural") %>%
  summarise(Combined = sum(Female)) %>%
  pull(Combined)

p6_data <- data.frame(
  Category = c("Combined Education", "Never Attended"),
  Percentage = c(combined_rural_female, 
                 clean_data %>% filter(Education == "Never attended", Locality == "Rural") %>% pull(Female))
)

p6 <- ggplot(p6_data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Rural Female: Education Attainment vs Never Attended",
       y = "Percentage") +
  theme_minimal()

# 7. Comparison 7: Rural - compare all educational levels for male and female (excluding total)
p7 <- clean_data %>% 
  filter(Education != "Total", Locality == "Rural") %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
  ggplot(aes(x = Education, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Educational Attainment in Rural Areas",
       subtitle = "Comparison by Gender",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. Comparison 8: Urban - compare all educational levels for male and female (excluding total)
p8 <- clean_data %>% 
  filter(Education != "Total", Locality == "Urban") %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
  ggplot(aes(x = Education, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Educational Attainment in Urban Areas",
       subtitle = "Comparison by Gender",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)