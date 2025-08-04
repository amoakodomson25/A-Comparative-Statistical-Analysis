library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# Load the dataset
data <- read_excel("2017-2018 data set.xlsx", sheet = "17-18")

# Clean column names
colnames(data) <- trimws(colnames(data))
colnames(data) <- gsub("[^A-Za-z0-9_]", "", colnames(data))
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Normalize and rename columns
data$Education.Level <- tolower(trimws(data$EducationLevel))
data$Locality <- tolower(trimws(data$Locality))

# ===============================
# PART 1: Combined Urban & Rural Gender Comparison (Total only)
# ===============================
filtered_data <- data[data$Education.Level == "total" & data$Locality %in% c("urban", "rural"), ]

urban_data <- filtered_data[filtered_data$Locality == "urban", ]
rural_data <- filtered_data[filtered_data$Locality == "rural", ]

if (nrow(urban_data) == 0 | nrow(rural_data) == 0) {
  stop("One of the groups (urban/rural) has no data. Please check the dataset values.")
}

combined_plot_data <- data.frame(
  Locality = rep(c("Urban", "Urban", "Rural", "Rural"), times = 1),
  Gender = rep(c("Male", "Female"), times = 2),
  Value = c(
    as.numeric(urban_data$Male),
    as.numeric(urban_data$Female),
    as.numeric(rural_data$Male),
    as.numeric(rural_data$Female)
  )
)

ggplot(combined_plot_data, aes(x = Gender, y = Value, fill = Locality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Urban vs Rural by Gender (2017–2018)",
    x = "Gender",
    y = "Percentage"
  ) +
  theme_minimal() +
  ylim(0, 100)

# ===============================
# PART 2: Never Attended vs Formal Education by Locality
# ===============================
relevant_levels <- c("never attended", "basic education", "secondary/vocational", "tertiary")
filtered_data_2 <- data[data$Education.Level %in% relevant_levels & data$Locality %in% c("urban", "rural"), ]

summary_data <- filtered_data_2 %>%
  group_by(Locality, Education.Level) %>%
  summarise(Value = sum(as.numeric(Male), na.rm = TRUE) + sum(as.numeric(Female), na.rm = TRUE), .groups = "drop")

wide_data <- pivot_wider(summary_data, names_from = Education.Level, values_from = Value)

wide_data <- wide_data %>%
  mutate(
    Formal.Education = `basic education` + `secondary/vocational` + `tertiary`,
    Never.Attended = `never attended`
  )

compare_data <- data.frame(
  Locality = rep(wide_data$Locality, each = 2),
  Category = rep(c("Never Attended", "Formal Education"), times = 2),
  Value = c(wide_data$Never.Attended, wide_data$Formal.Education)
)

ggplot(compare_data, aes(x = Locality, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  labs(
    title = "Never Attended vs Formal Education by Locality (2017–2018)",
    x = "Locality",
    y = "Percentage"
  ) +
  theme_minimal() +
  ylim(0, 100)

# ===============================
# PART 3: Education Levels by Gender and Locality
# ===============================
all_levels <- c("never attended", "basic education", "secondary/vocational", "tertiary")
edu_gender_data <- data %>%
  filter(Education.Level %in% all_levels & Locality %in% c("urban", "rural")) %>%
  mutate(Education.Level = factor(Education.Level, levels = all_levels))

# Plot for Males
male_data <- edu_gender_data %>%
  select(Education.Level, Locality, Male) %>%
  mutate(Value = as.numeric(Male)) %>%
  select(-Male)

ggplot(male_data, aes(x = Education.Level, y = Value, fill = Locality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Education Levels of Males by Locality (2017–2018)",
    x = "Education Level",
    y = "Percentage"
  ) +
  theme_minimal()

# Plot for Females
female_data <- edu_gender_data %>%
  select(Education.Level, Locality, Female) %>%
  mutate(Value = as.numeric(Female)) %>%
  select(-Female)

ggplot(female_data, aes(x = Education.Level, y = Value, fill = Locality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Education Levels of Females by Locality (2017–2018)",
    x = "Education Level",
    y = "Percentage"
  ) +
  theme_minimal()
