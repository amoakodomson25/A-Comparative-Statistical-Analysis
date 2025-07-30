# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Read data
data_1718 <- read_excel("2017-2018 data set.xlsx", sheet = "17-18")
data_2024 <- read_excel("2017-2018 data set.xlsx", sheet = "2024")

# Add Year column
data_1718 <- data_1718 %>% mutate(Year = "2017-2018")
data_2024 <- data_2024 %>% mutate(Year = "2024")

# Combine datasets
combined_data <- bind_rows(data_1718, data_2024)

# Convert to numeric
combined_data <- combined_data %>%
  mutate(
    Female = as.numeric(Female),
    Male = as.numeric(Male)
  )

# Standard education level order
edu_levels <- c("Never attended", "Basic education", "Secondary/vocational", "Tertiary")

# =========================================================
# A. Education by Sex with % Change (Bar Chart)
# =========================================================

sex_edu <- combined_data %>%
  filter(
    !is.na(`Education Level`),
    `Education Level` != "Total",
    !is.na(Female),
    !is.na(Male)
  ) %>%
  group_by(Year, `Education Level`) %>%
  summarise(
    Female = sum(Female, na.rm = TRUE),
    Male = sum(Male, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Sex", values_to = "Value") %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

sex_edu_change <- sex_edu %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  filter(!is.na(`2024`) & !is.na(`2017-2018`)) %>%
  mutate(PercentChange = (`2024` - `2017-2018`) / `2017-2018` * 100)

ggplot(sex_edu, aes(x = `Education Level`, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sex) +
  labs(
    title = "Education Levels by Sex: 2017-2018 vs 2024",
    x = "Education Level",
    y = "Value",
    caption = "Percentage change annotated"
  ) +
  geom_text(
    data = sex_edu_change,
    aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3,
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  theme_minimal()

# =========================================================
# B. Education by Locality (Both Sexes) with % Change (Bar Chart)
# =========================================================

locality_edu <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total", !is.na(Locality)) %>%
  group_by(Year, `Education Level`, Locality) %>%
  summarise(Total = sum(Female + Male, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

locality_edu_change <- locality_edu %>%
  pivot_wider(names_from = Year, values_from = Total) %>%
  filter(!is.na(`2024`) & !is.na(`2017-2018`)) %>%
  mutate(PercentChange = (`2024` - `2017-2018`) / `2017-2018` * 100)

ggplot(locality_edu, aes(x = `Education Level`, y = Total, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Locality) +
  labs(
    title = "Education Levels by Locality: 2017-2018 vs 2024",
    x = "Education Level",
    y = "Total",
    caption = "Percentage change annotated"
  ) +
  geom_text(
    data = locality_edu_change,
    aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3,
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  theme_minimal()

# =========================================================
# C. Female Education by Locality with % Change (Bar Chart)
# =========================================================

female_locality <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total", Locality %in% c("Urban", "Rural")) %>%
  select(`Education Level`, Locality, Female, Year) %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

female_change <- female_locality %>%
  pivot_wider(names_from = Year, values_from = Female) %>%
  mutate(PercentChange = (`2024` - `2017-2018`) / `2017-2018` * 100)

ggplot(female_locality, aes(x = `Education Level`, y = Female, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Locality) +
  labs(
    title = "Female Education by Locality: 2017-2018 vs 2024",
    x = "Education Level",
    y = "Female",
    caption = "Percentage change annotated"
  ) +
  geom_text(
    data = female_change,
    aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3,
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  theme_minimal()

# =========================================================
# D. Male Education by Locality with % Change (Bar Chart)
# =========================================================

male_locality <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total", Locality %in% c("Urban", "Rural")) %>%
  select(`Education Level`, Locality, Male, Year) %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

male_change <- male_locality %>%
  pivot_wider(names_from = Year, values_from = Male) %>%
  mutate(PercentChange = (`2024` - `2017-2018`) / `2017-2018` * 100)

ggplot(male_locality, aes(x = `Education Level`, y = Male, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Locality) +
  labs(
    title = "Male Education by Locality: 2017-2018 vs 2024",
    x = "Education Level",
    y = "Male",
    caption = "Percentage change annotated"
  ) +
  geom_text(
    data = male_change,
    aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 3,
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  theme_minimal()


# =========================================================
# E. Gender Gap by Education Level and Year (Bar Chart)
# =========================================================

gender_gap <- combined_data %>%
  filter(
    !is.na(`Education Level`),
    `Education Level` != "Total"
  ) %>%
  group_by(Year, `Education Level`) %>%
  summarise(
    Male = sum(Male, na.rm = TRUE),
    Female = sum(Female, na.rm = TRUE),
    Gap = Male - Female,
    .groups = "drop"
  ) %>%
  mutate(
    `Education Level` = factor(`Education Level`, levels = edu_levels),
    GapLabel = ifelse(Gap > 0, paste0("+", round(Gap)), round(Gap))
  )

ggplot(gender_gap, aes(x = `Education Level`, y = Gap, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = GapLabel),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    color = "black",
    size = 3
  ) +
  labs(
    title = "Gender Gap in Education Levels: Male - Female",
    x = "Education Level",
    y = "Gap (Male - Female)",
    caption = "Positive = more males; Negative = more females"
  ) +
  theme_minimal()

# =========================================================
# F. Growth Rates by Education Level, Gender, and Locality
# =========================================================

# Filter and reshape for comparison
growth_data <- combined_data %>%
  filter(
    !is.na(`Education Level`),
    `Education Level` != "Total"
  ) %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Count") %>%
  group_by(`Education Level`, Locality, Gender, Year) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total) %>%
  mutate(
    `Growth Rate (%)` = round(((`2024` - `2017-2018`) / `2017-2018`) * 100, 1)
  )

# Set Education Level order
growth_data$`Education Level` <- factor(
  growth_data$`Education Level`,
  levels = c("Never attended", "Basic education", "Secondary/vocational", "Tertiary")
)

# Plotting: Facet by Gender, fill by Locality
ggplot(growth_data, aes(x = `Education Level`, y = `Growth Rate (%)`, fill = Locality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_wrap(~Gender) +
  geom_text(aes(label = paste0(`Growth Rate (%)`, "%")),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 5, # Increased text size
            fontface = "bold")+
  labs(
    title = "Growth Rates by Education Level, Locality, and Gender (2017–2018 to 2024)",
    x = "Education Level",
    y = "Growth Rate (%)",
    fill = "Locality"
  ) +
  theme_minimal()
