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

# A. Education by Sex
sex_edu <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total", !is.na(Female), !is.na(Male)) %>%
  group_by(Year, `Education Level`) %>%
  summarise(Female = sum(Female, na.rm = TRUE), Male = sum(Male, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Sex", values_to = "Value") %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

sex_edu_change <- sex_edu %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  filter(!is.na(`2024`) & !is.na(`2017-2018`)) %>%
  mutate(PercentChange = (`2024` - `2017-2018`) / `2017-2018` * 100)

ggplot(sex_edu, aes(x = `Education Level`, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sex) +
  geom_text(data = sex_edu_change,
            aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
            position = position_dodge(width = 0.9),
            size = 4.5, vjust = -0.5, inherit.aes = FALSE) +
  labs(title = "A: Education Levels by Sex: 2017-2018 vs 2024",
       x = "Education Level", y = "Value", caption = "Percentage change annotated") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# B. Education by Locality
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
  geom_text(data = locality_edu_change,
            aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
            position = position_dodge(width = 0.9),
            size = 4.5, vjust = -0.5, inherit.aes = FALSE) +
  labs(title = "B: Education Levels by Locality: 2017-2018 vs 2024",
       x = "Education Level", y = "Total", caption = "Percentage change annotated") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# C. Female by Locality
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
  geom_text(data = female_change,
            aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
            position = position_dodge(width = 0.9),
            size = 4.5, vjust = -0.5, inherit.aes = FALSE) +
  labs(title = "C: Female Education by Locality: 2017-2018 vs 2024",
       x = "Education Level", y = "Female", caption = "Percentage change annotated") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# D. Male by Locality
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
  geom_text(data = male_change,
            aes(x = `Education Level`, y = `2024`, label = paste0(round(PercentChange), "%")),
            position = position_dodge(width = 0.9),
            size = 4.5, vjust = -0.5, inherit.aes = FALSE) +
  labs(title = "D: Male Education by Locality: 2017-2018 vs 2024",
       x = "Education Level", y = "Male", caption = "Percentage change annotated") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# E. Gender Gap
gender_gap <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total") %>%
  group_by(Year, `Education Level`) %>%
  summarise(Male = sum(Male, na.rm = TRUE),
            Female = sum(Female, na.rm = TRUE),
            Gap = Male - Female,
            .groups = "drop") %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels),
         GapLabel = ifelse(Gap > 0, paste0("+", round(Gap)), round(Gap)))

ggplot(gender_gap, aes(x = `Education Level`, y = Gap, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = GapLabel),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4.5, color = "black") +
  labs(title = "E: Gender Gap in Education Levels: Male - Female",
       x = "Education Level", y = "Gap (Male - Female)",
       caption = "Positive = more males; Negative = more females") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# F. Growth Rate by Gender and Locality
growth_data <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total") %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Count") %>%
  group_by(`Education Level`, Locality, Gender, Year) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total) %>%
  mutate(`Growth Rate (%)` = round(((`2024` - `2017-2018`) / `2017-2018`) * 100, 1))

growth_data$`Education Level` <- factor(growth_data$`Education Level`, levels = edu_levels)

ggplot(growth_data, aes(x = `Education Level`, y = `Growth Rate (%)`, fill = Locality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_wrap(~ Gender) +
  geom_text(aes(label = paste0(`Growth Rate (%)`, "%")),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  labs(title = "F: Growth Rates by Education Level, Locality, and Gender (2017–2018 to 2024)",
       x = "Education Level", y = "Growth Rate (%)", fill = "Locality") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 13))

# ====================================
# Step G: Gender Disparity Index (GDI)
# ====================================

gdi_data <- combined_data %>%
  filter(
    !is.na(`Education Level`),
    `Education Level` != "Total"
  ) %>%
  group_by(Year, Locality, `Education Level`) %>%
  summarise(
    Male = sum(Male, na.rm = TRUE),
    Female = sum(Female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(GDI = round(Female / Male, 2))

# Set factor levels again
gdi_data$`Education Level` <- factor(
  gdi_data$`Education Level`,
  levels = c("Never attended", "Basic education", "Secondary/vocational", "Tertiary")
)

# Plot Gender Disparity Index
ggplot(gdi_data, aes(x = `Education Level`, y = GDI, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~Locality) +
  geom_text(
    aes(label = GDI),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4,
    family = "sans"
  ) +
  labs(
    title = "G: Gender Disparity Index by Education Level and Locality",
    subtitle = "GDI = Female / Male (Closer to 1 means parity)",
    x = "Education Level",
    y = "Gender Disparity Index",
    fill = "Year",
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


# ====================================
# PART H: Change in Gender Parity Over Time
# ====================================

# Step 1: Calculate parity ratio (Female / Male) per Year and Education Level
parity_data <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total") %>%
  group_by(Year, `Education Level`) %>%
  summarise(
    Total_Female = sum(Female, na.rm = TRUE),
    Total_Male = sum(Male, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(GenderParity = round(Total_Female / Total_Male, 2))

# Step 2: Reshape to wide format to calculate change
parity_change <- parity_data %>%
  pivot_wider(names_from = Year, values_from = GenderParity,
              names_prefix = "Parity_") %>%
  mutate(Change = Parity_2024 - `Parity_2017-2018`)

# Step 3: Reshape to long for plotting
parity_long <- parity_data %>%
  mutate(`Education Level` = factor(`Education Level`, levels = edu_levels))

# Step 4: Plot
ggplot(parity_long, aes(x = `Education Level`, y = GenderParity, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = parity_change,
            aes(x = `Education Level`, y = pmax(Parity_2024, `Parity_2017-2018`),
                label = paste0("Δ ", round(Change, 2))),
            position = position_dodge(width = 0.9),
            vjust = -0.6,
            inherit.aes = FALSE,
            size = 4.5,
            color = "black") +
  labs(title = "H. Change in Gender Parity Over Time (Female / Male)",
       x = "Education Level", y = "Gender Parity Ratio",
       caption = "Δ = Change in ratio from 2017–2018 to 2024") +
  scale_fill_manual(values = c("2017-2018" = "steelblue", "2024" = "darkorange")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))




