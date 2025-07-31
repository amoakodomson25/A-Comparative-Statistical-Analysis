# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(forcats)

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




# Dot chart with connecting lines for percentage change by Sex
ggplot(sex_edu_change, aes(x = `Education Level`, y = PercentChange, group = Sex, color = Sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(formatC(PercentChange, format = "f", digits = 1), "%")),
            vjust = -1, size = 4.5, color = "black") +
  facet_wrap(~ Sex) +
  labs(
    title = "Percentage Change in Education Levels by Sex (2017-2018 to 2024)",
    x = "Education Level",
    y = "Percentage Change",
    caption = "Percentage Change = ((2024 - 2017-2018) / 2017-2018) × 100"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 13),
    legend.position = "none"
  )





# Dot chart with connecting lines for percentage change by Locality
ggplot(locality_edu_change, aes(x = `Education Level`, y = PercentChange, group = Locality, color = Locality)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(formatC(PercentChange, format = "f", digits = 1), "%")),
            vjust = -0.8, size = 4.5, color = "black") +
  facet_wrap(~ Locality) +
  labs(
    title = "Percentage Change in Education Levels by Locality (2017-2018 to 2024)",
    x = "Education Level",
    y = "Percentage Change",
    caption = "Percentage Change = ((2024 - 2017-2018) / 2017-2018) × 100"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 13),
    legend.position = "none"
  )






gender_gap <- combined_data %>%
  filter(!is.na(`Education Level`), `Education Level` != "Total") %>%
  mutate(`Education Level` = factor(`Education Level`,
                                    levels = c("Never attended", "Basic education", 
                                               "Secondary/vocational", "Tertiary"))) %>%
  group_by(Year, `Education Level`) %>%
  summarise(Male = sum(Male, na.rm = TRUE),
            Female = sum(Female, na.rm = TRUE),
            Gap = Male - Female,
            .groups = "drop") %>%
  mutate(GapLabel = ifelse(Gap > 0, paste0("+", round(Gap, 1)), round(Gap, 1)))
ggplot(gender_gap, aes(x = `Education Level`, y = Gap, fill = Year)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = GapLabel), 
            position = position_dodge(width = 0.6), 
            vjust = ifelse(gender_gap$Gap >= 0, -0.5, 1.3), 
            size = 4.5, color = "black") +
  facet_wrap(~Year) +
  labs(title = "E: Gender Gap in Education Levels: Male - Female",
       subtitle = "Positive = more males; Negative = more females",
       y = "Gap (Male - Female)", x = "Education Level",
       caption = "Gender Gap = Total Male - Total Female") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size = 13))







gap_change <- gender_gap %>%
  select(Year, `Education Level`, Gap) %>%
  pivot_wider(names_from = Year, values_from = Gap) %>%
  mutate(Change =`2017-2018` - `2024` ,
         ChangeLabel = round(Change, 1))
# Order education levels if not already
gap_change$`Education Level` <- factor(gap_change$`Education Level`,
                                       levels = c("Never attended", "Basic education", "Secondary/vocational", "Tertiary"))

# Line chart of gender gap change
ggplot(gap_change, aes(x = `Education Level`, y = Change, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  geom_point(aes(color = Change > 0), size = 4) +
  geom_text(aes(label = sprintf("%.1f", Change)), vjust = -1, size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("red", "blue"), labels = c("Decrease", "Increase")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +  # Add space above the highest point
  labs(
    title = "Change in Gender Gap by Education Level (2024 - 2017-2018)",
    subtitle = "Positive = Shift toward females; Negative = Widened gap toward males",
    x = "Education Level", y = "Change in Gender Gap",
    color = "Direction of Change"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.position = "top"
  )
