# assignment-stats
library(tidyverse)
library(lubridate)
library(readxl)
# libraries for visualization
library(ggplot2)
library(scales)
library(patchwork)

# 1. Data Cleaning and Preparation
# Fix the file path (use forward slashes or double backslashes)
data <- read_excel("C:/Users/annur/OneDrive - Universiti Teknologi PETRONAS/1st Year 3rd Sem/Statistics/Assignment/statistics_assignment_dataset.xlsx")

# Check the structure of the data
str(data)

# Clean column names (remove special characters to make it consistent)
names(data) <- tolower(names(data))  # Make all column names lowercase
names(data) <- gsub("[^a-z0-9]", "_", names(data))  # Replace special chars with underscore

# Fix the participant_id column 
data <- data %>% 
  rename(row_num = 1, participant_id = 2) %>% 
  select(-row_num)

# Convert date to proper Date format (mm/dd/yyyy format)
data$date <- mdy(data$date)

# Check for missing values
sum(is.na(data))

# Remove duplicates 
data <- data %>% 
  distinct(participant_id, date, .keep_all = TRUE)

# Check for outliers in daily steps and sleep hours
boxplot(data$daily_steps, main = "Boxplot of Daily Steps")
boxplot(data$sleep_hours, main = "Boxplot of Sleep Hours")

# 2. Classify participants into lifestyle categories

# Calculate average daily steps per participant
participant_avg <- data %>%
  group_by(participant_id, age, gender) %>%
  summarise(
    avg_daily_steps = mean(daily_steps, na.rm = TRUE),
    avg_sleep_hours = mean(sleep_hours, na.rm = TRUE),
    observation_count = n(),
    .groups = 'drop'  # Added to avoid warning
  )

# Classify into lifestyle categories
participant_avg <- participant_avg %>%
  mutate(lifestyle = case_when(
    avg_daily_steps < 5000 ~ "Sedentary",
    avg_daily_steps >= 5000 & avg_daily_steps < 10000 ~ "Moderately Active",
    avg_daily_steps >= 10000 ~ "Active",
    TRUE ~ "Unknown"
  ))

# Convert lifestyle to factor with ordered levels
participant_avg$lifestyle <- factor(
  participant_avg$lifestyle,
  levels = c("Sedentary", "Moderately Active", "Active"),
  ordered = TRUE
)

# View the results
head(participant_avg)

# lifestyle categories summary
lifestyle_summary <- participant_avg %>%
  group_by(lifestyle) %>%
  summarise(
    count = n(),
    percentage = n() / nrow(participant_avg) * 100,
    avg_steps = mean(avg_daily_steps),
    avg_sleep = mean(avg_sleep_hours)
  )

print(lifestyle_summary)

# Visualize the results
# Prepare the data for visualization
lifestyle_vis <- participant_avg %>%
  group_by(lifestyle) %>%
  summarise(
    count = n(),
    avg_sleep = mean(avg_sleep_hours),
    .groups = 'drop'
  ) %>%
  mutate(
    prop = count / sum(count),
    sleep_prop = avg_sleep / sum(avg_sleep)
  )

# 1. Lifestyle Distribution Pie Chart
p1 <- ggplot(lifestyle_vis, aes(x = "", y = prop, fill = lifestyle)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percent(prop, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Participant Lifestyle Distribution",
       fill = "Lifestyle") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "right")

# 2. Sleep Proportion Donut Chart
p2 <- ggplot(lifestyle_vis, aes(x = 2, y = avg_sleep, fill = lifestyle)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(sleep_prop*100, 1), "%\n(", round(avg_sleep, 1), " hrs)")), 
            position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Average Sleep Distribution by Lifestyle",
       fill = "Lifestyle") +
  xlim(0.5, 2.5) + # Creates the donut hole
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "right")

# Combine plots
combined <- p1 + p2 + 
  plot_annotation(title = "Fitness Data Analysis",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))

print(combined)