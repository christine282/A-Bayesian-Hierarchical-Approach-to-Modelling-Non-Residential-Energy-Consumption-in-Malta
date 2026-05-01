# Loading the required packages.
library(dbplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
options(scipen = 9999)

# Importing the required dataset. 
db <- read.csv("/Users/christinegrech/Desktop/Course/Year 4/Dissertation/Data/Final Data/data_final_to_use.csv", header = TRUE)

# How many companies are considered.
db %>% summarise(count = n_distinct(Company_id))

# Check: How many companies in each locality by NACE (letter)
summary_locality_nace_div <- db %>%
  distinct(Company_id, Locality_id, nace_section_letter) %>% 
  count(Locality_id, nace_section_letter, name = "n_companies") %>%
  arrange(Locality_id, nace_section_letter)

summary_locality_nace_div


# Descriptive Statistics.

quantitative_descriptives_tot <- db %>%
  summarise(
    # Sample size
    n = n(),

    # Consumption (kWh)
    mean_cons   = mean(Consumption_KWH, na.rm = TRUE),
    median_cons = median(Consumption_KWH, na.rm = TRUE),
    sd_cons     = sd(Consumption_KWH, na.rm = TRUE),
    min_cons    = min(Consumption_KWH, na.rm = TRUE),
    q1_cons     = quantile(Consumption_KWH, 0.25, na.rm = TRUE),
    q3_cons     = quantile(Consumption_KWH, 0.75, na.rm = TRUE),
    max_cons    = max(Consumption_KWH, na.rm = TRUE),

    # Company Size
    mean_company_size    = mean(Company_Size, na.rm = TRUE),
    median_company_size  = median(Company_Size, na.rm = TRUE),
    sd_company_size      = sd(Company_Size, na.rm = TRUE),
    min_company_size     = min(Company_Size, na.rm = TRUE),
    q1_company_size      = quantile(Company_Size, 0.25, na.rm = TRUE),
    q3_company_size      = quantile(Company_Size, 0.75, na.rm = TRUE),
    max_company_size     = max(Company_Size, na.rm = TRUE)
  )


# By Year
quantitative_descriptives <- db %>%
  group_by(Year) %>%
  summarise(
    # Sample size
    n = n(),
    
    # Consumption (kWh)
    mean_cons   = mean(Consumption_KWH, na.rm = TRUE),
    median_cons = median(Consumption_KWH, na.rm = TRUE),
    sd_cons     = sd(Consumption_KWH, na.rm = TRUE),
    min_cons    = min(Consumption_KWH, na.rm = TRUE),
    q1_cons     = quantile(Consumption_KWH, 0.25, na.rm = TRUE),
    q3_cons     = quantile(Consumption_KWH, 0.75, na.rm = TRUE),
    max_cons    = max(Consumption_KWH, na.rm = TRUE),
    
    # Company Size
    mean_company_size    = mean(Company_Size, na.rm = TRUE),
    median_company_size  = median(Company_Size, na.rm = TRUE),
    sd_company_size      = sd(Company_Size, na.rm = TRUE),
    min_company_size     = min(Company_Size, na.rm = TRUE),
    q1_company_size      = quantile(Company_Size, 0.25, na.rm = TRUE),
    q3_company_size      = quantile(Company_Size, 0.75, na.rm = TRUE),
    max_company_size     = max(Company_Size, na.rm = TRUE)
  )


# Descriptives by section & year
section_year_nace_stats <- db %>%
  group_by(Year, nace_section_letter) %>%
  summarise(
    n = n(),
    total_cons = sum(Consumption_KWH, na.rm = TRUE),
    mean_cons = mean(Consumption_KWH, na.rm = TRUE),
    sd_cons   = sd(Consumption_KWH, na.rm = TRUE),
    mean_company_size  = mean(Company_Size, na.rm = TRUE)
  ) %>%
  ungroup()


# Plot
library(ggrepel)

# Defining the colour scheme
nace_colors <- scales::hue_pal()(length(unique(section_year_nace_stats$nace_section_letter)))
names(nace_colors) <- sort(unique(section_year_nace_stats$nace_section_letter))

# 1. Prepare the label data (last year only)
label_data <- section_year_nace_stats %>%
  group_by(nace_section_letter) %>%
  filter(Year == max(Year))

# 2. Create the plot with "Infinity" overlaps and extra margin
main_plot <- ggplot(section_year_nace_stats, 
                    aes(x = Year, y = total_cons, color = nace_section_letter)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  
  geom_text_repel(
    data = label_data,
    aes(label = nace_section_letter),
    hjust = 0,
    nudge_x = 0.3,
    direction = "y",
    segment.color = 'grey80',
    segment.size = 0.2,
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = nace_colors) +  
  scale_x_continuous(breaks = 2019:2023, limits = c(2019, 2024)) +
  scale_y_continuous(labels = scales::comma) +
  
  theme_minimal() +
  labs(
    title = "Electricity Consumption Trends by NACE Section",
    y = "Total Consumption (kWh)",
    x = "Year"
  )

zoom_limit <- 6e7

zoom_labels <- label_data %>%
  filter(total_cons <= zoom_limit)

zoom_plot <- ggplot(section_year_nace_stats, 
                    aes(x = Year, y = total_cons, color = nace_section_letter)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  
  geom_text_repel(
    data = zoom_labels,
    aes(label = nace_section_letter),
    hjust = 0,
    nudge_x = 0.3,
    direction = "y",
    segment.color = 'grey80',
    segment.size = 0.2,
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  coord_cartesian(ylim = c(0, zoom_limit)) +   
  
  scale_color_manual(values = nace_colors) +  
  scale_x_continuous(breaks = 2019:2023, limits = c(2019, 2024)) +
  scale_y_continuous(labels = scales::comma) +
  
  theme_minimal() +
  labs(
    y = "Total Consumption (kWh)",
    x = "Year"
  )

main_plot
zoom_plot


# Descriptives by locality & year
section_year_loc_stats <- db %>%
  group_by(Year, Locality_id) %>%
  summarise(
    n = n(),
    total_cons = sum(Consumption_KWH, na.rm = TRUE),
    mean_cons = mean(Consumption_KWH, na.rm = TRUE),
    sd_cons   = sd(Consumption_KWH, na.rm = TRUE),
    mean_company_size  = mean(Company_Size, na.rm = TRUE)
  ) %>%
  ungroup()
