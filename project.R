# Setting working directory
setwd("~/Main Folder/Mission MS/Coursework/Winter 24/fds/FinalProject")

# Loading necessary libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries
library(dplyr)
library(RColorBrewer)
theme_set(theme_light())

# 2. Dataset
# - 2.1 Loading the data
data <- read.csv("master.csv", check.names = FALSE)

# - 2.2 Data Cleaning & Preprocessing

# a) Removing 'HDI for year' column due to over two-thirds missing values, making it unusable.
#    - Renaming 'gdp_for_year ($)' to 'gdp_for_year' .
#    - Renaming 'gdp_per_capita ($)' to 'gdp_per_capita'.
#    - Renaming 'country-year' to 'country_year' 
#    - Renaming 'suicides/100k pop' to suicides_per_100k
data <- data %>%
  select(-`HDI for year`) %>%
  rename(
    gdp_for_year = `gdp_for_year ($)`, 
    gdp_per_capita = `gdp_per_capita ($)`, 
    country_year = `country-year`,
    suicides_per_100k = `suicides/100k pop`
  )

# b) We should have 12 rows for every county-year combination (6 age bands * 2 genders)
# data %>%
#   group_by(country_year) %>%
#   count() %>%
#   filter(n != 12)

# Excluding data for the year 2016 due to issues with its completeness and reliability.
data <- data %>%
  filter(year != 2016) %>% # Remove rows where the year is 2016
  select(-country_year) # Drop the country_year column

# c) Excluding countries with <= 3 years of data:
# - Calculate the number of years of data available for each country, assuming each country should have data for 12 months each year.
# - Exclude countries with 3 or fewer years of data from the 'data' dataframe.
minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(),             
            years = rows / 12) %>%  
  filter(years > 3)               
data <- data %>%
  filter(country %in% minimum_years$country)

# d) Tidying the dataframe
data <- data %>%
  mutate(
    age = gsub(" years", "", age),    # Remove "years" from the age column
    sex = ifelse(sex == "male", "Male", "Female")  # Standardize the sex column values
  )

# e) Getting continent data
data <- data %>%
  mutate(
    continent = countrycode(sourcevar = country,
                            origin = "country.name",
                            destination = "continent")
  )

# f) Convert selected nominal variables to factors
# 'country', 'sex', and 'continent' are treated as nominal 
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], factor)

# g) Convert 'age' to an ordered factor
data$age <- factor(data$age, 
                   ordered = TRUE, 
                   levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+"))

# h) Convert 'generation' to an ordered factor
data$generation <- factor(data$generation, 
                          ordered = TRUE, 
                          levels = c("G.I. Generation", 
                                     "Silent", 
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

data <- as_tibble(data)

# i) Calculate the global average suicide rate per 100,000 people
global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000

# Function to calculate suicide rates
calc_suicide_rate <- function(data) {
  (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000
}

glimpse(data)

# 3 Global Analysis
# - 3.1 Global Trend
# Calculate yearly statistics and plot global suicides per 100k over time
yearly_stats <- data %>%
  group_by(year) %>%
  summarize(
    rate_per_100k = calc_suicide_rate(cur_data())
  )

ggplot(yearly_stats, aes(x = year, y = rate_per_100k)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = global_average, color = "grey35", linetype = 2, size = 1) +
  labs(
    title = "Trend of Global Suicide Rates Over Time",
    subtitle = "Annual suicide rates per 100,000 people from 1985 to 2015, compared to the global average",
    x = "Year",
    y = "Suicides per 100k") +
  scale_x_continuous(breaks = seq(1985, 2015, 2)) +
  scale_y_continuous(breaks = seq(10, 20))

# - 3.2 By Age
# Prepare data for plotting suicide rates by age
age_stats <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = calc_suicide_rate(cur_data()))

# Plot global suicides per 100k, categorized by age groups
age_plot <- ggplot(age_stats, aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Suicide Rate (per 100k) Globally, by Age Group",
       x = "Age Group", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = FALSE) 

# Prepare data for plotting trends over time by age
age_time_stats <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = calc_suicide_rate(cur_data()))

# Plot trends over time for suicide rates by age group
age_time_plot <- ggplot(age_time_stats, aes(x = year, y = suicide_per_100k, color = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() +  geom_point() + 
  labs(title = "Suicide Rate Trends (1985-2015), by Age Group ", 
       x = "Year", 
       y = "Suicides per 100k") + 
  theme(legend.position = "right") + scale_color_brewer(palette = "Paired") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = FALSE) 

grid.arrange(age_plot, age_time_plot, ncol = 2)

# - 3.3 By Sex
# Generate a bar plot of global suicide rates per 100k by sex
suicide_by_gender_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data())) %>%
  ggplot(aes(x = sex, y = suicide_rate, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Global Suicide Rates per 100k by Sex",
       x = "Sex", 
       y = "Suicide Rate per 100,000") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Female" = "darkorchid", "Male" = "seagreen")) +
  scale_y_continuous(breaks = seq(0, 25, 5), minor_breaks = FALSE)

# Generate a line plot of suicide rate trends by sex from 1985-2015
suicide_by_gender_trend_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data())) %>%
  ggplot(aes(x = year, y = suicide_rate, color = sex)) +
  facet_grid(sex ~ ., scales = "free_y") +
  geom_line() + geom_point() +
  labs(title = "Suicide Rate Trends by Sex (1985-2015)", 
       x = "Year", 
       y = "Suicide Rate per 100,000") +
  theme(legend.position = "right") +
  scale_color_manual(values = c("Female" = "darkorchid", "Male" = "seagreen")) +
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = FALSE)

grid.arrange(suicide_by_gender_plot, suicide_by_gender_trend_plot, ncol = 2)

# - 3.4 By Continent

# Generate a bar plot of global suicide rates per 100k by continent
continent_barplot <- data %>%
  group_by(continent) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data())) %>%
  ggplot(aes(x = continent, y = suicide_rate, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Global Suicide Rates per 100k by Continent",
       x = "Continent", 
       y = "Suicide Rate per 100k") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = FALSE)

# Generate a line plot of suicide rate trends by continent from 1985-2015
continent_trend_lineplot <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data())) %>%
  ggplot(aes(x = year, y = suicide_rate, color = continent)) +
  facet_grid(continent ~ ., scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(title = "Suicide Rate Trends by Continent (1985-2015)", 
       x = "Year", 
       y = "Suicide Rate per 100k") +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = FALSE)

grid.arrange(continent_barplot, continent_trend_lineplot, ncol = 2)

# - 3.5 By Country
# - 3.5.1 Overall 

# Calculate suicide rates by country and plot
ggplot(data %>%
         group_by(country, continent) %>%
         summarize(n = n(),
                   suicide_rate = calc_suicide_rate(cur_data())) %>%
         arrange(desc(suicide_rate)), 
       aes(x = reorder(country, suicide_rate), y = suicide_rate, fill = continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = global_average, linetype = "dashed", color = "grey35", size = 1) +
  labs(title = "Suicide Rates per 100k by Country",
       x = "Country", 
       y = "Suicide Rate per 100k") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "bottom")


# Heat map

# Summarize suicide rates by country and join to map data
suicide_rates_map <- data %>%
  group_by(country) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data())) %>%
  joinCountryData2Map(joinCode = "NAME", nameJoinColumn = "country")

# Plot the map with suicide rates
mapParams <- mapCountryData(
  suicide_rates_map,
  nameColumnToPlot = "suicide_rate",
  mapTitle = "Suicides by Country",
  colourPalette = "heat",
  oceanCol = "lightblue",
  missingCountryCol = "grey65",
  catMethod = "pretty",
)

# 3.6 Age difference, by Continent
# Prepare the summarized data for plotting
age_continent_suicides <- data %>%
  group_by(continent, age) %>%
  summarize(
    suicide_per_100k = calc_suicide_rate(cur_data())) %>%
  ungroup()

# Plot the data with ggplot
ggplot(age_continent_suicides, aes(x = continent, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Spectral") +
  geom_hline(yintercept = global_average, linetype = "dashed", color = "grey35", size = 1) +
  labs(
    title = "Suicide Rates per 100k by Age Group and Continent",
    subtitle = "Comparing the impact of age on suicide rates across continents",
    x = "Continent", 
    y = "Suicides per 100k", 
    fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "right")

# 3.7 Gender differences, by Continent
# Create a bar plot for suicide rates per 100k by sex and continent
ggplot(data %>%
         group_by(continent, sex) %>%
         summarize(suicide_per_100k = calc_suicide_rate(cur_data()), .groups = 'drop'),
       aes(x = continent, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = global_average, linetype = "dashed", color = "grey35", size = 1) +
  labs(title = "Suicide Rates per 100k by Sex Across Continents",
       subtitle = "Comparison of suicide rates by gender within each continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Sex") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "right")

# 3.8 Gender differences, by Country
# Calculate suicides per 100,000 for overall data by country and continent
overallSuicideRates <- data %>%
  group_by(country , continent) %>%
  summarize(
    suicide_per_100k = calc_suicide_rate(cur_data())) %>% ungroup()

# Calculate suicides per 100,000 by country, continent, and sex
suicideRatesBySex <- aggregate(cbind(suicides_no, population) ~ country + continent + sex, data, sum)
suicideRatesBySex$suicide_per_100k <- (as.numeric(suicideRatesBySex$suicides_no) / as.numeric(suicideRatesBySex$population)) * 100000

# Transform data from long to wide format
wideFormat <- reshape(suicideRatesBySex, idvar = c("country", "continent"), timevar = "sex", direction = "wide")
wideFormat <- wideFormat[order(wideFormat$suicide_per_100k.Male - wideFormat$suicide_per_100k.Female), ]

# Order countries in both datasets
wideFormat$country <- factor(wideFormat$country, ordered = TRUE, levels = wideFormat$country)
suicideRatesBySex$country <- factor(suicideRatesBySex$country, ordered = TRUE, levels = wideFormat$country)

# Plotting the data
gender_disparity_plot <- ggplot() + 
  geom_dumbbell(data = wideFormat, aes(y = country, x = suicide_per_100k.Female, xend = suicide_per_100k.Male), 
                color = "grey", size = 1) + 
  geom_point(data = suicideRatesBySex, aes(x = suicide_per_100k, y = country, color = sex), 
             size = 3) +
  geom_point(data = overallSuicideRates, aes(x = suicide_per_100k, y = country), 
             color = "darkblue", size = 1) + 
  geom_vline(xintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  scale_color_manual(values = c("Female" = "salmon", "Male" = "lightblue")) +
  theme(axis.text.y = element_text(size = 8), 
        legend.position = c(0.85, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  labs(title = "Gender Disparity in Suicides, by Country", 
       x = "Suicides per 100k", 
       y = "Country", 
       color = "Sex")

wideFormat$Male_Proportion <- wideFormat$suicide_per_100k.Male / (wideFormat$suicide_per_100k.Female + wideFormat$suicide_per_100k.Male)
wideFormat <- wideFormat[order(wideFormat$Male_Proportion), ]
suicideRatesBySex$country <- factor(suicideRatesBySex$country, ordered = TRUE, levels = wideFormat$country)

# Plotting the data as stacked bar chart showing the proportion of male and female suicides
gender_proportion_plot <- ggplot(suicideRatesBySex, aes(x = country, y = suicide_per_100k, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("Female" = "salmon", "Male" = "lightblue")) +  # Custom colors
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Proportions of Suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Percentage of Suicides",
       fill = "Sex")

grid.arrange(gender_disparity_plot, gender_proportion_plot, ncol = 2)

# 3.9 A Comparative Study of GDP Per Capita and Suicide Incidence
# - As a country gets richer, does it’s suicide rate decrease?

# Calculate mean GDP per capita for each country and year, 
# then calculate the correlation between year and GDP per capita for each country
country_year_gdp_corr <- data %>%
  group_by(country, year) %>%
  summarize(gdp_per_capita = mean(gdp_per_capita), .groups = 'drop') %>%
  group_by(country) %>%
  summarize(year_gdp_correlation = cor(year, gdp_per_capita), .groups = 'drop')

# Calculate the mean of the year and GDP per capita correlations across all countries
mean_correlation <- mean(country_year_gdp_corr$year_gdp_correlation, na.rm = TRUE)
cat("The mean correlation is:", mean_correlation, "\n")


# Linear Trend

# Summarize data by country and year
country_summary <- data %>%
  group_by(country, year) %>%
  summarize(
    suicide_rate = calc_suicide_rate(cur_data()),
    avg_gdp_per_capita = mean(gdp_per_capita)
  )

# Fit linear models and tidy the data
trend_analysis <- country_summary %>%
  ungroup() %>%
  nest(data = -country) %>%
  # for each item in 'data', fit a linear model
  mutate(
    lm_fit = map(data, ~ lm(suicide_rate ~ year, data = .)),
    lm_results = map(lm_fit, broom::tidy)
  ) %>%
  unnest(lm_results)

# Filter significant trends and adjust p-values
sig_trends <- trend_analysis %>%
  filter(term == "year") %>%
  mutate(p_adj = p.adjust(p.value, method = "holm")) %>%
  filter(p_adj < .05) %>%
  arrange(estimate) %>%
  mutate(country = factor(country, levels = country, ordered = TRUE))

# Plotting  trends
ggplot(sig_trends, aes(x = country, y = estimate, color = estimate)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  scale_color_gradient(low = "green", high = "red") +
  geom_segment(aes(y = 0, xend = country, yend = estimate), size = 1) +
  labs(
    title = "Annual Change in Suicide Rates per 100k",
    subtitle = "Countries with Significant Trends (p < 0.05)",
    x = "Country", y = "Annual Change (Suicides per 100k)"
  ) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), limits = c(-1.5, 1.5)) +
  theme(legend.position = "none") + coord_flip()


# - Suicide trends and GDP per capita
# Calculate global stats
global_stats <- data %>%
  group_by(year) %>%
  summarize(
    suicide_rate = calc_suicide_rate(cur_data()),
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)
  ) %>%
  ungroup()

# Add a 'Global' category to the global_stats
global_stats$continent <- 'GLOBAL'

# Combine the global stats with the continent stats
combined_stats <- data %>%
  group_by(year, continent) %>%
  summarize(
    suicide_rate = calc_suicide_rate(cur_data()),
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  bind_rows(global_stats) 

# Reorder so that 'Global' comes first
combined_stats$continent <- toupper(combined_stats$continent)
combined_stats$continent <- factor(combined_stats$continent,
                                   levels = c('GLOBAL', unique(combined_stats$continent[combined_stats$continent != 'GLOBAL'])))

# Determine the scaling factor for all data combined
max_gdp <- max(combined_stats$avg_gdp_per_capita, na.rm = TRUE)
max_suicide_rate <- max(combined_stats$suicide_rate, na.rm = TRUE)
scaling_factor <- max_suicide_rate / max_gdp

# Create the plots
ggplot(combined_stats, aes(x = year)) +
  geom_line(aes(y = avg_gdp_per_capita, color = "GDP per Capita"), linetype=1) +
  geom_line(aes(y = suicide_rate / scaling_factor, color = "Suicide Rate"), linetype=2) +
  geom_hline(yintercept = global_average / scaling_factor, linetype = "dashed", color = "grey") +
  labs(title = "Trend of Suicide Rates and GDP per Capita Over Time by Continent and Globally",
       subtitle = "Suicide rates per 100,000 people and GDP per capita",
       x = "Year", y = "GDP per Capita ($)",
       color = "Indicator") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * scaling_factor, name = "Suicide Rate per 100k")
  ) +
  facet_wrap(~ continent, scales = 'free_y') + 
  theme_minimal() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_color_manual(values = c("darkblue", "orange")) 

# Create a plot comparing suicide rate against GDP per capita
suicide_rate_plot <- data %>%
  group_by(country, gdp_per_capita) %>%
  summarize(suicide_rate = calc_suicide_rate(cur_data()), .groups = 'drop') %>%
  ggplot(aes(x = gdp_per_capita, y = suicide_rate)) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) + geom_point(alpha = 0.4) +
  labs(
    title = "Suicide Rate vs. GDP per capita",
    x = "GDP (per capita)",
    y = "Suicides per 100k")

# Compute the mean GDP per capita and suicide rates per 100k and create the plot
mean_gdp_suicide_rate_plot <- data %>%
  group_by(country, continent) %>%
  summarize(
    suicide_per_100k = calc_suicide_rate(cur_data()),
    gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    .groups = 'drop') %>%
  ggplot(aes(x = gdp_per_capita, y = suicide_per_100k, color = continent)) + geom_point() +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$"), breaks = seq(0, 70000, 10000)) +
  labs(
    title = "Correlation between average GDP (per capita) and Suicides per 100k",
    x = "GDP (per capita)",
    y = "Suicides per 100k",
    color = "Continent")

grid.arrange(suicide_rate_plot, mean_gdp_suicide_rate_plot, ncol = 2)


# Suicide Rates by Age Group in relation to GDP per Capita
data %>%
  group_by(country, age, gdp_per_capita) %>%
  summarize(suicide_per_100k = calc_suicide_rate(cur_data()), .groups = 'drop') %>%
  ggplot(aes(x = gdp_per_capita, y = suicide_per_100k)) +
  geom_point(alpha = 0.4, size = 1, colour = '#7650c5') +
  facet_wrap(~age) + scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme(strip.background=element_rect(fill="#7650c5")) +
  labs(
    title = "Suicide Rates by Age Group in relation to GDP per Capita",
    x = "GDP per capita ($)",
    y = "Suicide Rate per 100k population",
    caption = "Data grouped by country and age"
  )

# Suicide Rates in Relation to GDP per Capita by Gender
data %>%
  group_by(country, sex, gdp_per_capita) %>%
  summarize(suicide_per_100k = calc_suicide_rate(cur_data()), .groups = 'drop') %>%
  ggplot(aes(x = gdp_per_capita, y = suicide_per_100k)) +
  geom_point(alpha = 0.4, size = 1, colour = '#7650c5') +
  facet_wrap(~sex) + scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme(strip.background=element_rect(fill="#7650c5")) +
  labs(
    title = "Suicide Rates in Relation to GDP per Capita by Gender",
    x = "GDP per Capita ($)",
    y = "Suicides per 100k population",
    caption = "Data grouped by country and gender"
  )
