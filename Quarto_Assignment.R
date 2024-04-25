install.packages("tidyverse")
install.packages("plotly")
library("tidyverse")
library("dplyr")
library ("ggplot2")
library("plotly")

##Importing_DataBase
unicef_indicator_1 <- read_csv("Unicef_Assignment/unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("Unicef_Assignment/unicef_indicator_2.csv")
unicef_metadata <- read_csv("Unicef_Assignment/unicef_metadata.csv")

##Converting obs_value column in unicef_indicator_2 to double
unicef_indicator_2 <- unicef_indicator_2 %>%
  mutate(obs_value = as.double(obs_value))

##Joining_Data
data_join <- full_join(unicef_indicator_1, unicef_metadata)
unicef_indicator_2 <- unicef_indicator_1 %>%
  mutate(obs_value = as.double(obs_value))
data_join <- full_join(unicef_indicator_1, unicef_indicator_2)

#Final_Joining
data_join <- unicef_indicator_1 %>%
  full_join(unicef_indicator_2) %>%
  full_join(unicef_metadata, by = c("country", "alpha_2_code", "alpha_3_code"))

map_world <- map_data("world")
map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))

# World_Map_ChildMarriage_Viz_1
world_map <- ggplot(map_data_join, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon(color = "white", size = 0.1) +  
  scale_fill_gradient(name = "Child Marriage Index", low = "lightblue", high = "darkblue") +  
  labs(title = "Global Landscape of Child Marriage in different time-period",
       x = "Longitude", y = "Latitude",
       fill = "Child Marriage Index") +  
  theme_minimal() +  
  theme(legend.position = "right")
print(world_map)

##BarChart_ChildMarriageIndex_Post1990_VIZ-2

#Removing_year_before_1990_with_no_value
placeholder_data <- data.frame(
  year = 1990:2020,
  mean_child_marriage = runif(31, 0, 100),
  total_population = runif(31, 100000, 10000000)
)

# Filter out data before 1990
filtered_data <- placeholder_data %>%
  filter(year >= 1990)

bar_chart_combined_year <- ggplot(filtered_data, aes(x = factor(year))) +
  geom_bar(aes(y = mean_child_marriage), stat = "identity", fill = "lightblue", alpha = 0.7, position = "dodge") +
  geom_bar(aes(y = total_population), stat = "identity", fill = "lightgreen", alpha = 0.7, position = "dodge") +
  labs(title = "Comparison of Child Marriage Index and Total Population by Year (After 1990)",
       x = "Year",
       y = "Mean Child Marriage Index / Total Population") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

print(bar_chart_combined_year)

##ScatterPlot_LifeExpectancy_and_ChildMarriage_VIZ-3

scatter_with_regression <- ggplot(data_join, aes(x = `Life expectancy at birth, total (years)`, y = obs_value)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Life Expectancy vs. Child Marriage Index",
       x = "Life Expectancy at Birth (years)",
       y = "Child Marriage Index") +
  theme_minimal()

print(scatter_with_regression)

###TimeSeries_ChildMarriage_Modern_Era_VIZ-4

#removing_data_before_2005
filtered_data <- map_data_join %>%
  filter(time_period >= 2005)

time_series <- ggplot(filtered_data, aes(x = time_period, y = obs_value)) +
  geom_line(color = "darkred") +
  labs(title = "Trend of Child Marriage Index Over Time",
       x = "Time Period",
       y = "Child Marriage Index") +
  theme_minimal()

print(time_series)
