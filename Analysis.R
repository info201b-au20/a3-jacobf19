install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(maps)
library(mapproj)
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

incarceration_trends <- incarceration_trends %>% 
  select(1,2,3,4,5,6,15,16,17,18,19,20,21,22,23,34,54)

incarceration_trends <- incarceration_trends %>%
  mutate(prop_in_pretrial = total_jail_pretrial/total_jail_pop)
incarceration_trends[, 'prop_in_pretrial']=round(incarceration_trends[, 'prop_in_pretrial'], 2)

incarceration_trends <- incarceration_trends %>% 
  mutate(prop_pop_prison_pop = total_prison_pop/total_pop)
incarceration_trends[, 'prop_pop_prison_pop']=round(incarceration_trends[, 'prop_pop_prison_pop'], 2)

incarceration_trends <- incarceration_trends %>% 
  mutate(prop_prison_pretrial = total_jail_pretrial/total_prison_pop)
incarceration_trends[, 'prop_prison_pretrial']=round(incarceration_trends[, 'prop_prison_pretrial'], 2)

median_prop_pretrial <- incarceration_trends %>% 
  filter(prop_in_pretrial == median(prop_in_pretrial, na.rm = TRUE)) %>% 
  select(prop_in_pretrial)

median_prop_pretrial <- incarceration_trends %>% 
  filter(prop_in_pretrial == median(prop_in_pretrial, na.rm = TRUE)) %>% 
  select(county_name, state)

max_jail_pop <- incarceration_trends %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  select(total_jail_pop)
max_jail_pop <- as.numeric(max_jail_pop)

max_jail_pop_county <- incarceration_trends %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  select(county_name)

max_jail_pop_year <- incarceration_trends %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  select(year)

max_total_pop <- incarceration_trends %>% 
  filter(total_pop == max(total_pop, na.rm = TRUE)) %>% 
  select(total_pop)
max_total_pop <- as.numeric(max_total_pop)

max_prison_pop <- incarceration_trends %>% 
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>% 
  select(total_prison_pop)
max_prison_pop <- as.numeric(max_prison_pop)

max_pretrial <- incarceration_trends %>%
  filter(total_jail_pretrial == max(total_jail_pretrial, na.rm = TRUE)) %>% 
  select(total_jail_pretrial)
max_pretrial <- as.numeric(max_pretrial)

max_pretrial_county <- incarceration_trends %>%
  filter(total_jail_pretrial == max(total_jail_pretrial, na.rm = TRUE)) %>% 
  select(county_name)

max_pretrial_year <- incarceration_trends %>%
  filter(total_jail_pretrial == max(total_jail_pretrial, na.rm = TRUE)) %>% 
  select(year)

total_state_pretrial_pop_2018 <- incarceration_trends %>% 
  filter(year == "2018")

total_state_pretrial_pop_sum <- total_state_pretrial_pop_2018 %>% 
  group_by(state) %>% 
  summarize(
    total_pretrial = sum(total_jail_pretrial, na.rm = TRUE)
  )

total_state_pretrial_pop_sum <- total_state_pretrial_pop_sum %>% 
  rename(state_init = state)

state_pretrial_sum <- total_state_pretrial_pop_sum %>% 
  filter(state_init != "AK")
state_pretrial_sum <- state_pretrial_sum %>% 
  filter(state_init != "HI")

state <- c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia", "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montanta", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")
state <- as.data.frame(state)

state_pretrial_sum <- state_pretrial_sum %>% 
  cbind(state)

incarceration_trends <- incarceration_trends %>% 
  rename("Total population in jail" = total_jail_pop, "Total population in pretrial" = total_jail_pretrial)

NY_Plot_Data <- incarceration_trends %>% 
  filter(county_name == "New York County")

NY_County_Prisons_Graph <- ggplot(NY_Plot_Data) +
  geom_point(mapping = aes(
    x = year, y = total_prison_pop
  )) +
  labs(
    title = "New York County Prison Population, 1970-2018",
    x = "Year",
    y = "Prison Population"
  )

bar_plot_data <- NY_Plot_Data %>% 
  select(2,13,16)

bar_plot_gathered <- bar_plot_data %>% 
  gather("Stat", "Value", -year)

bar_plot_jail_NY <- ggplot(bar_plot_gathered, aes(x = year, y = Value, fill = Stat)) +
  geom_col(position = "dodge") + labs(
    title = "Jail Population vs. Pretrial Population, New York County, by year",
    x = "Year",
    y = "Population"
  )

state_shape <- map_data("state") %>% 
  rename(state = region)
  left_join(state_pretrial_sum,state_shape, by = "state")

US_Pretrial_Map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_pretrial ),
    color = "white",
    size = .01
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Purple") +
  labs(fill = "Total Population in US States in Pretrial, 2018 (thousands)")
