# Set Working Directory
setwd("~/GitHub/TidyTuesdays")

# Load Libraries
library(tidyverse); library(ggrepel); library(extrafont); library(ggthemes)

# Converssion factors consistent with data source: https://docs.google.com/spreadsheets/d/1cz7TDhm0ebVpySqbTvrHrD3WpxeyE4hLZtifWSnoNTQ/edit#gid=21
mins_to_sec <- 60; 
hours_to_mins <- 60; hours_to_sec <- hours_to_mins * mins_to_sec
days_to_hours <- 24; days_to_sec <- days_to_hours * hours_to_sec
weeks_to_days <- 7; weeks_to_sec <- weeks_to_days * days_to_sec
months_to_days <- 30.41666; months_to_sec <- months_to_days * days_to_sec
years_to_months <- 365.25; years_to_sec <- years_to_months * months_to_sec

# Read in data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>% 
  na.omit() %>% 
  mutate(category = str_to_title(category)) %>% 
  mutate(length = nchar(password)) %>%
  mutate(online_crack_sec = case_when(time_unit == 'seconds' ~ value,
                                      time_unit == 'minutes' ~ value * mins_to_sec,
                                      time_unit == 'weeks' ~ value * weeks_to_sec,
                                      time_unit == 'hours' ~ value * hours_to_sec,
                                      time_unit == 'days' ~ value * days_to_sec,
                                      time_unit == 'months' ~ value * months_to_sec,
                                      time_unit == 'years' ~ value * years_to_sec)) %>% 
  mutate(log_online_crack_sec = log10(online_crack_sec))

# Plot Parameters
y_upper <- log10(years_to_sec); y_lower <- log10(10 * mins_to_sec); space <- 2; label_size <- 2.75

# Creating Plot  
passwords_plot <- ggplot(data = passwords, mapping = aes(x = length, y = log_online_crack_sec))+
  geom_point(shape = 16, size = 1, alpha = 0.4)+
  geom_text_repel(data = passwords %>% # Add a label for difficult to crack passwords
              dplyr::filter(log_online_crack_sec > y_upper), 
              mapping = (aes(label = password)), seed = 1234,
              size = label_size, family = 'Bahnschrift', segment.size = 0.1, segment.alpha = 0.25, col = 'darkgreen')+
  geom_text_repel(data = passwords %>% # Add a label for easy to crack passwords
                     dplyr::filter(log_online_crack_sec < y_lower), 
                   mapping = (aes(label = password)), seed = 1,
                   size = label_size, family = 'Bahnschrift', segment.size = 0.1, segment.alpha = 0.25, col = 'darkred')+
  geom_hline(mapping = aes(yintercept = y_upper), linetype = 2, alpha = 0.5, col = 'darkgreen')+ # Draw the threshold for difficult to crack passwords
  geom_hline(mapping = aes(yintercept = y_lower), linetype = 2, alpha = 0.5, col = 'darkred')+ # Draw the threshold for easy to crack passwords
  facet_wrap(facets = ~category, nrow = 5)+
  theme_tufte(base_size = 11, base_family = 'Bahnschrift')+
  scale_x_continuous(name = 'Password Length', limits = c(min(passwords$length) - space, max(passwords$length) + space))+
  scale_y_continuous(name = expression(Log[10]("Time Taken to Guess Password", seconds)), 
                     limits = c(min(passwords$log_online_crack_sec) - space, max(passwords$log_online_crack_sec) + space))+
  labs(title = "Relationship Between Security and Length of Various Categories of Common Passwords",
       subtitle = expression(paste("'Online Guess Times' of  > 1 year and < 10 minutes are labelled in green and red, respectively.")),
       caption = "Tidy Tuesday 2020, Week 3  |  Data from  Information is Beautiful  |  @d73mwf")

ggsave(filename = 'week3.png', plot = passwords_plot, device = 'png', width = 8.27, height = 11.69, units = 'in', dpi = 'retina')