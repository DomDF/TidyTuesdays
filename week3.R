# Set Working Directory
setwd("~/GitHub/TidyTuesdays")

# Load Libraries
library(tidyverse); library(ggrepel); library(extrafont); library(ggthemes)

# Read in data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>%
  na.omit() %>% 
  mutate(category = str_to_title(category)) %>% 
  mutate(length = nchar(password))

# Plot Parameters
y_upper <- 0.5; y_lower <- -12; space <- 2; label_size <- 2.75; set.seed(1008)

# Creating Plot  
passwords_plot <- ggplot(data = passwords)+
  geom_point(aes(x = length, y = log(offline_crack_sec)), shape = 16, size = 1, alpha = 0.2)+
  geom_text_repel(data = passwords %>% # Add a label for difficult to crack passwords
              dplyr::filter(log(offline_crack_sec) > y_upper), 
              mapping = (aes(x = length, y = log(offline_crack_sec), label = password)),
              size = label_size, family = 'Bahnschrift', segment.size = 0.1, segment.alpha = 0.25,
              col = 'darkgreen')+
  geom_text_repel(data = passwords %>% # Add a label for easy to crack passwords
                     dplyr::filter(log(offline_crack_sec) < y_lower), 
                   mapping = (aes(x = length, y = log(offline_crack_sec), label = password)),
                   size = label_size, family = 'Bahnschrift', segment.size = 0.1, segment.alpha = 0.25,
                   col = 'darkred')+
  geom_hline(mapping = aes(yintercept = y_upper), linetype = 2, alpha = 0.5, col = 'darkgreen')+ # Draw the threshold for difficult to crack passwords
  geom_hline(mapping = aes(yintercept = y_lower), linetype = 2, alpha = 0.5, col = 'darkred')+ # Draw the threshold for easy to crack passwords
  facet_wrap(facets = ~category, nrow = 5)+
  theme_tufte(base_size = 11, base_family = 'Bahnschrift')+
  scale_x_continuous(name = 'Password Length', limits = c(min(passwords$length)-space, max(passwords$length) + space))+
  scale_y_continuous(name = expression(Log[]("Time Taken to Crack Password Offline", seconds)), 
                     limits = c(min(log(passwords$offline_crack_sec)) - space, max(log(passwords$offline_crack_sec)) + space))+
  ggtitle(label = "Relationship Between Security and Length of Various Categories of Common Passwords", 
          subtitle = expression(paste("'Crack Times' of  > 1 second and < ", 10^-12, " seconds are labelled in green and red, respectively")))+
  labs(caption = "Tidy Tuesday 2020, Week 3. Data from  Information is Beautiful. @d73mwf")

ggsave(filename = 'week3.png', plot = passwords_plot, device = 'png', width = 8.27, height = 11.69, units = 'in', dpi = 'retina')