# Load Libraries
library(tidyverse); library(ggpointdensity); library(ggrepel); library(extrafont)

# Get the Data



# Value  = Time to crack the password by online guessing

y_crit <- 0.5

passwords <- na.omit(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')) %>%
  dplyr::mutate(category = str_to_title(category)) %>% 
  mutate(strength = (strength - mean(strength)))
  
ggplot(data = passwords)+
  geom_pointdensity(aes(x = strength, y = log(offline_crack_sec)), shape = 1, size = 3,  adjust = 1)+
  scale_color_gradient(low = 'steelblue', high = 'darkblue')+
  geom_label_repel(data = passwords %>% 
              dplyr::filter(log10(offline_crack_sec) > y_crit), 
              mapping = (aes(x = strength, y = log(offline_crack_sec), label = password)),
              size = 3, family = 'Trebuchet MS', seed = 123)+
  geom_hline(mapping = aes(yintercept = y_crit), linetype = 2.5)+
  facet_wrap(facets = ~category, nrow = 2)+
  scale_shape_manual(values=seq(from = 1, to = length(unique(passwords$category)), by = 1))+
  theme_bw(base_size = 10, base_family = 'Trebuchet MS')+
  theme(legend.position = 'top')+
  scale_x_continuous(name = 'Normalised Password Strength')+
  scale_y_continuous(name = expression(Log[e]('Time Taken to Crack Password Offline', seconds)))+
  ggtitle(label = 'Strength of Various Categories of Common Passwords', 
          subtitle = '')+
  labs(col = 'Data Point Density',
       caption = '@d73mwf')
  
