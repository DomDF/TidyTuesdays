# TidyTuesday 2020 Week 30
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md

library(tidyverse); library(extrafont); library(DomDF)

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
# animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
# brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

# Animals recieved into RSPCA Australia's animal shelters

animal_outcomes_tidy <- animal_outcomes %>% 
  tidyr::pivot_longer(cols = -c(year, animal_type, outcome), names_to = 'state_or_territory', values_to = 'count') %>% 
  mutate(state_or_territory_long = case_when(
    state_or_territory == 'ACT' ~ 'Australian Capital Territory',
    state_or_territory == 'NSW' ~ 'New South Wales',
    state_or_territory == 'NT' ~ 'Northern Territory',
    state_or_territory == 'QLD' ~ 'Queensland',
    state_or_territory == 'SA' ~ 'South Australia',
    state_or_territory == 'TAS' ~ 'Tasmania',
    state_or_territory == 'VIC' ~ 'Victoria',
    state_or_territory == 'WA' ~ 'Western Australia',
    TRUE ~ state_or_territory
  )) %>% 
  group_by(year, animal_type, state_or_territory) %>% 
  mutate(count_prop_by_state = count /  sum(count)) %>% 
  ungroup()

cats_and_dogs_NSW_QLD <- animal_outcomes_tidy %>% 
  dplyr::filter(animal_type == 'Cats' | animal_type == 'Dogs') %>% 
  dplyr::filter(state_or_territory == 'NSW' | state_or_territory == 'QLD') %>% 
  mutate(cat_image = 'https://upload.wikimedia.org/wikipedia/commons/5/5b/Cat_%2849952%29_-_The_Noun_Project.svg',
         dog_image = 'https://upload.wikimedia.org/wikipedia/commons/8/89/Dog.svg',
         QLD_image = 'https://upload.wikimedia.org/wikipedia/commons/0/04/Flag_of_Queensland.svg',
         NSW_image = 'https://upload.wikimedia.org/wikipedia/commons/4/47/New_South_Wales_Customs_House_Flag_%281832%29.svg')

library(ggimage)

RSPCA_plot <- ggplot(data = cats_and_dogs_NSW_QLD %>% 
         dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized'))+
  geom_line(mapping = aes(x = year, y = count_prop_by_state, col = outcome, linetype = outcome))+
  scale_color_manual(values = c('firebrick', 'forestgreen', 'blue4'))+
  geom_image(data = cats_and_dogs_NSW_QLD %>% 
               dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized') %>% 
               dplyr::filter(animal_type == 'Cats'), 
             mapping = aes(x = 2006, y = 0.8, image = cat_image),
             alpha = 0.4, size = 0.15)+
  geom_image(data = cats_and_dogs_NSW_QLD %>% 
               dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized') %>% 
               dplyr::filter(state_or_territory == 'QLD'), 
             mapping = aes(x = 2010, y = 0.8, image = QLD_image),
             alpha = 0.4, size = 0.15)+
  geom_image(data = cats_and_dogs_NSW_QLD %>% 
               dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized') %>% 
               dplyr::filter(animal_type == 'Dogs'), 
             mapping = aes(x = 2006, y = 0.8, image = dog_image),
             alpha = 0.4, size = 0.15)+
  geom_image(data = cats_and_dogs_NSW_QLD %>% 
               dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized') %>% 
               dplyr::filter(state_or_territory == 'NSW'), 
             mapping = aes(x = 2010, y = 0.8, image = NSW_image),
             alpha = 0.4, size = 0.15)+
  facet_grid(state_or_territory_long ~ animal_type)+
  scale_y_continuous(name = 'Percentage of total annual cases in state or territory, by animal type', 
                     limits = c(0, 1), labels = scales::label_percent())+
  # geom_text(data = cats_and_dogs_NSW_QLD %>% 
  #             dplyr::filter(outcome == 'Reclaimed' | outcome == 'Rehomed' | outcome == 'Euthanized') %>% 
  #             dplyr::filter(is.na(count_prop_by_state)),
  #             mapping = aes(x = year, y = 0, label = 'N/A'),
  #           family = 'Bahnschrift', size = 3)+
  DomDF::theme_ddf_light()+
  theme(axis.title.x = element_blank())+
  labs(title = "Cats & dogs recieved into RSPCA Australia's animal shelters", 
       subtitle = "The proportion of RSPCA Australia's cats and dogs being rehomed has increased in recent years. \nThere has been a corresponding reduction in the number being euthanized.", 
       caption = 'Tidy Tuesday 2020, Week 30   |   Data from the RSPCA   |   Images from Wikipedia   |   @d73mwf')

ggsave(filename = 'week30.png', device = 'png', width = 8, height = 8, dpi = 'retina')