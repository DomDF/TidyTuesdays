library(tidyverse); library(Rokemon); library(showtext); library(ggimage)

#font_add_google(name = 'Press Start 2P', family = 'Press_Start_2P')
showtext_auto()

mk_records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

three_lap_df <- mk_records %>% 
  dplyr::filter(shortcut == 'No', type == 'Three Lap') %>% 
  mutate(image = '~/mushroom.png')

ggplot(data = three_lap_df,
       mapping = aes(x = date, y = time))+
  ggimage::geom_image(mapping = aes(image = image), size = 0.075)+
  geom_vline(mapping = aes(xintercept = lubridate::dmy('01.01.2000')), 
             alpha = 1/2, lty = 2)+
  geom_line(alpha = 1/2)+
  scale_x_date(name = 'Date')+
  scale_y_continuous(name = '3 Lap Time (seconds)')+
  facet_wrap(facets = ~ track, scales = 'free_y', nrow = 4)+
  Rokemon::theme_gba()+
  labs(title = 'N64 Mario Kart World Record Track Times (Excluding Shortccuts)', 
       subtitle = ' - each mushroom represents a new world record \n - after steep increases in the 1990s, progress has since plateaued', 
       caption = 'Tidy Tuesday 2021, Week 22  |  Data from Mario Kart World Records  |  Image from mariokart.fandom.com  |  @Domenic_DF')+
  theme(text = element_text(family = 'Press_Start_2P', size = 9),
        plot.title = element_text(family = 'Press_Start_2P', size = 12, colour = 'white'),
        plot.subtitle = element_text(family = 'Press_Start_2P', size = 10, colour = 'white'))

ggsave(filename = '2021_week22.png', device = 'png', width = 12, height = 9, dpi = 'retina')
