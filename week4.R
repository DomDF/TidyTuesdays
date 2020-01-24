library(tidyverse); library(extrafont); library(ggthemes); library(scales)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

top_artists <- spotify_songs %>% 
  distinct(track_id, .keep_all = TRUE) %>% 
  group_by(track_artist) %>%
  summarise(n_songs = length(unique(track_name)), mean_pop = mean(track_popularity),
            dance = mean(danceability), tempo = mean(tempo), loud = mean(loudness),
            length = mean(duration_ms), speech = mean(speechiness), acoustic = mean(acousticness),
            valence = mean(valence), instr. = mean(instrumentalness)) %>% 
  mutate(dance =  rescale(dance), tempo = rescale(tempo), loud = rescale(loud),
         length = rescale(length), speech = rescale(speech), acoustic = rescale(acoustic),
         valence = rescale(valence), instr. = rescale(instr.)) %>%
  filter(n_songs >= 5 & mean_pop > 0) %>% 
  top_n(wt = mean_pop, n = 8) %>% 
  arrange(desc(mean_pop)) %>% 
  gather(key = parameter, value = value, -track_artist) %>%
  filter(!grepl(pattern = '_', x = parameter)) %>% 
  mutate(parameter = str_to_title(parameter))

song_plot <- ggplot(data = top_artists, mapping = aes(x = parameter, y = value, fill = track_artist))+
  geom_col(col = 'black', alpha = 0.4)+
  coord_polar()+
  facet_wrap(facets = ~ track_artist, ncol = 4)+
  ggthemes::theme_base(base_size = 10, base_family = 'Bahnschrift')+
  theme(axis.line = element_blank(), line = element_blank(), rect = element_blank(), 
        axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size = 6),
        legend.position = 'none')+
  scale_y_continuous(breaks = NULL)+
  labs(title = 'Song Characteristics of Most Popular Spotify Artists',
       subtitle = 'Considering Average Characteristics from Artists with at Least 5 Separate Songs Featured',
       caption = 'Tidy Tuesday 2020, Week 4  |  Data from SpotifyR  |  @d73mwf')

ggsave(filename = 'week4.png', plot = song_plot, device = 'png', width = 7, height = 5, units = , dpi = 'retina')

