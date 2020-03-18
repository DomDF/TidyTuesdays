library(tidyverse); library(extrafont)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

data <- office_ratings %>%
  group_by(season) %>% 
  mutate(prop = (episode-1) / max(episode-1)) %>% 
  ungroup() %>% 
  rename(episode_name = title) %>% 
  tidyr::unite(episode_id, c(season, episode), sep = '', remove = FALSE) %>% 
  mutate(Season = paste('Season', season))

ggplot(data = data)+
  geom_point(mapping = aes(x = prop, y = imdb_rating), 
             alpha = 0.6, shape = 21, col = 'black')+
  ggrepel::geom_text_repel(data = data %>% 
                             dplyr::filter(imdb_rating > 9 | imdb_rating < 7),
                           mapping = aes(x = prop, y = imdb_rating, label = episode_name),
                           size = 3, family = 'Bahnschrift', col = 'black')+
  geom_abline(slope = lin_mod$coefficients[2], intercept = lin_mod$coefficients[1], linetype = 2)+
  facet_wrap(facets = ~ Season)+
  #scale_fill_viridis_c()+
  theme_ddf_light()#+
  #guides(fill = guide_colourbar(barwidth = 15, barheight = 1))


library(lme4)

lin_mod_1 <- lm(formula = imdb_rating ~ prop, data = data %>% dplyr::filter(season == 1))

schrute::theoffice %>% 
  group_by(episode_name) %>% 
  summarise(season = season, episode = episode, )


comb_data <- schrute::theoffice %>% 
  mutate_at(.vars = c('season', 'episode'), .funs = as.numeric) %>%
  #mutate(episode = case_when(grepl(pattern = two_parters, x = episode_name) == TRUE ~ NA,
  #                           TRUE ~ episode))
  tidyr::unite(episode_id, c(season, episode), sep = '', remove = FALSE) %>% 
#  dplyr::filter(episode_name == 'Pilot') %>% 
  group_by(episode_id) %>% 
  count(character = character) %>% 
  ungroup() %>% 
  dplyr::rename(lines_of_dialogue = n) %>%
  # group_by('episode_name') %>% 
  # mutate(episode_id = case_when(grepl(pattern = 'Parts1&2', x = lag('episode_name')) == TRUE ~ max(1, episode_id),
  #                               TRUE ~ episode_id)) %>% 
  # ungroup()
  left_join(y = data, by = 'episode_id')

top_chars <- c('Michael', 'Dwight', 'Jim', 'Pam', 'Angela', 'Stanley', 'Kevin', 'Phyllis', 'Oscar', 'Kelly', 'Ryan')
  
ggplot(data = comb_data %>% 
         na.omit() %>% 
#         dplyr::filter(season < 3) %>%
         dplyr::filter(character == top_chars))+
  geom_col(mapping = aes(y = character, x = lines_of_dialogue, fill = imdb_rating), col = 'black')+
  scale_fill_viridis_c()+
  facet_wrap(facets = ~ season, nrow = 1, scales = 'fixed')+
  theme(legend.position = 'top')+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

  #facet_grid(rows = season ~ episode)+#, cols = ~season)+

