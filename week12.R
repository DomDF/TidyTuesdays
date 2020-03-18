library(tidyverse); library(extrafont)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

data <- office_ratings %>%
  group_by(season) %>% 
  mutate(episode_prop = scales::rescale(x = episode, to = c(0, 1))) %>% 
  ungroup() %>% 
  rename(episode_name = title) %>% 
  tidyr::unite(episode_id, c(season, episode), sep = '', remove = FALSE) %>% 
  mutate(Season = paste('Season', season))

lin_mod <- lm(formula = imdb_rating ~ episode_prop, data = data)

library(lme4) # Allows for MLE of Linear Mixed Models (https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf)
pp_lin_mod <- lme4::lmer(formula = imdb_rating ~ 1 + episode_prop + (1 + episode_prop | season), data = data, REML = FALSE)

ind_model_df <- data.frame(Season = character(), intercept = double(), slope = double(), model = character()) -> mix_model_df

for (s in seq(from = 1, to = max(data$season), by = 1)){
  ind_mod <- lm(formula = imdb_rating ~ episode_prop, data = data %>% 
                     dplyr::filter(Season == paste('Season', s)))
  ind_model_df <- rbind(ind_model_df, data.frame(Season = paste('Season', s), 
                                                 intercept = ind_mod$coefficients['(Intercept)'], 
                                                 slope = ind_mod$coefficients['episode_prop'],
                                                 model = 'Independent Models'))
  
  mix_model_df <- rbind(mix_model_df, data.frame(Season = paste('Season', s), 
                                                 intercept = coef(object = pp_lin_mod)$season[s,]$`(Intercept)`, 
                                                 slope = coef(object = pp_lin_mod)$season[s,]$episode_prop,
                                                 model = 'Partial Pooling Model'))
}

pop_model_df <- data.frame(Season = ind_model_df$Season, 
                           intercept = lin_mod$coefficients['(Intercept)'], 
                           slope = lin_mod$coefficients['episode_prop'],
                           model = 'Aggregated Model')

model_df <- rbind(ind_model_df, mix_model_df, pop_model_df)

week12_plot <- ggplot(data = data, mapping = aes(x = episode_prop, y = imdb_rating))+
  geom_point(shape = 21, col = 'black', fill = 'white')+
  ggrepel::geom_text_repel(data = data %>% 
                              dplyr::filter(imdb_rating > 9 | imdb_rating < 7),
                            mapping = aes(x = episode_prop, y = imdb_rating, label = episode_name),
                            size = 3, family = 'Tahoma', col = 'darkblue', seed = 999)+
  geom_abline(data = model_df, mapping = aes(col = model, slope = slope, intercept = intercept), alpha = 0.8, size =  0.8)+
  #scale_linetype_manual(values = c(5, 1, 2))+
  scale_color_viridis_d()+
  facet_wrap(facets = ~ Season)+
  scale_x_continuous(name = 'Distance Through Season', labels = NULL)+
  scale_y_continuous(name = 'IMDB Rating')+
  ggthemes::theme_excel()+ theme(text = element_text(family = 'Tahoma'), panel.grid.major = element_line(linetype = 2, size = 0.25),
                                 legend.position = 'top', legend.title = element_blank())+
  #  theme_ddf_dark() + theme(axis.ticks.length.x = unit(0, 'pt'))+
  labs(title = 'Investigation of Whether Episodes of the Office Improved Within Seasons', 
       subtitle = 'An Aggegated Model Neglects Variation Between Seasons \nIndependent Models Neglect Commonality Between Seasons',
       caption = 'Tidy Tuesday 2020, Week 12  |  Data from github.com/rfordatascience/tidytuesday  |  @d73mwf')

ggsave(filename = 'week12.png', plot = week12_plot, device = 'png', width = 8, height = 8, units = , dpi = 'retina')

