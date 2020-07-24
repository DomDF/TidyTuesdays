library(tidyverse); library(Rokemon)

# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 

clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3)),
         owners = as.factor(x = owners),
         release_date = lubridate::mdy(release_date),
         release_year = lubridate::year(release_date),
         release_decade = as.factor(paste0(signif(x = release_year, digits = 3), "'s")),
         normalised_log_median_playtime = scales::rescale(x = log(median_playtime)),
         price = case_when(
           is.na(price) ~ 0,
           TRUE ~ price
         ))

unique(clean_df$owners)

million_plus <- clean_df %>% 
  dplyr::filter(owners == unique(clean_df$owners)[1] |
                  owners == unique(clean_df$owners)[2] |
                  owners == unique(clean_df$owners)[3] |
                  owners == unique(clean_df$owners)[4] |
                  owners == unique(clean_df$owners)[11] |
                  owners == unique(clean_df$owners)[12] |
                  owners == unique(clean_df$owners)[13])

ten_million_plus <- clean_df %>% 
  dplyr::filter(owners == unique(clean_df$owners)[1] |
                  owners == unique(clean_df$owners)[11] |
                  owners == unique(clean_df$owners)[12] |
                  owners == unique(clean_df$owners)[13])

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

library(showtext)
font_add(family = 'GameBoy', regular = 'C:/Users/domen/Downloads/early_gameboy/Early GameBoy.ttf')

showtext::showtext_auto()

scores <- million_plus %>% 
  dplyr::filter(!is.na(release_year) & !is.na(metascore)) %>% 
  select(metascore)

max_score <- max(scores); min_score <- min(scores)

ggplot(data = million_plus %>% 
         dplyr::filter(!is.na(release_year) & !is.na(metascore)))+
         geom_point(mapping = aes(y = metascore, x = price), 
             shape = 15, size = 3, col = 'green4',  alpha = 0.6)+
  ggrepel::geom_text_repel(mapping = aes(y = metascore, x = price, label = game), 
                           data =  million_plus %>% 
                             dplyr::filter(!is.na(release_year) & !is.na(metascore)) %>% 
                             dplyr::filter(metascore == min_score),
                           family = 'GameBoy', size = 3, col = 'firebrick', seed = 1, segment.alpha = 0.4)+
  ggrepel::geom_text_repel(mapping = aes(y = metascore, x = price, label = game), 
                           data =  million_plus %>% 
                             dplyr::filter(!is.na(release_year) & !is.na(metascore)) %>% 
                             dplyr::filter(metascore == max_score),
                           family = 'GameBoy', size = 3, col = 'black', seed = 1, segment.alpha = 0.4)+
  facet_wrap(facets = ~ release_decade, ncol = 1)+
  Rokemon::theme_gameboy()+
  scale_y_continuous(name = 'Metacritic Score', limits = c(35, 100))+
  labs(x = 'Price, USD', 
       title = 'Videogames (with at least one million Steam downloads) Throughout the Decades', 
       subtitle = 'Top selling games are increasingly expensive, but have not risen in ratings',
       caption = 'TidyTuesday 2019 Week 31 - Data from Steam Spy - @d73mwf')+
  theme(text = element_text(family = 'GameBoy'), plot.title = element_text(family = 'GameBoy'))
