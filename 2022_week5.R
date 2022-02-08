library(tidyverse); library(ggimage)

setwd("~/GitHub/TidyTuesdays")

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')

breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') |>
  mutate(across(starts_with('20'), ~ scales::rescale(.x, to = c(1, 0)))) |>
  tidyr::pivot_longer(cols = starts_with('20'), names_to = 'Year', values_to = 'Popularity') |>
  mutate(Year = gsub(pattern = ' Rank', replacement = '', x = Year))

trait_description |> View()

pup_df <- breed_traits |> 
  left_join(y = breed_rank_all |> dplyr::filter(Year == 2020) |> select(-c(Year)) |> rename('2020 Popularity' = Popularity), by = 'Breed')

affectionate_trainable <- pup_df |>
  dplyr::filter(`Affectionate With Family` == 5 & `Trainability Level` == 5) |> na.omit()

noise_df <- tibble(x_noise = rnorm(n = affectionate_trainable |> nrow(), mean = 0, sd = 1/2),
                   y_noise = rnorm(n = affectionate_trainable |> nrow(), mean = 0, sd = 1/2))

ggplot(data = cbind(affectionate_trainable, noise_df) |>
         mutate(`Energy Level` = scales::rescale(x = `Energy Level` + x_noise), 
                `Mental Stimulation Needs` = scales::rescale(x = `Mental Stimulation Needs` + y_noise)), 
       mapping = aes(x = `Energy Level`, y = `Mental Stimulation Needs`))+
  ggimage::geom_image(mapping = aes(image = Image), alpha = 1/4, size = 1/8)+
  ggrepel::geom_text_repel(mapping = aes(label = Breed), size = 4, family = 'Atkinson Hyperlegible', min.segment.length = 1)+
  geom_point(shape = 21, size = 2, mapping = aes(fill = `2020 Popularity`))+
  scale_fill_viridis_c(direction = -1)+
  scale_x_continuous(breaks = scales::pretty_breaks())+
  scale_y_continuous(breaks = scales::pretty_breaks())+
  labs(fill = '2020 Popularity', 
       title = 'Which are the neediest dog breeds?', 
       subtitle = 'Considering only the most affectionate and trainable, using standardised scales', 
       caption = 'Tidy Tuesday 2022, Week 5  |  Data from American Kennel Club  |  @Domenic_DF')+
  DomDF::theme_ddf_light(base_family = 'Atkinson Hyperlegible', base_size = 14) +
  theme(legend.title = element_text(size = 9), plot.subtitle = element_text(size = 12))+
  guides(fill = guide_colorbar(barwidth = 15, barheight = 1/2, title.position = 'left'))
