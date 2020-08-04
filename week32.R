# Tidy Tuesday 2020, Week 32: European energy

library(tidyverse); library(DomDF)

country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv') %>% 
  tidyr::pivot_longer(cols = -c(country, country_name, type, level), 
                      names_to = 'year', values_to = 'total_GWhrs') %>% 
  rename(production_type = type,
         production_level = level)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv') %>% 
  tidyr::pivot_longer(cols = -c(country, country_name, type, level),
                      names_to = 'year', values_to = 'GWhrs') %>% 
  group_by(country, year) %>% 
  mutate(GWhrs_by_country_by_year = sum(GWhrs)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(GWhrs_by_country = sum(GWhrs)) %>% 
  ungroup() %>% 
  mutate(subtype = case_when(
    type == 'Hydro' | type == 'Pumped hydro power' | type == 'Wind' | type == 'Solar' | type == 'Geothermal' ~ 'Renewable',
    type == 'Nuclear' ~ 'Non-Renewable Clean',
    type == 'Conventional thermal' ~ 'Fossil Fuels',
    TRUE ~ 'Unknown'),
    country_name = case_when(
    country == 'UK' ~ 'UK',
    country == 'EL'~ 'Greece', # https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Country_codes
    TRUE ~ country_name
  ))

n_countries <- 8

top_producers <- energy_types %>% 
  distinct(GWhrs_by_country, .keep_all = TRUE) %>% 
  arrange(desc(GWhrs_by_country)) %>% 
  top_n(n = n_countries, wt = GWhrs_by_country) %>% 
  select(country)

energy_type_plot <- ggplot(data = energy_types %>% 
         dplyr::filter(country %in% top_producers$country) %>% 
         mutate(country_name = forcats::fct_reorder(.f = as.factor(country_name),
                                               .x = desc(GWhrs_by_country))))+
  geom_bar(mapping = aes(x = country_name, y = GWhrs, fill = subtype), 
           position = 'stack', stat = 'identity', col = NA)+
#  facet_wrap(facets = ~ year, ncol = 1)+
  scale_fill_viridis_d()+
  scale_y_continuous(name = 'Gigawatt hours produced', labels = scales::comma)+
  theme_ddf_dark()+
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
        plot.background = element_rect(colour = 'grey20', fill = 'grey20'))

renewables <- energy_types %>%
  dplyr::filter(subtype == 'Renewable') %>% 
  group_by(type) %>% 
  mutate(GWhrs_by_type = sum(GWhrs)) %>% 
  ungroup() %>% 
  distinct(type, .keep_all = TRUE) %>% 
  select(type, GWhrs_by_type) %>% 
  mutate(prop_GWhrs = GWhrs_by_type / sum(GWhrs_by_type))

renewables_plot <- ggplot(data = renewables %>% 
         mutate(type = forcats::fct_reorder(.f = as.factor(type),
                                            .x = desc(prop_GWhrs))))+
  geom_col(mapping = aes(x = type, y = prop_GWhrs), col = NA)+
  geom_text(mapping = aes(x = type, y = prop_GWhrs + 0.025, 
                          label = paste(signif(x = 100 * prop_GWhrs, digits = 3), '%')),
            family = 'Bahnschrift', size = 3.5)+
  theme_ddf_dark()+
  theme(axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(colour = 'grey20', fill = 'grey20'))+
  labs(subtitle = 'Renewable production')

energy_prop_plot <- ggplot(data = energy_types %>% 
                             dplyr::filter(country %in% top_producers$country) %>% 
                             group_by(subtype, year) %>% 
                             mutate(GWhrs_by_subtype_by_year = sum(GWhrs)) %>% 
                             ungroup() %>% 
                             distinct(GWhrs_by_subtype_by_year, .keep_all = TRUE) %>% 
                             select(year, subtype, GWhrs_by_subtype_by_year) %>% 
                             group_by(year) %>% 
                             mutate(prop_GWhrs_by_subtype_by_year = GWhrs_by_subtype_by_year / sum(GWhrs_by_subtype_by_year)) %>% 
                             ungroup(),
                           mapping = aes(x = as.integer(year), y = prop_GWhrs_by_subtype_by_year, fill = as.factor(subtype)))+
  geom_area(position = position_stack())+
  scale_x_continuous(breaks = c(2016, 2017, 2018))+
  scale_y_continuous(labels = scales::percent)+
  # geom_point(mapping = aes(fill = subtype), col = 'white', shape = 21, size = 3, alpha = 0.4)+
  # geom_line(mapping = aes(col = subtype, group = subtype))+
  scale_color_viridis_d() + scale_fill_viridis_d()+
  theme_ddf_dark()+
  theme(plot.background = element_rect(colour = 'grey20', fill = 'grey20'), 
        legend.position = 'none',
        axis.title = element_blank())

library(patchwork)

RHS <- (energy_prop_plot / renewables_plot)

univ_plot <- (energy_type_plot + RHS) +
  plot_layout(ncol = 2, widths = c(4/3, 1))+
  plot_annotation(title = 'Electricity generation in the highest producing countries in Europe (2016 - 2018)',
                  subtitle = ' -  France dominates non-renewable clean (nuclear) energy production \n -  There was a rise in percentage of energy produced using clean technology in 2018 \n -  Renewable energy in Europe is primarily generated from hydropower and wind',
                  caption = 'Tidy Tuesday 2020, Week 32  |  Data from Eurostat  |  @Domenic_DF')&
  theme(panel.background = element_rect(fill = 'grey20'), 
        text = element_text(family = 'Bahnschrift', colour = 'white'),
        plot.background = element_rect(colour = 'grey20', fill = 'grey20'))

univ_plot

ggsave(filename = 'week32.png', device = 'png', width = 10, height = 8, dpi = 'retina')

