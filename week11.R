library(tidyverse)

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

n_selected <- 10

tidy_fees_exp <- tuition_cost %>% 
  dplyr::filter(degree_length == '4 Year') %>% 
  select(name, state, out_of_state_total, type) %>%
  arrange(out_of_state_total) %>% 
  na.omit() %>% 
  rowid_to_column() %>% 
  mutate(group = 
      case_when(rowid <= n_selected ~ 'Least Expensive',
                rowid > (nrow(x = tuition_cost %>%
                                dplyr::filter(degree_length == '4 Year') %>% 
                                select(name, state, out_of_state_total, type) %>%
                                na.omit()) - n_selected) ~ 'Most Expensive')
  ) %>% 
  na.omit()

fees_plot_exp <- ggplot(data = tidy_fees_exp %>%
                          dplyr::filter(group == 'Most Expensive'))+
  geom_col(mapping = aes(x = out_of_state_total, y = fct_inorder(f = name), fill = type), col = 'white')+
  geom_text(mapping = aes(x = 2e4, y = fct_inorder(f = name), label = state), col = 'white', family = 'Bahnschrift', size = 3)+
  theme_ddf_dark() + theme(axis.title = element_blank(), legend.position = 'none', 
                           axis.text.y = element_text(angle = 0), axis.text.x = element_blank(), 
                           plot.title = element_text(size = 10, margin = margin(b = 4)))+
  scale_x_continuous(limits = c(0, 8e4))+
  labs(title = 'Most Expensive Universities')+
  scale_fill_viridis_d()

fees_plot_chp <- ggplot(data = tidy_fees_exp %>%
                          dplyr::filter(group == 'Least Expensive'))+
  geom_col(mapping = aes(x = out_of_state_total, y = fct_inorder(f = name), fill = type), col = 'white')+
  geom_text(mapping = aes(x = 2e4, y = fct_inorder(f = name), label = state), col = 'white', family = 'Bahnschrift', size = 3)+
  theme_ddf_dark() + theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 10), legend.position = 'bottom', 
                           axis.text.y = element_text(angle = 0), plot.title = element_text(size = 10, margin = margin(b = 4)))+
  scale_x_continuous(limits = c(0, 8e4), name = 'Total Out of State Fees')+
  labs(title = 'Least Expensive Universities')+
  scale_fill_viridis_d()

tidy_salaries <- salary_potential %>%
  left_join(y = ggplot2::map_data('state') %>%
              mutate(region = str_to_title(region)) %>% 
              rename(state_name = region), by = 'state_name') %>%
  left_join(y = tuition_cost, by = 'name') %>% 
  group_by(state_code) %>% 
  mutate('Mid Career Salary' = mean(mid_career_pay), 'Early Career Salary' = mean(early_career_pay)) %>% 
  select(state_code, group, state_name, long, lat, 'Mid Career Salary', 'Early Career Salary') %>% 
  gather(key = 'Career Stage', value = 'Income', -c(state_code, group, state_name, long, lat))

salary_plot <- ggplot(data = ggplot2::map_data(map = 'state'),
                      mapping = aes(x = long, y = lat, group = group))+
  geom_polygon(data = tidy_salaries,
               mapping = aes(fill = Income), col = 'white')+
  theme_ddf_dark()+
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        legend.title = element_text(vjust = 1, size = 10), legend.position = 'bottom')+
  labs(fill = 'Salary, US $')+
  scale_fill_viridis_c()+
  facet_wrap(facets = ~ `Career Stage`, nrow = 2)+
  guides(fill = guide_colourbar(barwidth = 8, barheight = 1))
  
library(patchwork)

fees_plot <- (fees_plot_exp / fees_plot_chp)

univ_plot <- (salary_plot + fees_plot) +
  plot_layout(ncol = 2, widths = c(1, 1))+
  plot_annotation(title = 'Comparison of Fees and Graduate Salaries for Universities Across the USA',
                  caption = 'Tidy Tuesday 2020, Week 11  |  Data from TuitionTracker.org  |  @d73mwf')&
  theme(panel.background = element_rect(fill = 'grey20', colour = NULL), 
        text = element_text(family = 'Bahnschrift', colour = 'white'),
        rect = element_rect(fill = 'grey20', colour = NULL))

ggsave(filename = 'week11.png', plot = univ_plot, device = 'png', width = 8, height = 6, units = , dpi = 'retina')

