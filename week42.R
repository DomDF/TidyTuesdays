datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

library(tidyverse); library(extrafont)

datasets <- c('dino', 'star')

plot_df <- datasaurus %>% 
  dplyr::filter(dataset %in% datasets) %>% 
  group_by(dataset) %>% 
  mutate(rho = cor(x = x, y = y, method = 'pearson'), 
         mu_x = mean(x = x), sigma_x = sd(x = x), 
         mu_y = mean(x = y), sigma_y = sd(x = y)) %>% 
  ungroup() %>% 
  mutate(ds_id = as.factor(case_when(
    dataset == 'dino' ~ 'dataset 1',
    dataset == 'star' ~ 'dataset 2'
  )))

boxplot_df <- selected_ds %>% 
  tidyr::pivot_longer(cols = c(x, y), 
                      names_to = 'param')

label_df <- boxplot_df %>% 
  distinct(dataset, .keep_all = TRUE)

summary_stats <- ggplot(data = label_df)+
  geom_text(mapping = aes(x = 0, y = 0, 
                          label = paste0('Correlation co-efficent: ', signif(x = rho, digits = 2),
                                         '\n\n Mean(x): ', signif(x = mu_x, digits = 3), 
                                         '\n\n Std. Dev.(x): ', signif(x = sigma_x, digits = 3),
                                         '\n\n Mean(y): ', signif(x = mu_y, digits = 3), 
                                         '\n\n Std. Dev.(y): ', signif(x = sigma_y, digits = 3))),
            size = 3, family = 'Bahnschrift')+
  facet_wrap(facets = ~ ds_id, nrow = 2)+
  DomDF::theme_ddf_light()+
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks.length = unit(0,'mm'), 
        strip.text = element_text(color = 'white'))

boxplots <- ggplot(data = selected_ds %>% 
         tidyr::pivot_longer(cols = c(x, y), 
                             names_to = 'param'))+
  geom_boxplot(mapping = aes(y = value ))+
  facet_grid(ds_id ~ param)+
  DomDF::theme_ddf_light()+
  theme(axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.length.x = unit(0,'mm'), 
        strip.text.y = element_blank())

xy_plot <- ggplot(data = selected_ds, 
                  mapping = aes(x = x, y = y))+
  geom_point(alpha = 0.7)+
  facet_wrap(facets = ~ ds_id, nrow = 2)+
  DomDF::theme_ddf_light()+
  theme(axis.ticks.length = unit(0,'mm'),
        strip.text.x = element_text(color = 'white'))

library(patchwork)

comb_plot <- (summary_stats + boxplots + xy_plot)+
  plot_annotation(title = 'Plot your data!', 
                  subtitle = 'Summary statistics may not sufficiently characterise your datasets', 
                  caption = 'TidyTuesday 2020, Week 42    |    Data from http://www.thefunctionalart.com/    |    @Domenic_DF') &
  theme(plot.title = element_text(family = 'Bahnschrift'),
        plot.subtitle = element_text(family = 'Bahnschrift'), 
        plot.caption = element_text(family = 'Bahnschrift'))

ggsave(filename = 'week42.png', device = 'png', width = 9, height = 6, dpi = 'retina')