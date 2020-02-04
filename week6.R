library(tidyverse)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

NFL_teams <- dplyr::left_join(x = attendance, y = standings, by = c("year", "team_name", "team")) 

plot_data <- NFL_teams %>% 
  filter(grepl(pattern = 'Chargers|Rams', x = team_name)) %>% 
  filter(year > 2010)

avg_att <- plot_data %>% 
  group_by(team, team_name, year) %>% 
  summarise(avg_weekly_atd =  round(x = mean(x = weekly_attendance, na.rm = TRUE), digits = 0)) %>% 
  ungroup()

att_th <- 9e4
top_att <- plot_data %>% 
  filter(weekly_attendance > att_th) %>% 
  select(weekly_attendance)

top_att_games <- data.frame(team = character(), team_name = character(), year = double(), week = double(), weekly_attendance = double())
for (i in seq(from = 1, to = nrow(top_att), by = 1)){
  top_att_games <- rbind(top_att_games, data.frame(NFL_teams %>% 
                                                     filter(weekly_attendance == top_att$weekly_attendance[i]) %>% 
                                                     select(team, team_name, year, week, weekly_attendance) %>% 
                                                     arrange(desc(weekly_attendance)) %>% 
                                                     mutate(opp_team = case_when(weekly_attendance == lag(x = weekly_attendance, n = 1) ~ lag(x = team, n = 1),
                                                                                 weekly_attendance == lead(x = weekly_attendance, n = 1) ~ lead(x = team, n = 1))) %>% 
                                                     mutate(opp_team_name = case_when(weekly_attendance == lag(x = weekly_attendance, n = 1) ~ lag(x = team_name, n = 1),
                                                                                      weekly_attendance == lead(x = weekly_attendance, n = 1) ~ lead(x = team_name, n = 1))) %>% 
                                                     filter(grepl(pattern = 'Chargers|Rams', x = team_name))))
}

library(gganimate); library(ggrepel); library(extrafont)

plot <- ggplot(data = plot_data)+
  geom_point(mapping = aes(x = week, y = weekly_attendance, col = team), shape = 1)+
  geom_hline(mapping = aes(yintercept = avg_weekly_atd, col = team), linetype = 2, data = avg_att)+
  geom_text_repel(mapping = aes(x = 10, y = 3.5e4, label = paste('Average Weekly Attendance: ', avg_weekly_atd)), 
                  family = 'Bahnschrift', size = 3, data = avg_att, seed = 1008, segment.alpha = 0.4)+
  geom_text_repel(mapping = aes(x = week, y = weekly_attendance, label = paste('vs: ', opp_team_name)), 
                  family = 'Bahnschrift', size = 2.75, data = top_att_games, seed = 1008, segment.alpha = 0.4)+
  facet_wrap(facets = ~ team_name)+
  ggthemes::theme_tufte(base_size = 12, base_family = 'Bahnschrift')+
  labs(x = 'Game Week', y = 'Attendance', col = 'Location: ', 
       title = "Weekly NFL Attendances for Chargers and Rams Games, Year: {closest_state}",
       subtitle = 'Moving to LA was Deterimental for Attendances at Chargers Games \n and Beneficial for Attendances at Rams Games (Considering 2011 - 2019)',
       caption = 'Tidy Tuesday 2020, Week 6  |  Data from Pro Football Reference  |  @d73mwf')+
  theme(legend.position = 'top')+
  transition_states(states = year, transition_length = 1, state_length = 5, wrap = FALSE)+
  enter_fade() + exit_fade()

animate(plot = plot, fps = 20, duration = 10, end_pause = 3, width = 750, height = 500)

anim_save(animation = last_animation(), filename = 'week6.gif')
