library(tidyverse)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

wwc_outcomes <- wwc_outcomes %>% 
  left_join(y = codes, by = 'team') %>% 
  group_by(year, yearly_game_id) %>% 
  mutate(game_goals = sum(score)) %>% 
  ungroup() %>% group_by(year, round) %>% 
  mutate(avg_goals_per_round = mean(game_goals)) %>% 
  ungroup() %>% group_by(year, team) %>% 
  mutate(team_tournament_goals = sum(score)) %>% 
  ungroup() %>% group_by(year, round, team) %>% 
  mutate(team_round_goals = sum(score)) %>% 
  ungroup() %>% group_by(year) %>% 
  mutate(tournament_goals = sum(score)) %>% 
  ungroup() %>% group_by(team) %>% 
  mutate(team_goals = sum(score)) %>% 
  ungroup() %>% mutate(team = fct_reorder(team, team_goals))

knock_outs <- wwc_outcomes %>% 
  dplyr::filter(round != 'Group')

label_tbl <- knock_outs %>% 
  group_by(year) %>% 
  mutate(stage = case_when(
    round == 'Round of 16' & win_status == 'Lost' ~ 'Last 16',
    round == 'Quarter Final' & win_status == 'Lost' ~ '1/4 Finalists',
    round == 'Third Place Playoff' & win_status == 'Lost' ~ 'Semi-Finalists',
    round == 'Third Place Playoff' & win_status == 'Won' ~ '3rd Place',
    round == 'Final' & win_status == 'Lost' ~ 'Finalists',
    round == 'Final' & win_status == 'Won' ~ 'Winners'
  )) %>% 
  ungroup() %>% 
  dplyr::filter(round != 'Group') %>% 
  group_by(year, team) %>% 
  mutate(ko_goals = sum(team_round_goals)) %>%
  ungroup() %>% 
  na.omit() %>% 
  mutate(colour = case_when(
    stage == 'Winners' ~ 'green3',
    stage != 'Winners' ~ 'white',
  )) %>% 
  mutate(size = case_when(
    stage == 'Winners' ~ 2.75,
    stage != 'Winners' ~ 2.25,
  ))

ggplot(data = knock_outs,
       mapping = aes(x = team_round_goals, y = team))+
  geom_col(mapping = aes(fill = team_goals), col = NA, alpha = 0.8)+
  geom_text(data = label_tbl, mapping = aes(x = 10, y = team, label = stage), col = label_tbl$colour,
            nudge_x = 0.5, family = 'Bahnschrift', size = label_tbl$size)+
  facet_wrap(facets = ~ year, scales = 'free_y', nrow = 2)+
  scale_fill_gradient(low = 'yellow', high = 'firebrick', breaks = c(30, 60, 90, 120))+
  theme_ddf_dark() + theme(axis.title = element_blank(), legend.title = element_text(size = 10))+
  guides(fill = guide_colourbar(barwidth = 12))+
  labs(title = "Goals Scored in Knock-Out Matches of Women's World Cup (WWC) Tournaments, 1991 \u279c 2019",
       subtitle = paste('As the Tournament Expanded and Introduced a Round of 16, More Teams Featured in the Knock-Out Stages', 
       '\n...But the Same Top Teams Have Consistently Reached the Later Stages'),
       fill = 'Total Number of WWC Tournament Goals Scored \n(Including Group Stage Games)',
       caption = 'Tidy Tuesday 2019, Week 28  |  Data from data.world  |  @d73mwf')

ggsave(filename = 'wwc.png', device = 'png', width = 9, height = 6, dpi = 'retina')