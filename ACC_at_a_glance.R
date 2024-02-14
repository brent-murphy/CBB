library(cbbplotR)
library(cbbdata)
library(tidyverse)
library(gt)

acc_team_data <- cbd_torvik_ratings(year = 2024, conf = 'ACC')

acc_team_data %>% 
  ggplot(aes(adj_o, adj_d, team = team)) +
  geom_cbb_teams(width = 0.10) +
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d), color = 'black') +
  theme_minimal() +
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold')) +
  labs(title = 'Adjusted Efficiencies in the ACC',
       x = 'Adjusted Offense',
       y = 'Adjusted Defense')

acc_team_data %>% 
  slice(1:15) %>% 
  select(team, barthag, barthag_rk, adj_o, adj_d) %>% 
  gt_cbb_teams(team, team) %>% 
  gt() %>% 
  fmt_markdown(team) %>%
  cols_align(columns = team, 'left')

acc_team_data %>%
  ggplot(aes(adj_o, adj_d, team = team)) +
  geom_cbb_teams(highlight_teams = c('Georgia Tech', 'Syracuse', 'Notre Dame'), width = 0.08, highlight_method = 'both') +
  geom_mean_lines(aes(x0 = adj_o, y0 = adj_d), color = 'black') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = 'bold', size = 14),
    plot.title.position = 'plot'
  ) +
  labs(title = 'Adjusted Efficiencies in the ACC',
       x = 'Adjusted defense',
       y = 'Adjusted offense')

tourney<-cbd_torvik_current_resume()
acc_tourney<-filter(tourney, conf == 'ACC')

acc_tourney %>% 
  ggplot(aes(wab, barthag, team = team)) +
  geom_cbb_teams(width = 0.10) +
  geom_mean_lines(aes(x0 = wab, y0 = barthag), color = 'black') +
  theme_minimal() +
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold')) +
  labs(title = 'ACC Resumes',
       x = 'Wins against Bubble',
       y = 'Torvik Rank')
