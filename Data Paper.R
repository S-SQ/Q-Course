library(tidyverse)
library(httr)
library(lubridate)
library(jsonlite)
library(janitor)
library(zoo)
library(rvest)
library(xm12)
library(ggpmisc)

df_0 <- read_csv('Trips_by_Distance.csv') %>%
  janitor::clean_names()

df_nat <- df_0 %>%
  filter(level == 'National') %>%
  group_by(date = floor_date(date, unit = "weeks")) %>%
  summarize_at(vars("population_staying_at_home", "population_not_staying_at_home"), sum) %>%
  mutate(percent_staying_home = population_staying_at_home / (population_staying_at_home + population_not_staying_at_home),
         year = year(date),
         COVID = case_when(
           date >= as.Date('2020-03-16') ~ 'Pandemic',
           T ~ 'Normal'
         ))

df_nat_19 <- df_nat %>%
  filter(year == 2019)
mean(df_nat_19$percent_staying_home)

df_nat_20 <- df_nat %>%
  filter(year == 2020)
mean(df_nat_19$percent_staying_home)

ggplot(data = df_nat, aes(x = date, y = percent_staying_home, color = COVID)) +
  scale_color_manual(values = c('blue', 'red')) +
  geom_text(aes(x = as.Date('2019-12-15'), y = .30, label ='"15 Days"'), color = 'Black') +
  geom_line(size = 1.01) +
  geom_vline(aes(xintercept = as.Date('2020-03-18')), color = 'black', size = 1.1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "% of Pop Staying Home", 
       title = "Percentage of US Population Staying at Home per Week") +
  theme_bw()

ggsave(filename = "total_stay_home.jpg", device='jpeg', dpi=700)

glimpse(df_0)

df_state <- df_0 %>%
  filter(level == 'State')

df_ste_wk <- df_state %>%
  group_by(year = year(date), week = week(date), state_postal_code) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  select(year, week, state_postal_code, population_staying_at_home) %>%
  pivot_wider(names_from = "year", values_from = 'population_staying_at_home') %>%
  mutate(baseline20 = ((`2020` - `2019`)/`2019`), baseline21 = ((`2021` - `2019`)/`2019`)) %>%
  pivot_longer(cols = contains('baseline')) %>%
  select(week, state_postal_code, name, value) %>%
  mutate(year = case_when(
    name == 'baseline20' ~ 2020,
    name == 'baseline21' ~ 2021,
    T ~ NA_real_
  ),
  date = ymd(str_c(year, "-01-01")) + weeks(week - 1)) %>%
  arrange(state_postal_code, date) %>%
  filter(is.na(value) == F, week != 53) %>%
  group_by(state_postal_code) %>%
  mutate(rank = 1:n()) %>%
  filter(rank < max(rank))

df_ste_plt <- df_ste_wk %>%
  filter(state_postal_code %in% c('CA', 'FL', 'NY', 'TX')) %>%
  group_by(state_postal_code) %>%
  mutate(roll_avg=rollapply(value,3,mean,align='right',fill=NA))

ggplot(data = df_ste_plt) +
  geom_path(aes(x = date, y = roll_avg, color = state_postal_code), size = 1.25) +
  geom_hline(aes(yintercept = 0), color = 'black', size = 1.5, linetype = 'dashed') +
  geom_text(aes(x = as.Date('2020-07-01'), y = .02), label = "2019 Baseline") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "% from Baseline", x = "Date", title = "State Population Staying at Home Compared to 2019 Baseline") +
  theme_bw()


df_ste_sm <- df_ste_wk %>%
  group_by(state_postal_code) %>%
  summarize_at(vars(value), mean)

df_wom <- read_html("https://www.worldometers.info/coronavirus/country/us/") %>%
  html_nodes('#usa_table_countries_today') %>%
  html_table()
df_wom <- df_wom[[1]] %>%
  clean_names()

df_st_cd <- read_html("https://www.ssa.gov/international/coc-docs/states.html") %>%
  html_nodes('.m-w-66') %>%
  html_table()
df_st_cd <- df_st_cd[[1]]
colnames(df_st_cd) <- c('state','state_postal_code')

df_st_vt <- read_csv('vote_by_state.csv') %>%
  clean_names() %>%
  select(state = x1, per_dem)

df_st_ue <- read_html("https://www.bls.gov/web/laus/laumstch.htm") %>%
  html_nodes('div table') %>%
  html_table()
df_st_ue <- df_st_ue[[2]]
df_st_ue <- df_st_ue[-1,]
df_st_ue <- df_st_ue[-52:-53,]
colnames(df_st_ue) <- c("state", "unemployment_june_2020", "unemployment_june_2021", "chg_year", "chg_rank")
df_st_ue <- df_st_ue %>%
  mutate(state = str_to_upper(state), unemployment_june_2020 = as.numeric(unemployment_june_2020)/100,
         unemployment_june_2021 = as.numeric(unemployment_june_2021)/100) %>%
  select(state, unemployment_june_2020, unemployment_june_2021)


df_dth <- df_wom %>%
  select(usa_state, deaths_1m_pop) %>%
  rename(state = usa_state) %>%
  mutate(state = str_to_upper(state)) %>%
  left_join(df_st_vt, by = 'state') %>%
  left_join(df_st_ue, by = 'state') %>%
  right_join(df_st_cd, by = 'state') %>%
  right_join(df_ste_sm, by = 'state_postal_code') %>%
  mutate(deaths_1m_pop = as.numeric(gsub("\\,", "", deaths_1m_pop))) %>%
  filter(state_postal_code != 'DC')


my.formula <- y ~ x
ggplot(data = df_dth, aes(x = value, y = deaths_1m_pop)) +
  geom_smooth(method='lm', se = F, formula = my.formula) +
  geom_point(aes(color = per_dem), size = 3) +
  geom_text(aes(label = state_postal_code), hjust = 0, vjust = 0) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),  label.y = .05, label.x = .95,
               parse = TRUE) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_gradient(low = 'red', high = 'blue', name = "Party Vote 2020") +
  labs(title = "Travel Change vs COVID Deaths per Million by State",
    caption = "Travel Change : % Change in population staying at home compared to 2019", 
    x = "% Change - Staying at Home", y = "Deaths per Million") +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = "chart1.jpg", device='jpeg', dpi=700, height = 4, width = 6)

ggplot(data = df_dth, aes(x = value, y = per_dem)) +
  geom_smooth(method='lm', se = F, formula = my.formula) +
  geom_point(aes(color = per_dem), size = 3) +
  geom_text(aes(label = state_postal_code), hjust = 0, vjust = 0) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.y = .05, label.x = .95,
               parse = TRUE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_gradient(low = 'red', high = 'blue', name = "Party Vote 2020") +
  labs(title = "Travel Change vs Political Affiliation by State", 
      caption = "Travel Change : % Change in population staying at home compared to 2019", 
      x = "% Change - Staying at Home", 
      y = "% Voting Democrat 2020 Pres. Election") +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = "chart2.jpg", device='jpeg', dpi=700, height = 4, width = 6)

ggplot(data = df_dth, aes(x = value, y = unemployment_june_2021)) +
  geom_smooth(method='lm', se = F, formula = my.formula) +
  geom_point(aes(color = per_dem), size = 3) +
  geom_text(aes(label = state_postal_code), hjust = 0, vjust = 0) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.y = .05, label.x = .95,
               parse = TRUE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_gradient(low = 'red', high = 'blue', name = "Party Vote 2020") +
  labs(title = "Travel Change vs June 2021 Unemployment Rate", 
       caption = "Travel Change : % Change in population staying at home compared to 2019", x = "% Change - Staying at Home", 
       y = "Unemployment Rate") +
  theme_bw() +
  theme(legend.position = 'none')

ggsave(filename = "chart3.jpg", device='jpeg', dpi=700, height = 4, width = 6)


