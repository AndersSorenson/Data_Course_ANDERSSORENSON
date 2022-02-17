df <- read.csv("cleaned_covid_data.csv")
library(tidyverse)
library(dplyr)
A_states <- df %>% 
  filter(substr(Province_State, 1, 1) == "A")

ggplot(A_states, aes(x=Last_Update, y=Deaths, color=Province_State)) +
  geom_point(
    mapping = aes(x=Last_Update, y=Deaths)
  ) + 
  labs(title = "   Coronavrius Deaths Over Time in States Beginning with the Letter A", x.axis= "Time") + 
  facet_wrap(~Province_State) + 
  stat_smooth(method="loess") + 
  xlab("Time")
  



state_max_fatality_rate1 <- df %>%
  group_by(Province_State) %>%
  slice(which.max(Case_Fatality_Ratio))

state_max_fatality_rate <- state_max_fatality_rate1[, c(1,7)]

state_max_fatality_rate %>% arrange(desc(Case_Fatality_Ratio))

state_max_fatality_rate %>% 
  ggplot(aes(x = reorder(Province_State, -Case_Fatality_Ratio), y=Case_Fatality_Ratio)) +
  geom_col() + 
  coord_flip() + 
  xlab("States") + 
  ylab("Case Fatality Ratio")

