library(ggplot2)
library(tidyverse)
library(janitor)
library(modelr)

#1

df <- read.csv('unicef-u5mr.csv')

#2

tidydf <- df %>% 
  pivot_longer(U5MR.1950:U5MR.2015, names_to = "Year", values_to = "U5MR", values_drop_na = TRUE)


#3

tidydf$Year <- gsub("U5MR.", "", tidydf$Year)
tidydf$Year <- as.numeric(as.character(tidydf$Year))

tidydf %>%  
  ggplot(aes(x=Year, y=U5MR)) + 
  geom_point() + 
  facet_wrap( ~ Continent) + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
#5

dfmean <- tidydf %>% 
  group_by(Continent, Year) %>% 
  mutate(U5MR = mean(U5MR))



dfmean %>% 
  ggplot(aes(x=Year, y=U5MR, color=Continent)) + 
  geom_point() + 
  geom_line() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
#7
mod1 <- 
  lm(U5MR ~ Year, data = tidydf)

mod2 <- 
  lm(U5MR ~ Continent + Year, data = tidydf)

mod3 <- 
  lm(U5MR ~ Year + Continent + Year:Continent, data = tidydf)


tidydf = add_predictions(tidydf,mod1,var = "mod1_Prediction")
tidydf = add_predictions(tidydf,mod2,var = "mod2_Prediction")
tidydf = add_predictions(tidydf,mod3,var = "mod3_Prediction")

#8
trainmodel <- tidydf %>% 
  slice_sample(prop = .25)

testmodel <- anti_join(tidydf, trainmodel)

testmodel %>% 
  add_residuals(model = mod1)

testmodel %>% 
  add_residuals(model = mod2)

testmodel %>% 
  add_residuals(model = mod3)

#model 3 is the best model because it has the difference between the predicted rate of U5MR and the real rate- 
#-of U5MR are closest in this model. 

#9
dflong <- tidydf %>% 
  gather(model, predicted_U5MR, mod1_Prediction:mod2_Prediction:mod3_Prediction)

dflong %>% 
  ggplot(aes(x=Year, y=predicted_U5MR, color=Continent)) + 
  geom_line() + 
  facet_wrap( ~ model)

  


  