# Libraries needed:----
library(gapminder)
library(tidyverse)
library(gganimate)
library(parsnip)
library(ggcorrplot)
library(broom)
library(yardstick)

gapminder <- gapminder

 
# Correlation analysis:-----
gapminder %>% 
  select_if(~is.numeric(.)) %>% 
  cor() %>% 
  round(2) %>% 
  ggcorrplot::ggcorrplot(hc.order = TRUE,
                                      type     = 'upper',
                                      colors   =  c('#8E1600', 'white', '#003b00'),
                                      lab      = T)

# Custom Function:----
plot_cor <- function(data){
  data %>% 
    select_if(~is.numeric(.)) %>% 
    cor() %>% 
    round(2) %>% 
    ggcorrplot::ggcorrplot(hc.order = TRUE,
                               type = 'upper',
                             colors =  c('#8E1600', 'white', '#003b00'),
                                lab = T)
}

# Test the function:----
mtcars %>% plot_cor()

# Parsnip API:----
# 3 steps :
# 1. Create a model              -> parsnip::linear_reg()
# 2. Set an engine               -> parsnip::set_engine()
# 3. Fit the model to data       -> parsnip::fit()
# 4. Predict                     -> base::predict()

# 1----
parsnip_lm <- linear_reg(mode = 'regression') %>% 
# 2----
  set_engine(engine = 'lm') %>% 
# 3----
  fit(formula = lifeExp ~ year + gdpPercap, data = gapminder)
# 4----
parsnip_lm %>% 
  predict(new_data = gapminder) %>% 
  bind_cols(gapminder) %>% 
  select(.pred,lifeExp, everything()) %>% 
 # Is the model any good?
  # yardstick::metrics(truth    = lifeExp,
  #                   estimate = .pred )
  pivot_longer(cols      = .pred:lifeExp,
               names_to  = 'actual_vs_predict',
               values_to = 'value') %>% 
  group_by(year,continent,actual_vs_predict) %>% 
  summarise(value = value %>% mean()) %>% 
  ungroup() %>% 

  # plot----
  ggplot(aes(year, value, color = actual_vs_predict))+
  geom_point()+
  geom_line()+
  facet_wrap(~ continent)


# Alternative:----
# base::lm()----
fitted_model_linear <-    lm(data = gapminder, 
  formula = lifeExp ~ year + gdpPercap) %>%
  broom::augment(gapminder %>% select(lifeExp, year, gdpPercap,country, continent)) %>%
  select(lifeExp,.fitted, everything()) %>% 
  pivot_longer(cols      = lifeExp:.fitted,
               names_to  = 'actual_and_fitted',
               values_to = 'measurements') %>% 
  group_by(year, actual_and_fitted) %>% 
  summarise(measurements = measurements %>% median()) %>% 
  ungroup()

# Is the model any good?
lm(data = gapminder, formula = lifeExp ~ year + gdpPercap) %>% broom::glance()

# plot----
plot <- ggplot(fitted_model_linear) +
  aes(x = year, y = measurements, colour = actual_and_fitted) +
  geom_line(size = 1L) +
  geom_point(aes(group = seq_along(year)))+
  
  
  scale_color_viridis_d(option = "plasma") +
  labs(      x = "Time(Years)",
             y = "Age", 
             title = "Using a linear model to predict life expectancy ", 
             subtitle = "Formula:\nlifeExp ~ year + gdpPercap", 
             color = '',
             caption = 'The year and the GDP per capita explain 43.7 % of the\nvariability in life-expectancy'
  ) +
  theme_bw()+
  expand_limits(y = 0)+
  
  theme(
    title        = element_text(size = 14.5, face = 'bold')  ,
    plot.caption = element_text(size = 13,   face = 'bold.italic')
  )
  
# Animate:----
  plot + gganimate::transition_reveal(year)

# save animation----
# anim_save(filename = 'gapminder_predict_linear.gif',
# path     = 'GIF/')
