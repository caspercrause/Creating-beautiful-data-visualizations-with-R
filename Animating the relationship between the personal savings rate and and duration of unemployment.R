# Libraries needed----
library(tidyverse)
library(colortools)
library(tidyquant)
library(gganimate)

economics <- economics

# Custom Function:----
plot_cor <- function(data){
  data %>% 
    select_if(~is.numeric(.)) %>% 
    cor() %>% 
    round(2) %>% 
    ggcorrplot::ggcorrplot(hc.order = TRUE,
                           type     = 'upper',
                           colors   =  c('#8E1600', 'white', '#003b00'),
                           lab      = T)
}

# Test function:----
economics %>% plot_cor()

# Get colors that compliment each other nicely:
colortools::complementary('steelblue')


plot_static <- economics %>% 
  mutate(month = date %>% lubridate::month(label = T),
         year  = date %>% lubridate::year()) %>% 
  unite(
    col = 'month_year',
    month:year,
    sep = ' - '
  ) %>% 
  select(date,month_year,uempmed,psavert) %>% 
  rename('median_period_unemp'  = uempmed,
         'personal_savings'     = psavert) %>% 
  pivot_longer(cols             = median_period_unemp:personal_savings,
               names_to         = 'measure',
               values_to        = 'value') %>% 
  ggplot(aes(date, value))+
  geom_area(aes(fill = measure,
               color = measure),
            alpha    = 0.7,
            position = position_dodge(width = 0.9),
           size      = 1 )+
  scale_fill_manual(values = c("#B47846","#4682B4"))+
  scale_color_manual(values = c("#B47846","#4682B4"))+
  labs(
    title = str_glue('What is the relationship between the personal savings 
    rate and the median duration of unemployment?'),
    y     = '',
    x     = '')+
  theme_tq()+
  scale_x_date(date_labels = '%b-%Y')
 
  # gganimate----
plot_anim <- plot_static + gganimate::transition_reveal(date)+
  ease_aes('linear')+
  enter_fade()+
  exit_fade()+
  view_follow(fixed_y = T)
 

# Top tip----
# Adding pause at the end of the animation to help viewers make sense of your findings is always a great idea
plot_anim %>% animate(end_pause = 30)
 # anim_save('economics.gif', path = '7 days of animation/GIF/')

