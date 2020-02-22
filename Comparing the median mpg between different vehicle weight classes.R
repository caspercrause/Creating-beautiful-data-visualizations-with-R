# 1) Libraries----
library(tidyverse)
library(tidyquant)
library(gganimate)



mtcars %>% as_tibble() -> mtcars

# Get the perentiles of interest. This will be used to discretize a continuous variable (weight) and divide it into 5 equal classes : 0 - 1.513, 1.514 - 2.349, 3.158 - 3.440, 3.440 - 3.770, > 3.770

quantile(mtcars$wt ,probs = seq(0,1, by = .2))

#2) Data-wrangling:----
 data <- mtcars %>% 
# 2.1 Categorize numerical data - weight----
  mutate(wt_class = wt %>% ntile(n = 5)) %>% 
  select(wt_class,cyl,vs,am,everything()) %>% 
  mutate_at(vars(wt_class:am), ~ as_factor(.)) %>% 
# 2.2 Get all the categories of interest into one column----
   pivot_longer(cols = vs:am) %>% 
unite(col = 'attr',sep = ' - ',name,value) %>% 
  mutate(attr = case_when(
    attr == 'vs - 1' ~ 'Straight',
    attr == 'vs - 0' ~ 'V-shaped',
    attr == 'am - 0' ~ 'Automatic',
    T                ~ 'Manual'
    
  ) %>% as_factor()) %>% 
  mutate(wt_class = case_when(
  wt_class == 1 ~ '0.5-2.5', 
  wt_class == 2 ~ '2.5-3.2',
  wt_class == 3 ~ '3.2-3.4',
  wt_class == 4 ~ '3.4-3.77',
  T             ~ '> 3.77'
  ) %>% as_factor()) %>% 
# 2.3 Relevel factors the way you want----
  mutate( wt_class = wt_class %>%  fct_relevel(c('0.5-2.5','2.5-3.2','3.2-3.4','3.4-3.77'))) %>% 
  select(cyl,attr,everything()) %>% 
# 2.4 Group by and summarise----
   group_by_at(vars(cyl:wt_class)) %>% 
  summarise(median_mpg = mpg %>% median())
  
# 3) Plotting----
#  3.1 Static----
   data %>% ggplot(aes(x = wt_class,y = median_mpg))+
  geom_col(aes(fill = attr),
           position = position_dodge(width = 5),
           color    = 'white',
           width    = 5,
           alpha    = 0.8 )+
  facet_wrap(~ wt_class, ncol = 5)+
  theme_minimal()+
  theme(
    panel.grid         = element_blank(),
    panel.grid.major.y = element_line(color = 'white'),
    panel.ontop        = T,
    strip.background   = element_rect(fill = palette_dark()[3]),
    strip.text         =  element_text(color = 'white', face = 'bold'),
    axis.text.x        = element_blank()

  )+
   scale_fill_tq(theme = 'dark')+
  labs(
    title = str_glue('Comparing the median miles per US-gallon
                     between different vehicle-weight classes
                     (Scaled by 1000 lbs)'),
    fill  = 'Attribute',
    x     = 'Weight class',
    y     = 'Median miles per US Gallon'
  ) -> plot_static

# 3.2 gganimate----
p <- plot_static+
  gganimate::transition_states(wt_class, wrap = F)+
  shadow_mark()

 animate(p,end_pause = 10)
# 
  # gganimate::anim_save('miles_perG.gif',path = '7 days of animation/GIF/')

