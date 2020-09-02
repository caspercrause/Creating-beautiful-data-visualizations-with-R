# Libraries ----
library(tidyverse)
library(tidyquant)
library(gganimate)


# Load data----
mtcars %>% as_tibble() -> mtcars

 plot_static <- mtcars %>% 
mutate_at(vars(vs:am), ~ as_factor(.)) %>% 
  mutate(cyl = cyl %>% as_factor()) %>% 
group_by(cyl,vs,am) %>% 
  summarise(median_mpg = median(mpg)) %>% 
  ungroup() %>% 
  mutate(vs = case_when(
    vs == 0 ~ 'V- Shaped',
    T       ~ 'Straight'
  )) %>% 
  ggplot(aes(x = cyl,y = median_mpg))+
  geom_col(aes(fill = vs), position = 'dodge', color = 'white', width = 1.5)+
  facet_wrap(~ cyl,ncol  = 3)+
  theme_minimal()+
  theme(
    panel.grid         = element_blank(),
    panel.grid.major.y = element_line(color = 'white'),
    panel.ontop        = T,
    strip.background   = element_rect(fill = palette_light()[1]),
    strip.text         = element_text(color = 'white', face = 'bold'),
    axis.text.x        = element_blank()
  )+
  scale_fill_viridis_d(option = 'C', direction = 1)+
  labs(
    title = 'Comparing the median miles per US Gallon\nbetween 4, 6 and 8-cylinder vehicles',
    fill  = '',
    x     = 'Cylinders',
    y     = 'Median miles per US Gallon'
  )

# gganimate
plot_static+
  gganimate::transition_states(cyl, wrap = FALSE)+
  shadow_mark()


# gganimate::anim_save('MPG.gif', path = 'GIF/')
  
