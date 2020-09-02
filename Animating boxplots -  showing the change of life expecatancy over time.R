# Libraries----
library(gapminder)
library(tidyverse)
library(gganimate)
library(ggrepel)
library(tidyquant)

data(gapminder)


# Custom Function----
data <- c(1,2,3,4,5,NA_real_,50,100)
show_outliers <- function(data){
  if(!is.numeric(data)){
    stop('data must be of numeric type')
  }
  
  data_tibble <- tibble::tibble(data = data)
  
  data_tibble_summary <- data_tibble %>% 
    dplyr::summarise(
      lower_quantile = data %>%  stats::quantile(0.25, na.rm = T),
      upper_quantile = data %>%  stats::quantile(0.75, na.rm = T),
      inter_quantile_range = data %>% stats::IQR(na.rm = T),
      lower_limit = lower_quantile - 1.5*inter_quantile_range,
      upper_limit = upper_quantile + 1.5*inter_quantile_range
    )
  
  output <-  data_tibble %>% 
    dplyr::mutate(outliers = dplyr::case_when(
      data > data_tibble_summary$upper_limit ~ TRUE,
      data < data_tibble_summary$lower_limit ~ TRUE,
      TRUE                                   ~ FALSE
    ))
  
  return(output$outliers)
}
# Test function:----
show_outliers(data)
# Plotting----
plot_static <- gapminder %>%
  dplyr::mutate(continent  =  forcats::fct_reorder(continent,lifeExp)) %>%
  dplyr::group_by(continent,year) %>% 
  dplyr::mutate(outliers = show_outliers(lifeExp)) %>% 
  dplyr::ungroup() %>% 

 ggplot(aes(x = continent, y = lifeExp, fill = continent)) +
 geom_boxplot() +
 labs(
       fill = "Continent",
         x  = 'Continent',
        y   = 'Life-expectancy (Years)') +
  
 
  ggrepel::geom_text_repel(aes(label = country),
                                data = . %>% filter(outliers) )+
  
  theme_minimal()+
coord_flip()+
  tidyquant::scale_fill_tq(theme = 'dark')+
  expand_limits( y = 0 )

# gganimate:----
plot_static + gganimate::transition_time(year)+
  labs(
    title    = 'Year: {frame_time}',
    subtitle = "Exploring the change in life-expectancy over time"
  )

# Save The file to designated folder of your choice
gganimate::anim_save('boxplot_w_labs.gif',path = "GIF/")

