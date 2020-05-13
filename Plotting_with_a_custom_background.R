# Libraries ----
library(tidyverse)
library(jpeg)
library(gridExtra)
library(grid)

# Get info on the directory of the tables
tables      <-  fs::dir_info("stocks/") 
# Grab the directories to the tables
these       <-  tables[[1]]

table_names <- stringr::str_split(these,pattern = "(/)|(\\.)",simplify = T)[,2]

# read all files and create a named list
lapply(these, readr::read_rds) %>% 
    purrr::set_names(table_names) -> named.list

# Right join two tables with purrr::reduce
reduce(named.list,right_join, by = 'symbol') %>% 
    select(symbol,company,sector,date,adjusted) -> stocks

# Wrangle data ----
stocks %>% 
    filter(grepl(x = symbol,pattern = "MSFT|NFL|^FB$") ) %>% 
    mutate(
        symbol = forcats::fct_reorder2(symbol,date,adjusted),
        pct    = adjusted/lag(adjusted) - 1
           ) %>% 
    filter(!is.na(pct)) -> plot_data


# Load raw image ----

 m <- jpeg::readJPEG("images/bull_and_bear.jpg")
# You'll need to comfortable with indexing arrays in order to manipulate and customize the image 
 class(m)
# Dimensions: 990  rows, 1300 columns, 3 matrices
 dim(m)
#  Grab the first matrix
 m[,,1]
  
# Grab the first row and 10 th column of the third matrix
 m[1,10,3]


 # Customizing the image ----
  # the rgb() function describes a color, by giving the intensity of the three primary colors: red, green and blue.
  # It takes four main arguments -> red, green, blue and alpha
  # To use this function, we'll index the array for the values of red, green and blue
 
  img.c <- matrix(rgb(red = m[,,1],green = m[,,2],blue = m[,,3] * 0.6,alpha = 0.7), nrow=dim(m)[1])
# I multiply the blues by 0.6 to get that mustard color

# Plot ----

 
ggplot(plot_data,aes(symbol,pct))+
     # Adding the image to the background with annotation_custom
    annotation_custom(xmin=-Inf, 
                      ymin=-Inf,
                      xmax=Inf, 
                      ymax=Inf,
                      rasterGrob(img.c,width=unit(1,"npc"), height=unit(1,"npc")))+
    geom_jitter(alpha = 0.2, color = "black")+
    geom_violin(aes(fill = symbol), alpha = 0.8)+
    theme(
        text = element_text(colour = "black",face = 'bold'),
        title = element_text(color = "black"),
        line = element_line(color = "black"),
        rect = element_rect(fill = "#eaec7c", color = "black"),
        axis.ticks = element_line(color = "#969696"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "#969696", linetype = 1),
        legend.background = element_rect(fill = NULL, color = NULL),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill=NA,colour=NA,size=NA,linetype = NULL)
    )+
    labs(x     = "", 
         fill  = "Symbol",
         y     = "Daily Percentage Return",
         title = "Investigating the volatility of stocks. \nNetflix is the riskier stock")+
    scale_fill_brewer(palette = "Dark2",direction = -1)+
    scale_y_continuous(
        labels = scales::percent_format(), 
        breaks = seq(-1, max(plot_data$pct), by = 0.1))

    
