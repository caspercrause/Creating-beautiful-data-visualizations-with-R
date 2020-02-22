# Libraries needed----
library(tidyverse)
library(tidyquant)
library(readxl)
library(gganimate)
library(furrr)

# Get the info of the directory----
dir_info <- fs::dir_info('All Varieties/')

# Read in all excel sheets using furrr:----

data_all_comm_combined <- dir_info %>% 
  select(path) %>% 
  mutate(data = path %>% future_map(read_excel))

# Type conversions and sting manipulation:----
data_all_comm_combined <-  data_all_comm_combined %>%
  unnest(cols   = c(data)) %>% 
  select(-path) %>% 
  mutate(Season = Season %>% as.numeric()) %>% 
  mutate_if(is.character, ~ str_to_title(.)) %>% 
  mutate_if(is.character, as_factor)

# static plot ----
   plot_stat <- data_all_comm_combined %>% 
    # Get month from year and week----
  mutate(Month = month(ymd(Season * 10000 + 0101) + ShipWeeks * 7)) %>% 
  select(Season,Month,ReportCommDesc,Plt_Qty) %>% 
    arrange(Month) %>% 
    group_by(Month,ReportCommDesc) %>% 
    summarise(total_qty = Plt_Qty %>% sum()) %>% 
    mutate(rank = rank(-total_qty)) %>% 
    ungroup() %>% 
    mutate(txt = total_qty %>% scales::comma()) %>% 
  
    
    # filter(rank <= 10) %>% 
  ggplot(aes(x = rank,y = total_qty/2, fill =  ReportCommDesc, group =  ReportCommDesc ))+
  # geometries
  geom_tile(aes(height = total_qty, width = 1 ), alpha = 0.7)+
   coord_flip(clip = 'off')+
   scale_x_reverse()+
  # Formatting
  theme_minimal()+
   # 1.Change plot background
   theme(axis.line       = element_blank(),
         axis.text.x     = element_blank(),
         axis.text.y     = element_blank(),
         axis.ticks      = element_blank(),
         axis.title.x    = element_blank(),
        axis.title.y     = element_blank(),
        legend.position  = "none",
         plot.background = element_blank(),
         plot.margin     = margin(2,2, 2, 4, "cm"))+

    # 2. Fill color
scale_fill_brewer(palette = 'Paired', direction = -1)+
    # 3. Labels
   geom_text(aes(y = 0, label = paste(ReportCommDesc, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = total_qty,label = txt , hjust = 0))

# gganimate----

p <- plot_stat + gganimate::transition_states(Month, transition_length = 4, state_length = 1)+
  labs(
    title    = 'Month:{closest_state}',
    subtitle = str_glue( 'The top perishable exports by pallet-quantity
                         from South Africa by month in 2018'),
    caption  = 'Source : Agri Hub'
  )


gganimate::animate(p,fps = 6)

# gganimate::anim_save('exports_by_comm.gif',path = '7 days of animation/')

  