

# Libraries needed----
library(tidyverse)
library(DataExplorer)
library(gganimate)
library(readxl)
library(tidyquant)


# Instructions:----
# 1.0 Get all of the information on the data in the All Varieties Folder
 dir_inf        <- fs::dir_info('All Varieties/') 

# 2.0 Filter them down for only pome fruit
filtered_sheets <-  dir_inf$path[str_detect(dir_inf$path,"pome")]

# 3.0 Read all these sheets into one list 
list.of.objects <- map(filtered_sheets, readxl::read_excel)

# 4.0 Combine all of these objects into on dataframe
pome_19_18      <- do.call(rbind, list.of.objects)
# 5.0 Convert all the characters to factors

pome_19_18      <- pome_19_18 %>%
  mutate_if(is.character, as_factor)

# Plotting----

    plot_stat_y <-  pome_19_18 %>%
  select(Season,ShipWeeks,RegionReportAsDesc:Eqv_Ctns) %>% 
  rename_at(vars(Act_Ctns:Eqv_Ctns), ~ str_replace_all(. ,'_', ' ')) %>% 
  unite('year_week', Season,ShipWeeks,sep = '',remove = F)%>%
  mutate( year_week = year_week %>% as.numeric()) %>% 
  rename( Region = RegionReportAsDesc) %>% 
  group_by(Season,Region) %>% 
  summarise(total_cartons = `Act Ctns` %>% sum()) %>%
  mutate( rank = rank(-total_cartons),
          total_cartons_txt = total_cartons %>% scales::comma(scale = 1e-3,suffix = 'K')
          ) %>%
   # The rank is very important. This is what allows the categorical variables to overtake each other
     ungroup() %>% 
  
  # filter( rank<= 10) %>% 

  ggplot(aes(x = rank, fill = Region , group = Region))+
  geom_tile(aes(y = total_cartons/2, height = total_cartons), width = 0.8, alpha = 0.7)+
  
  # The clip needs to be set to off 
  coord_flip(clip = 'off',expand = F)+
  # scale_x_reverse()is for the highest rank to be at the top of the chart. 
  # Initially it was from low to high -> very similar to forcats::fct_rev() 
  # Remember we flipped the X and y axis with coord flip, but R still see's the x axis as rank 
  # and y axis as total cartons 
  scale_x_reverse()+
  theme_minimal()+
 # Change plot background
   theme(axis.line      =element_blank(),
        axis.text.x     =element_blank(),
        axis.text.y     =element_blank(),
        axis.ticks      =element_blank(),
        axis.title.x    =element_blank(),
        axis.title.y    =element_blank(),
        legend.position ="none",
        plot.background =element_blank(),
        plot.margin     = margin(2,2, 2, 4, "cm"))+
  scale_fill_tq(theme   = 'light')+
  # Labels
  geom_text(aes(y = 0, label = paste(Region, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = total_cartons,label = total_cartons_txt, hjust = 0))
  

# gganimate
 p_y <-  plot_stat_y + gganimate::transition_states(Season, transition_length = 4, state_length = 1)+
    view_follow(fixed_x = T)+
  labs(title    = 'Year :{closest_state}',
       subtitle =  'The top importers of South African apples and pears, 2017 - 2019\nMeasured in Cartons',
       caption  = str_glue('Source:
                           Argi Hub') )

  animate(p_y, fps = 20)
 
  
  
   # anim_save('exports_pome.gif',path = '7 days of animation/GIF/')

  
  # This is where I struggle. I want to show the animation by week and by year. The code is commented out because it doesn't run properly  
  
  # 2 Weekly----
  
  #  plot_stat_w_19 <- 
  # pome_19_18 %>%
 
  #   select(Season,ShipWeeks,RegionReportAsDesc:Eqv_Ctns) %>% 
  #   rename_at(vars(Act_Ctns:Eqv_Ctns), ~ str_replace_all(. ,'_', ' ')) %>% 
  #   unite('year_week', Season,ShipWeeks,sep = '',remove = F)%>%
  #   mutate( year_week = year_week %>% as.numeric()) %>% 
  #   rename( Region = RegionReportAsDesc) %>% 
  #   # mutate(ID = row_number()) %>% 
  #   group_by(ShipWeeks,Region) %>% 
  #   summarise(total_cartons = `Act Ctns` %>% sum()) %>%
  #   mutate( rank = rank(-total_cartons)*1) %>%
  #   # arrange(year_week, desc(rank)) %>% 
  #   
  #   ungroup() %>% 
  #   
  #   filter( rank<= 10) %>% 
  #   
  #   ggplot(aes(rank, fill = Region , group = Region))+
  #   geom_tile(aes(y = total_cartons/2, height = total_cartons), width = 0.8, alpha = 0.7)+
  #   coord_flip(clip = 'off',expand = F)+
  #   scale_x_reverse()+
  #   theme_minimal()+
  #   theme(axis.line=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank(),
  #         legend.position="none",
  #         plot.background=element_blank(),
  #         plot.margin = margin(2,2, 2, 4, "cm"))+
  #   scale_fill_tq(theme = 'light')+
  #   geom_text(aes(y = 0, label = paste(Region, " ")), vjust = 0.2, hjust = 1) +
  #   geom_text(aes(y=total_cartons,label = total_cartons, hjust = 0))
  # 
  # p_w_19 <-  plot_stat_w_19 + gganimate::transition_states(ShipWeeks, transition_length = 4, state_length = 1)+
  #   view_follow(fixed_x = T)+
  #   labs(title    = 'Week :{closest_state}',
  #        subtitle =  'The top importers of South African apples and pears by week in 2019',
  #        caption  = str_glue('Source:
  #                          Argi Hub') 
  #        )
  # 
  # animate(p_w_19, fps = 5)
  
# pome_19_18 %>%  readr::write_csv(path = '7 days of animation/pome_exports_2017-2019.csv')
            