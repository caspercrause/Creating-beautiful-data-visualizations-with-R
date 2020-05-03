library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(data.table)



data_all_comm_combined <- readr::read_rds("../Dashboard/data_all_combined.rds")
ne_countries(scale="medium",returnclass = "sf") -> world

data_all_comm_combined[, .(total = sum(Act_Ctns)), by = .(CountryDesc)] -> summed


summed[CountryDesc %like% "Korea",CountryDesc:="Korea"]


setdiff(summed$CountryDesc, world$name)

# Left join
sf_dataframe <- sf::st_as_sf(merge(summed,world, by.x = "CountryDesc", by.y = "name", all.x = TRUE)) 

sf_dataframe %>% 
    mutate(txt = paste("Cartons: ",scales::comma(total),"\n", "Country: ",CountryDesc)) -> sf_dataframe

sf_dataframe %>% 
    ggplot()+
    geom_sf(color = "black", aes(fill = total, text = txt))+
    scale_fill_viridis_c(option = "C", trans = "sqrt",labels = scales::comma_format(scale = 1e-6, suffix = "Mn"))+
    theme_minimal()+
    theme(axis.text = element_blank())+
    labs(fill = "Total Actual Cartons")-> p

ggplotly(p,tooltip = "txt")