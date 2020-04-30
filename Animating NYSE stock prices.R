# Libraries
library(tidyquant)
library(tidyverse)
library(data.table)



# Get the industries with the tidyquant API
nyse <- tidyquant::tq_exchange("NYSE")

# Filter for Agricultural industy
unique_industries <- unique(nyse$industry)


agri_chem <- unique_industries[grepl(x = unique_industries,ignore.case = TRUE,pattern = "agri")]

# Get the stock prices for all the companies listed in this sector

agricompanies <- nyse[nyse$industry %in% agri_chem,]$symbol %>% 
    tq_get(get = "stock.prices")

setDT(nyse)
setDT(agricompanies)

# Get the percentage return by symbol
agricompanies[, pct_return := adjusted/lag(adjusted) - 1, by = symbol][]

# Get additional information on the stocks by joining it to nyse    
agricompanies_merged <- merge(agricompanies,nyse[,.SD, .SDcols = c("symbol","company","sector","industry")],by = "symbol")

# Convert character fields to factor fields and reorder
agricompanies_merged[,lapply(.SD, as.factor), .SDcols = c("symbol","company","sector","industry")]

agricompanies_merged[, `:=`(symbol = forcats::fct_reorder2(symbol,date,pct_return), company = forcats::fct_reorder2(company,date,pct_return))]

# Add a six month rolling mean
agricompanies_merged[, rollmean := zoo::rollmean(pct_return,
                                            k      = 6,
                                            fill   = 0,
                                            na.pad = T,
                                            align  = 'right')]


static_p <- ggplot(agricompanies_merged[date > "2020-01-01"],aes(date,pct_return, color = symbol))+
    geom_line()+
    geom_point()+
    geom_line(aes(date,rollmean), color = 'black')+
    facet_wrap(~ company,scales = "free_y")+
    # Aesthetics
    theme_tq()+
    scale_color_tq()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_date()+
    labs(y = "Percentage Return", x = 'Date' )+
    theme(
        text = element_text(size = 13, face = "bold")
    )


animate_p <- static_p + gganimate::transition_reveal(date)

gganimate::animate(animate_p,height = 561,width = 780)

#gganimate::anim_save("agri_chem_companies.gif",path = '../Desktop/')
    