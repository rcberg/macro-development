#devtools::install_github("RamiKrispin/coronavirus", force = T)
library(  pacman )
p_load(janitor , 
       tidyverse ,
       lubridate , 
       coronavirus , 
       maps , 
       sf ,
       rnaturalearth , 
       rnaturalearthdata )

data = coronavirus %>% 
  mutate( date = ymd(date) ) %>%
  clean_names() 

cum_cases_function = function(ymd){
  data_us_date = data %>% 
    filter( date <= ymd ) %>%
    group_by(country_region,
             province_state ) %>%
    summarise( total_cases = sum(cases) ,
               lat = mean(lat) , 
               long = mean(long) ,
               date = ymd )
}

comparison_dates = c( ymd(max(totals_data_df$date)) - weeks(6) , 
                      ymd(max(totals_data_df$date)) - weeks(3) , 
                      ymd(max(totals_data_df$date)) )

totals_data_df = 
  map_dfr(comparison_dates , 
          cum_cases_function ) %>%
  arrange( desc(date) , desc(total_cases))


## USA

data_us = totals_data_df %>% filter(country_region == "US" )%>%
  arrange( desc(date) ,
           desc(total_cases) ) 

usa = st_as_sf(map("state" , plot = FALSE, fill = TRUE ) )

cvirus_usa = 
  ggplot( data = usa ) + 
  geom_sf( ) + 
  geom_point( data = data_us ,
              aes( x = long , 
                   y = lat ,
                   size = total_cases ),
              alpha = 0.6 ,
              color = "darkred") + 
  coord_sf( xlim = c(-125, -65) , 
            ylim = c(25, 50) , 
            expand = FALSE ) + 
  labs( title = "COVID-19 Cases in the United States" , 
        x = "" , 
        y = "" ,
        color = "No. of Cases") + 
  theme( panel.background = element_rect( fill = 'white '))

cvirus_usa

ggsave(filename = "cvirus.png" , plot = cvirus_usa , dpi = 750 )

## World

world = st_as_sf(map("world" , plot = F , fill = T ) )

cvirus_world = 
  ggplot( data = world ) + 
  geom_sf( ) + 
  geom_point( data = totals_data_df ,
              aes( x = long , 
                   y = lat ,
                   size = total_cases ,
                   color = total_cases ),
              alpha = 0.4 ,
              color = "darkred") + 
  facet_wrap( ~date , nrow = 3 ) + 
  labs( title = "COVID-19 Cases" , 
        x = "" , 
        y = "" ,
        color = "No. of Cases") + 
  theme( panel.background = element_rect( fill = 'white '))

cvirus_world

ggsave(filename = "cvirus_worldwide.png" , plot = cvirus_world , dpi = 750 )


#%>% st_transform(crs = "+init=epsg:4326") %>%
#  leaflet(width = "100%") %>%
#  addProviderTiles(provider = "CartoDB.Positron") %>%
#  addPolygons(
#    # popup = ~tract,
#    popup = ~paste0(tract, "<br>", "Median value: $", prettyNum(estimate, big.mark=",")),
#    stroke = FALSE,
#    smoothFactor = 0,
#    fillOpacity = 0.5,
#    color = ~lane_pal(estimate)
#  ) 
