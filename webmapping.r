#install.packages("sen2r")
#install.packages("landsat")
#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("rmapshaper")
library(leaflet)
library(sf)
library(rgeos)
library(sp)
library(rmapshaper)
library(dplyr)
leaflet() %>%
  addTiles()
getwd()
kenya_wards <-st_read("Data/Shapefile/kenya_wards/kenya_wards.shp")


#Add kenya maps to leaflets
leaflet() %>%
  addTiles() %>%
  addPolygons(data=kenya_wards)
start_time<-Sys.time()
leaflet() %>%
  addTiles() %>%
  addPolygons(data=kenya_wards)
end_time<-Sys.time()
difference<-end_time-start_time

#Reducing the size of shapefile
kenya_wards2<-rmapshaper::ms_simplify(input=kenya_wards,keep=0.03,keep_shapes = T)
start_time<-Sys.time()
leaflet() %>%
  addTiles() %>%
  addPolygons(data=kenya_wards2)
end_time<-Sys.time()
difference<-end_time-start_time

#Spatial joint
ward_data<-read.csv("Data/Tabular/wards_full.csv")
kenya_wards3<-merge(x=kenya_wards,y=ward_data,
                    by.x='gid',by.y='gid',
                    suffixes='.x','.y')
kenya_wards4<-kenya_wards3[,-c(7:13)]

#Making the web map interactive
leaflet() %>%
  addTiles() %>%
  addPolygons(data=kenya_wards4,
              popup = paste0("County:",kenya_wards4$county.x,"<br>",
                             "Ward:",kenya_wards4$ward.x,"<br>",
                             "Voters:",kenya_wards4$voters,"<br>",
                             "David Waihiga:",kenya_wards4$david_waih,'<br>',
                             "George Wajakoya:",kenya_wards4$george_waj,'<br>',
                             "Raila Odinga:",kenya_wards4$raila_odin,'<br>',
                             "William Ruto:",kenya_wards4$william_ru))

#Coloring the different wards
color_wards<-colorFactor(palette=topo.colors(47),domain=kenya_wards4$county.x)
leaflet() %>%
  addTiles() %>%
  addPolygons(data=kenya_wards4,
    color= ~color_wards(kenya_wards4$county.x),  
    opacity=1, weight=2,
              popup = paste0("County:",kenya_wards4$county.x,"<br>",
                             "Ward:",kenya_wards4$ward.x,"<br>",
                             "Voters:",kenya_wards4$voters,"<br>",
                             "David Waihiga:",kenya_wards4$david_waih,'<br>',
                             "George Wajakoya:",kenya_wards4$george_waj,'<br>',
                             "Raila Odinga:",kenya_wards4$raila_odin,'<br>',
                             "William Ruto:",kenya_wards4$william_ru),
    highlightOptions = highlightOptions(
      stroke=T,
      color="blue",
      opacity=0.5,
      weight=3,
      bringToFront = T
    ))

#Adding Base maps
leaflet() %>%
  addTiles(group='OSM') %>%
  addProviderTiles(
    providers$CartoDB.Positron,group='Carto'
  ) %>%
  addProviderTiles(
    providers$OpenStreetMap.France,group='OSM FRANCE'
  ) %>%
  addPolygons(data=kenya_wards4,
              color= ~color_wards(kenya_wards4$county.x),  
              opacity=1, weight=2,
              popup = paste0("County:",kenya_wards4$county.x,"<br>",
                             "Ward:",kenya_wards4$ward.x,"<br>",
                             "Voters:",kenya_wards4$voters,"<br>",
                             "David Waihiga:",kenya_wards4$david_waih,'<br>',
                             "George Wajakoya:",kenya_wards4$george_waj,'<br>',
                             "Raila Odinga:",kenya_wards4$raila_odin,'<br>',
                             "William Ruto:",kenya_wards4$william_ru),
              highlightOptions = highlightOptions(
                stroke=T,
                color="blue",
                opacity=0.5,
                weight=3,
                bringToFront = T
              ),
              group="Administrative") %>% #Putting our polygons into administrative group
  addLayersControl(
    baseGroups = c('OSM','Carto','OSM France'),#Group names for OSM layers
    overlayGroups = c('Administrative') #Group name for our polygons
  )

#Add Raster Data
library(raster)
library(terra)
land_cover<-rast("Data/raster/Training_s2.tif") 
#Reduce raster
land_cover2<-terra::aggregate(land_cover,fact=100,fun="mean",overwrite=T)
kenya_wards5<-filter(kenya_wards4,county.x=="KITUI")
#The land_cover2 cannot be used in leaflet since its a SpatRaster
#Therefore it has to be changed to RasterLayer
land_cover3<-raster::raster(land_cover2)
# Add raster to leafmap
leaflet() %>%
  addTiles(group='OSM') %>%
  addProviderTiles(
    providers$CartoDB.Positron,group='Carto'
  ) %>%
  addProviderTiles(
    providers$OpenStreetMap.France,group='OSM FRANCE'
  ) %>%
  addPolygons(data=kenya_wards5,
              color= ~color_wards(kenya_wards5$county.x),  
              opacity=1, weight=2,
              popup = paste0("County:",kenya_wards5$county.x,"<br>",
                             "Ward:",kenya_wards5$ward.x,"<br>",
                             "Voters:",kenya_wards5$voters,"<br>",
                             "David Waihiga:",kenya_wards5$david_waih,'<br>',
                             "George Wajakoya:",kenya_wards5$george_waj,'<br>',
                             "Raila Odinga:",kenya_wards5$raila_odin,'<br>',
                             "William Ruto:",kenya_wards5$william_ru),
              highlightOptions = highlightOptions(
                stroke=T,
                color="blue",
                opacity=0.5,
                weight=3,
                bringToFront = T
              ),
              group="Administrative") %>%#Putting our polygons into administrative group
  addRasterImage(land_cover3,
                 colors = rainbow(6),
                 opacity = .8,
                 group='land cover',
                 layerId = 'land_cover') %>%
  addLegend(
    data = land_cover3, 
    position = "bottomleft", 
    colors = rainbow(6), 
    opacity = 0.5, 
    labels = c(
      '1', '2', '3', '4', '5', '6'
    ), 
    title = "Land classes"
  ) %>% # add legend
  addLayersControl(
    baseGroups = c('OSM','Carto','OSM France'),#Group names for OSM layers
    overlayGroups = c('Administrative') #Group name for our polygons
  )


