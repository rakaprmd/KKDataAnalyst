library(raster)
library(sf)
library(leaflet)
library(readxl)
library(rnaturalearth)
library(WDI)
library(tigris)
library(dplyr)

kecamatan <- shapefile("~/idn/idn_admbnda_adm3_bps_2019.shp")
kkpolygon2 <- read_excel("kkpolygon.xlsx")

kopken <- kkpolygon2 %>%
  filter(status == "KK")

sbux <- kkpolygon2 %>%
  filter(status == "SB")

kkjoin <- geo_join(kecamatan, kopken, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")
sbjoin <- geo_join(kecamatan, sbux, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")

binpal <- colorBin("YlOrRd", kkjoin@data$avg.Net.Sales, 6, pretty = FALSE)
binpal2 <- colorBin("Greens", sbjoin@data$avg.Net.Sales, 6, pretty = FALSE)

kksbjoinMap <- leaflet()%>%
      setView(lat = -6.2088,
              lng = 106.8456, zoom = 5)%>%
      addProviderTiles("CartoDB") %>% 
           addPolygons(
                 data = kkjoin,
                 group = "KopiKenangan",
                 weight = 1,
                 color = ~binpal(avg.Net.Sales),
                 opacity = 1,
                 fillOpacity = 0.7,
                 label = ~paste(ADM3_EN), 
                 popup = ~paste0(
                       "<b>","Sub-district : ",ADM3_EN,"</b>",
                       "<br/>","Store Count : ",count.Site.Name,
                       "<br/>","Z-Score  : ",round(Z.SCORE,digits = 2),
                       "<br/>","Average Gmv (Month) : ",
                       format(avg.Net.Sales,big.mark=".",decimal.mark = ",")),
                 highlight = highlightOptions(weight = 1, color = "red",
                 bringToFront = TRUE)) %>%
       addPolygons(
             group = "Starbucks",
             data = sbjoin,
             weight = 1,
             color = ~binpal2(avg.Net.Sales),
             opacity = 1,
             fillOpacity = 0.7,
             label = ~paste(ADM3_EN), 
             popup = ~paste0(
                   "<b>","Sub-district : ",ADM3_EN,"</b>",
                   "<br/>","Store Count : ",count.Site.Name,
                   "<br/>","Z-Score  : ",round(Z.SCORE,digits = 2),
                   "<br/>","Average Gmv (Week) : ",
                   format(avg.Net.Sales,big.mark=".",decimal.mark = ",")),
             highlight = highlightOptions(weight = 1, color = "red",
            bringToFront = TRUE)) %>%
      addLayersControl(overlayGroups = c("KopiKenangan", "Starbucks"))

kksbjoinMap