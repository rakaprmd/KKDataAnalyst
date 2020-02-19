library(raster)
library(sf)
library(leaflet)
library(readxl)
library(rnaturalearth)
library(WDI)
library(tigris)
library(dplyr)
library(leaflet.extras)


kecamatan <- shapefile("~/idn/idn_admbnda_adm3_bps_2019.shp")
kkpolygon2 <- read_excel("KKDataAnalyst/kkpolygon.xlsx")
Kfc_clean <- read_excel("KKDataAnalyst/Kfc_clean.xlsx")
Gojek <- read_excel("KKDataAnalyst/Online-sales.xlsx")


kecamatan_jawa<-st_as_sf(kecamatan)
kecamatan_jawa <-kecamatan_jawa %>%
  filter(kecamatan@data$ADM1_PCODE =="ID36"
         |kecamatan@data$ADM1_PCODE =="ID34"
         |kecamatan@data$ADM1_PCODE =="ID31"
         |kecamatan@data$ADM1_PCODE =="ID32"
         |kecamatan@data$ADM1_PCODE =="ID33"
         |kecamatan@data$ADM1_PCODE =="ID35")

kopken <- kkpolygon2 %>%
  filter(status == "KK")

sbux <- kkpolygon2 %>%
  filter(status == "SB")

GojekFiltered <- Gojek %>%
  filter(KK_status == "Belum ada")

kkjoin <- geo_join(kecamatan, kopken, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")
sbjoin <- geo_join(kecamatan, sbux, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")
kfcjoin <- geo_join(kecamatan, Kfc_clean, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")
gojekJoin <- geo_join(kecamatan, Gojek, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")
gojekFilteredJoin <- geo_join(kecamatan, GojekFiltered, 'ADM3_PCODE', 'CODE', by = NULL, how = "inner")


kkColor <- colorBin("YlOrRd", kkjoin@data$avg.Net.Sales, 6, pretty = FALSE)
SBColor <- colorBin("Greens", sbjoin@data$avg.Net.Sales, 6, pretty = FALSE)
KFCColor <- colorBin("PuRd", kfcjoin@data$avg..Average , 6, pretty = FALSE)
GojekColor <- colorBin("YlOrRd", gojekJoin@data$sum.Completed.Order , 6, pretty = FALSE)


kksbjoinMap <- leaflet()%>%
      setView(lat = -6.2088,
              lng = 106.8456, zoom = 10)%>%
      addProviderTiles("CartoDB") %>%
      addResetMapButton()%>%
  
        addPolygons(
          data = kecamatan_jawa,
          group = "kecamatan",
          weight = 0.2,
          opacity = 0.2,
          fillOpacity = 0,
          label = ~paste(ADM3_EN,ADM2_EN,ADM1_EN), 
          popup = ~paste0("<b>",ADM3_EN," ",ADM2_EN," ",ADM1_EN,"</b>"),
          highlight = highlightOptions(weight = 1, color = "red",
                                       bringToFront = TRUE)) %>%
           addPolygons(
                 data = kkjoin,
                 group = "KopiKenangan",
                 weight = 1,
                 color = ~kkColor(avg.Net.Sales),
                 opacity = 1,
                 fillOpacity = 0.7,
                 label = ~paste(ADM3_EN), 
                 popup = ~paste0(
                       "<b>",status,"</b>",
                       "<br/>","<b>","Sub-district : ",ADM3_EN," ",ADM2_EN,"</b>",
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
               color = ~SBColor(avg.Net.Sales),
               opacity = 1,
               fillOpacity = 0.7,
               label = ~paste(ADM3_EN), 
               popup = ~paste0(
                     "<b>",status,"</b>",
                     "<br/>","<b>","Sub-district : ",ADM3_EN," ",ADM2_EN,"</b>",
                     "<br/>","Store Count : ",count.Site.Name,
                     "<br/>","Z-Score  : ",round(Z.SCORE,digits = 2),
                     "<br/>","Average Gmv (Week) : ",
                     format(avg.Net.Sales,big.mark=".",decimal.mark = ",")),
               highlight = highlightOptions(weight = 1, color = "red",
              bringToFront = TRUE)) %>%
  
            addPolygons(
              data = kfcjoin,
              group = "KFC",
              weight = 1,
              color = ~KFCColor(avg..Average),
              opacity = 1,
              fillOpacity = 0.7,
              label = ~paste(ADM3_EN), 
              popup = ~paste0(
                "<b>",Status,"</b>",
                "<br/>","<b>","Sub-district : ",ADM3_EN," ",ADM2_EN,"</b>",
                "<br/>","Store Count : ",count.Store,
                "<br/>","Z-Score  : ",round(avg..zScore,digits = 2),
                "<br/>","Average Gmv (Month) : ",
                format(avg..Average,big.mark=".",decimal.mark = ",")),
              highlight = highlightOptions(weight = 1, color = "red",
                                           bringToFront = TRUE)) %>%
      addLayersControl(overlayGroups = c("KopiKenangan", "Starbucks","KFC","kecamatan"))

GojekSales <- leaflet()%>%
    setView(lat = -6.2088,
            lng = 106.8456, zoom = 10)%>%
    addProviderTiles("CartoDB") %>%
    addResetMapButton()%>%
  
    addPolygons(
      data = gojekJoin,
      group = "Gojek-all",
      weight = 1,
      color = ~GojekColor(sum.Completed.Order),
      opacity = 1,
      fillOpacity = 0.7,
      label = ~paste(ADM3_EN), 
      popup = ~paste0(
        "<b>","Sub-district : ",ADM3_EN," ",ADM2_EN,"</b>",
        "<br/>","Z-Score  : ",round(Zscore,digits = 2),
        "<br/>","Order Count : ",
        format(sum.Completed.Order,big.mark=".",decimal.mark = ",")),
      highlight = highlightOptions(weight = 1, color = "red",
                                   bringToFront = TRUE)) %>%
      addPolygons(
        data = gojekFilteredJoin,
        group = "Gojek-Filtered",
        weight = 1,
        color = ~GojekColor(sum.Completed.Order),
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste(ADM3_EN), 
        popup = ~paste0(
          "<b>","Sub-district : ",ADM3_EN," ",ADM2_EN,"</b>",
          "<br/>","Z-Score  : ",round(Zscore,digits = 2),
          "<br/>","Order Count : ",
          format(sum.Completed.Order,big.mark=".",decimal.mark = ",")),
        highlight = highlightOptions(weight = 1, color = "red",
                                     bringToFront = TRUE)) %>%
  
  addLayersControl(overlayGroups = c("Gojek-all", "Gojek-Filtered"))
  
  


