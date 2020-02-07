install.packages("leaflet")
library(leaflet)
library(readxl)
ALL_STORE <- read_excel("~/Downloads/ALL-STORE.xlsx")
df_store <- data.frame(ALL_STORE)

map_store <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircles(data = df_store, color = "green", radius = 5000,opacity = .005)%>%
  addCircles(data = df_store, color = "red", radius = 1,label = df_store$store_name)

