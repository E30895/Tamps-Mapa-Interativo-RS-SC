#PACOTES
library(readxl)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis) #pacote de palhetas
library(readr)
source(local = TRUE, file = 'formata_moeda.R')

#ENTRADA
capag <- read_xlsx("capag.xlsx", sheet = 1)
mymap <- geobr::read_municipality(code_muni = c("RS", "SC"), year = 2020, showProgress = FALSE)


#PROCESSAMENTO
capag1 <- capag
capag1$Endividamento <- round(capag1$Endividamento_pib, digits = 4)
capag1$Poupança <- round(capag1$Poupança_Corrente, digits = 4)
capag1$Liquidez <- round(capag1$Liquidez, digits = 4)
capag1 <- capag1[,-c(2,3,4,5,6,7,8,10,11,12)]

capag2 <- capag
capag2 <- capag2[,-c(2,3,4,5,6,7,8,9,10)]
capag2 <- capag2 %>%
  mutate(Limite = case_when(Nota_CAPAG == "A" ~ RCL*0.12, 
                            Nota_CAPAG == "B" ~ RCL*0.08,
                            Nota_CAPAG == "C" ~ RCL*0.03))

capag2$RCL <- format_real(capag2$RCL, 2)
capag2$Limite <- format_real(capag2$Limite, 2)


mymap <- as.data.frame(mymap)
mapa_and_data <- inner_join(mymap,capag, by = "code_muni")
mapa_and_data <- st_as_sf(mapa_and_data) #Transforamdo em 'formato especial'
tmap_mode("view")

#SAÍDA
tm_shape(mapa_and_data) +
  tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_fill("Nota_CAPAG", id = "name_muni", palette = "viridis", 
          popup.vars = c(
            "População", 
            "Endividamento%pib", 
            "Poupança_Corrente", 
            "Liquidez", 
            "Nota_CAPAG"), 
          alpha = 0.4) + tm_borders() + tm_scale_bar()

  
