''' CRIANDO MAPAS COM R - TMAPS

PARA CRIAR OS MAPAS S�O NECESS�RIOS ALGUNS ITENS:

1) BASE DE DADOS COM VALORES DE INTERPOLA��O - ALGUM C�DIGO PARA UNIFICAR A BASE COM OUTRA
2) SHAPEFILE DE ONDE QUER PLOTAR OS DADOS - MALHA CARTOGRAFICA

OBS1.: OS C�DIGOS DO ITEM 1 PODEM SER ARBITRARIOS OU N�O - NO CASO DO BRASIL TEM OS C�DIGOS IBGE
OBS2.: � POSS�VEL CRIAR MALHA CARTOGR�FICA COM O QGIS OU ARCGIS
'''
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
capag1$Poupan�a <- round(capag1$Poupan�a_Corrente, digits = 4)
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

#SA�DA
tm_shape(mapa_and_data) +
  tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_fill("Nota_CAPAG", id = "name_muni", palette = "viridis", 
          popup.vars = c(
            "Popula��o", 
            "Endividamento%pib", 
            "Poupan�a_Corrente", 
            "Liquidez", 
            "Nota_CAPAG"), 
          alpha = 0.4) + tm_borders() + tm_scale_bar()

  