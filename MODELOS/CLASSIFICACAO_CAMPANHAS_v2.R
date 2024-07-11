## CLASSIFICATION CAMPAIGNS


## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)

con2 <- dbConnect(odbc::odbc(), "repro", encoding = "latin1")

## LOAD BASES ==================================

BASE_CAMPANHAS <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\Base_campanhas.xlsx") 


BASE_PARTICIPANTES <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\Base_participantes.xlsx") 



## SQL ==========================================


pedidos <- dbGetQuery(con2, statement = read_file('MODELOS/PEDIDOS.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))

View(pedidos)

promo <- dbGetQuery(con2, statement = read_file('MODELOS/PROMO.sql')) %>% mutate(PROMO=as.integer(PROMO))

pedidos_2 <-
  left_join(pedidos,promo,by="ID_PEDIDO") %>% mutate(PROMO=if_else(is.na(PROMO),0,PROMO))


modelos <- dbGetQuery(con2, statement = read_file('MODELOS/MODELOS_CAMPANHAS.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))


##  CLASSIFICATION PRODUCTS =============================== 

pedidos_class1 <-
  
  left_join(
    inner_join(pedidos_2,modelos %>% distinct(PROCODIGO),by=c("PROCODIGO"))
    
    ,modelos,by=c("PROCODIGO")) %>% 
  mutate(ID_PEDIDO=as.numeric(ID_PEDIDO))


View(pedidos_class1)


##  CLASSIFICATION WITHIN DATE =============================== 

pedidos_class2 <-
left_join(pedidos_class1,BASE_CAMPANHAS,by=c("CLICODIGO"="Cliente")) %>% 
  mutate(DATA_DENTRO=ifelse(PEDDTEMIS >= `Data Início` & PEDDTEMIS <= `Data Final`, 1, 0))


    View(pedidos_class2)

##  CLASSIFICATION PED PARAM =============================== 

ped_param <- data.frame(
  ORIGEM_WEB=1,
  VENDA=1,
  QTD=2,
  PED_PARAM=1
)

View(ped_param)        
    
pedidos_class3 <- 
left_join(pedidos_class2,ped_param,by=c("ORIGEM_WEB","VENDA","QTD")) 


View(pedidos_class3)
    
    
    
    
    
    
