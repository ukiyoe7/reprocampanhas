
## PERIODO DE REFERENCIA 1223
## CANPANHA VARILUX XR
## SANDRO JAKOSKA


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readr)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")


## SQL

CP_RESULT_VLX_XR_sql <- read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\VARILUX_XR_1223.sql')

CP_RESULT_VLX_XR<-  dbGetQuery(con2,CP_RESULT_VLX_XR_sql) 

View(CP_RESULT_VLX_XR)

CP_RESULT_VLX_XR_all <- 
  CP_RESULT_VLX_XR %>% filter(PEDDTEMIS>=as.Date('2023-11-01')) %>% filter(GCLCODIGO!=151 | is.na(GCLCODIGO))

CP_RESULT_VLX_XR_G151 <- 
  CP_RESULT_VLX_XR %>% filter(PEDDTEMIS>=as.Date('2023-11-20')) %>%  filter(GCLCODIGO==151)


## TODOS OS CLIENTES =========================


## RESULT XR

result_vlx_xr_all <- 
  CP_RESULT_VLX_XR_all %>% group_by(CLICODIGO,GCLCODIGO) %>% summarize(QTD_PAR=sum(QTD)/2) 


## RESULT TRANSITIONS

result_vlx_xr_trans_all <- 
  CP_RESULT_VLX_XR_all %>% filter(TRANSITIONS==1) %>% group_by(CLICODIGO,GCLCODIGO) %>% summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 

## join 


result_vlx_campanha_xr2_all <-
  left_join(result_vlx_xr_all,result_vlx_xr_trans_all %>% select(-GCLCODIGO),by="CLICODIGO") 

View(result_vlx_campanha_xr2_all)



## CREATE COLUMNS

result_vlx_campanha_xr3_all <-
  result_vlx_campanha_xr2_all %>% 
  mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
  mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
                              CONJUNTO_DE_5_PARES * 500,
                              ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                     QTD_PAR_TRANSITIONS * 500,
                                     0)))



View(result_vlx_campanha_xr3_all)


## JOIN CLI INFO 

result_vlx_campanha_xr4_all <-
  left_join(result_vlx_campanha_xr3_all,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 

View(result_vlx_campanha_xr4_all)

## CREATE FILE  

write.csv2(CP_RESULT_VLX_XR_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\list_pedidos_xr_all.csv",row.names = FALSE,na="")


write.csv2(result_vlx_campanha_xr4_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\result_vlx_campanha_xr4_all.csv",row.names = FALSE,na="")

## SWELL G151 =========================

## RESULT XR

result_vlx_xr_G151 <- 
  CP_RESULT_VLX_XR_G151 %>% group_by(CLICODIGO,GCLCODIGO) %>% summarize(QTD_PAR=sum(QTD)/2) 


## RESULT TRANSITIONS

result_vlx_xr_trans_G151 <- 
  CP_RESULT_VLX_XR_G151 %>% filter(TRANSITIONS==1) %>% group_by(CLICODIGO,GCLCODIGO) %>% summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 

## join 


result_vlx_campanha_xr2_G151 <-
  left_join(result_vlx_xr_G151,result_vlx_xr_trans_G151 %>% select(-GCLCODIGO),by="CLICODIGO") 

View(result_vlx_campanha_xr2_G151)



## CREATE COLUMNS

result_vlx_campanha_xr3_G151 <-
  result_vlx_campanha_xr2_G151 %>% 
  mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
  mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
                              CONJUNTO_DE_5_PARES * 500,
                              ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                     QTD_PAR_TRANSITIONS * 500,
                                     0)))



View(result_vlx_campanha_xr3_G151)


## JOIN CLI INFO 

result_vlx_campanha_xr4_G151 <-
  left_join(result_vlx_campanha_xr3_G151,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 

View(result_vlx_campanha_xr4_G151)

## CREATE FILE  

write.csv2(CP_RESULT_VLX_XR_G151,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\list_pedidos_xr_G151.csv",row.names = FALSE,na="")


write.csv2(result_vlx_campanha_xr4_G151,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\result_vlx_campanha_xr4_G151.csv",row.names = FALSE,na="")




