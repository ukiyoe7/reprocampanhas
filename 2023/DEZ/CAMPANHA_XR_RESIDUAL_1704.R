
## PERIODO DE REFERENCIA OUT NOV 23
## CANPANHA VARILUX XR
## SANDRO JAKOSKA


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")


CP_RESULT_VLX_XR_1704_sql <- read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\VARILUX_XR_1704.sql')

CP_RESULT_VLX_XR_1704<-  dbGetQuery(con2,CP_RESULT_VLX_XR_1704_sql) 

View(CP_RESULT_VLX_XR_1704)


## RESULT XR

result_vlx_xr_1704 <-
  CP_RESULT_VLX_XR_1704  %>% group_by(CLICODIGO) %>% 
  summarize(QTD_PAR=sum(QTD)/2) 


## TRANSITIONS

result_vlx_xr_1704_trans <-
  CP_RESULT_VLX_XR_1704  %>%  filter(TRANSITIONS==1) %>%  group_by(CLICODIGO) %>% 
  summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 


result_vlx_xr_1704_xr_all <-
  left_join(result_vlx_xr_1704,result_vlx_xr_1704_trans,by="CLICODIGO")  %>% 
   mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
    mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
     CONJUNTO_DE_5_PARES * 500, ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                                                QTD_PAR_TRANSITIONS * 500,0)))


View(result_vlx_xr_1704_xr_all)



write.csv2(result_vlx_xr_1704_xr_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\result_vlx_xr_1704_xr_all.csv",row.names = FALSE,na="")


write.csv2(CP_RESULT_VLX_XR_1704,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\DEZ\\CP_RESULT_VLX_XR_1704.csv",row.names = FALSE,na="")





