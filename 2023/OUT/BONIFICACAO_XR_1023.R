
## PERIODO DE REFERENCIA 1023
## CANPANHA VARILUX XR
## SANDRO JAKOSKA


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

## SQL

CP_RESULT_VLX_XR_sql <- read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\VARILUX_XR_1023.sql')

CP_RESULT_VLX_XR<-  dbGetQuery(con2,CP_RESULT_VLX_XR_sql) 

View(CP_RESULT_VLX_XR)

## RESULT XR

result_vlx_xr <- 
CP_RESULT_VLX_XR %>% group_by(CLICODIGO) %>% summarize(QTD_PAR=sum(QTD)/2) 


## RESULT TRANSITIONS

result_vlx_xr_trans <- 
  CP_RESULT_VLX_XR %>% filter(TRANSITIONS==1) %>% group_by(CLICODIGO) %>% summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 



## CLIENTES CAMPANHA

save(Campanha_bonificação_XR,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\Campanha_bonificação_XR.RData" )

cli_campanha_xr <- Campanha_bonificação_XR %>% select(CLICODIGO) %>% distinct()

View(cli_campanha_xr)


## LIST PEDIDOS

list_pedidos_xr <-
inner_join(CP_RESULT_VLX_XR,cli_campanha_xr,by="CLICODIGO") 

View(list_pedidos_xr)

## SUMMARY RESULTS

result_vlx_campanha_xr <-
left_join(cli_campanha_xr,result_vlx_xr,by="CLICODIGO") %>% 
   arrange(desc(QTD_PAR))

View(result_vlx_campanha_xr)


result_vlx_campanha_xr_trans <-
  left_join(cli_campanha_xr,result_vlx_xr_trans,by="CLICODIGO") %>% 
  arrange(desc(QTD_PAR_TRANSITIONS))

View(result_vlx_campanha_xr_trans)

result_vlx_campanha_xr2 <-
left_join(result_vlx_campanha_xr,result_vlx_campanha_xr_trans,by="CLICODIGO") 


## CREATE COLUMNS

result_vlx_campanha_xr3 <-
  result_vlx_campanha_xr2 %>% 
  mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
  mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
                              CONJUNTO_DE_5_PARES * 500,
                              ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                     QTD_PAR_TRANSITIONS * 500,
                                     0)))


  
  View(result_vlx_campanha_xr3)
  
  
 ## JOIN CLI INFO 

  result_vlx_campanha_xr4 <-
left_join(result_vlx_campanha_xr3,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 
  
  View(result_vlx_campanha_xr4)
  
## CREATE FILE  

write.csv2(list_pedidos_xr,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\list_pedidos_xr.csv",row.names = FALSE,na="")
  
      
write.csv2(result_vlx_campanha_xr4,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\result_vlx_campanha_xr4.csv",row.names = FALSE,na="")


