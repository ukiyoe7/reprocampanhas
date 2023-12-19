
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

CP_RESULT_VLX_XR_all <- 
CP_RESULT_VLX_XR %>% filter(PEDDTEMIS>=as.Date('2023-09-01')) %>% filter(GCLCODIGO!=77 | is.na(GCLCODIGO))

CP_RESULT_VLX_XR_g77 <- 
CP_RESULT_VLX_XR %>% filter(PEDDTEMIS>=as.Date('2023-08-29')) %>%  filter(GCLCODIGO==77)



## CLIENTES CAMPANHA =============================================

save(Campanha_bonificação_XR,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\Campanha_bonificação_XR.RData" )

cli_campanha_xr <- Campanha_bonificação_XR %>% select(CLICODIGO) %>% distinct()

View(cli_campanha_xr)

cli_campanha_xr_all <-
  left_join(cli_campanha_xr,clien %>% select(CLICODIGO,GCLCODIGO),by="CLICODIGO") %>% filter(GCLCODIGO!=77 | is.na(GCLCODIGO))

View(cli_campanha_xr_all)

cli_campanha_xr_g77 <-
  left_join(cli_campanha_xr,clien %>% select(CLICODIGO,GCLCODIGO),by="CLICODIGO") %>% filter(GCLCODIGO==77)

View(cli_campanha_xr_g77)


## TODOS OS CLIENTES =========================

## RESULT XR

result_vlx_xr_all <- 
  CP_RESULT_VLX_XR_all %>% group_by(CLICODIGO) %>% summarize(QTD_PAR=sum(QTD)/2) 


## RESULT TRANSITIONS

result_vlx_xr_trans_all <- 
  CP_RESULT_VLX_XR_all %>% filter(TRANSITIONS==1) %>% group_by(CLICODIGO) %>% summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 


## LIST PEDIDOS

list_pedidos_xr_all <-
inner_join(CP_RESULT_VLX_XR_all,cli_campanha_xr_all %>% select(-GCLCODIGO),by="CLICODIGO") 

View(list_pedidos_xr_all)


## SUMMARY RESULTS

result_vlx_campanha_xr_all <-
left_join(cli_campanha_xr_all,result_vlx_xr_all,by="CLICODIGO") %>% 
   arrange(desc(QTD_PAR))

View(result_vlx_campanha_xr_all)


result_vlx_campanha_xr_trans_all <-
  left_join(cli_campanha_xr_all,result_vlx_xr_trans_all,by="CLICODIGO") %>% 
   select(-GCLCODIGO)

View(result_vlx_campanha_xr_trans_all)

result_vlx_campanha_xr2_all <-
left_join(result_vlx_campanha_xr_all,result_vlx_campanha_xr_trans_all,by="CLICODIGO") 

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

write.csv2(list_pedidos_xr_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\list_pedidos_xr_all.csv",row.names = FALSE,na="")
  
      
write.csv2(result_vlx_campanha_xr4_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\result_vlx_campanha_xr4_all.csv",row.names = FALSE,na="")


## DANJU =========================


## RESULT XR

result_vlx_xr_g77 <- 
  CP_RESULT_VLX_XR_g77 %>% group_by(CLICODIGO) %>% summarize(QTD_PAR=sum(QTD)/2) 


## RESULT TRANSITIONS

result_vlx_xr_trans_g77 <- 
  CP_RESULT_VLX_XR_g77 %>% filter(TRANSITIONS==1) %>% group_by(CLICODIGO) %>% summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 


## LIST PEDIDOS

list_pedidos_xr_g77 <-
  inner_join(CP_RESULT_VLX_XR_g77,cli_campanha_xr_g77 %>% select(-GCLCODIGO),by="CLICODIGO") 

View(list_pedidos_xr_g77)


## SUMMARY RESULTS

result_vlx_campanha_xr_g77 <-
  left_join(cli_campanha_xr_g77,result_vlx_xr_g77,by="CLICODIGO") %>% 
  arrange(desc(QTD_PAR))

View(result_vlx_campanha_xr_g77)


result_vlx_campanha_xr_trans_g77 <-
  left_join(cli_campanha_xr_g77,result_vlx_xr_trans_g77,by="CLICODIGO") %>% 
  arrange(desc(QTD_PAR_TRANSITIONS)) %>% select(-GCLCODIGO)

View(result_vlx_campanha_xr_trans_g77)

result_vlx_campanha_xr2_g77 <-
  left_join(result_vlx_campanha_xr_g77,result_vlx_campanha_xr_trans_g77,by="CLICODIGO") 

View(result_vlx_campanha_xr2_g77)



## CREATE COLUMNS

result_vlx_campanha_xr3_g77 <-
  result_vlx_campanha_xr2_g77 %>% 
  mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
  mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
                              CONJUNTO_DE_5_PARES * 500,
                              ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                     QTD_PAR_TRANSITIONS * 500,
                                     0)))



View(result_vlx_campanha_xr3_g77)


## JOIN CLI INFO 

result_vlx_campanha_xr4_g77 <-
  left_join(result_vlx_campanha_xr3_g77,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 

View(result_vlx_campanha_xr4_g77)


## CREATE FILE  

write.csv2(list_pedidos_xr_g77,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\list_pedidos_xr_g77.csv",row.names = FALSE,na="")


write.csv2(result_vlx_campanha_xr4_g77,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\result_vlx_campanha_xr4_g77.csv",row.names = FALSE,na="")





