
## PERIODO DE REFERENCIA OUT NOV 23
## CANPANHA VARILUX XR
## SANDRO JAKOSKA


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

## SQL

CP_RESULT_VLX_XR_1123_sql <- read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\VARILUX_XR_1123.sql')

CP_RESULT_VLX_XR_1123<-  dbGetQuery(con2,CP_RESULT_VLX_XR_1123_sql) 

View(CP_RESULT_VLX_XR_1123)


## CLIENTES CAMPANHA =============================================

save(CLIENTES_CAMPANHA_XR_NOV23,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CLIENTES_CAMPANHA_XR_NOV23.RData" )

CLIENTES_CAMPANHA_XR_NOV23_2 <- CLIENTES_CAMPANHA_XR_NOV23 %>% select(CLICODIGO) %>% distinct()

View(CLIENTES_CAMPANHA_XR_NOV23_2)


cli_campanha_xr_1123_all <-
  inner_join(CLIENTES_CAMPANHA_XR_NOV23_2,cli %>% select(CLICODIGO,GCLCODIGO),by="CLICODIGO") %>% filter(GCLCODIGO!=45 | is.na(GCLCODIGO))

View(cli_campanha_xr_1123_all)

cli_campanha_xr_1123_g45 <-
  inner_join(CLIENTES_CAMPANHA_XR_NOV23_2,cli %>% select(CLICODIGO,GCLCODIGO),by="CLICODIGO") %>% filter(GCLCODIGO==45)

View(cli_campanha_xr_1123_g45)


## TODOS OS CLIENTES =========================

## RESULT XR

result_vlx_xr_1123_all <- 
  inner_join(CP_RESULT_VLX_XR_1123 %>% filter(PEDDTEMIS>=as.Date('2023-10-18')),cli_campanha_xr_1123_all %>% select(-GCLCODIGO),by="CLICODIGO") 

View(result_vlx_xr_1123_all)


result_vlx_xr_1123_all_2 <-
  result_vlx_xr_1123_all  %>% group_by(CLICODIGO) %>% 
  summarize(QTD_PAR=sum(QTD)/2) 



## TRANSITIONS

result_vlx_xr_1123_all_trans <- 
  inner_join(result_vlx_xr_1123_all,cli_campanha_xr_1123_all %>% select(-GCLCODIGO),by="CLICODIGO") %>% filter(TRANSITIONS==1)


result_vlx_xr_1123_all_trans_2 <-
  result_vlx_xr_1123_all_trans %>% group_by(CLICODIGO) %>% 
  summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 


## CREATE COLUMNS

result_vlx_campanha_xr_all <-
  left_join(result_vlx_xr_1123_all_2,result_vlx_xr_1123_all_trans_2,by="CLICODIGO")  %>% 
  mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
  mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
                              CONJUNTO_DE_5_PARES * 500, ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                                                QTD_PAR_TRANSITIONS * 500,0)))


View(result_vlx_campanha_xr_all)



## JOIN CLI INFO 

result_vlx_campanha_xr_all_2 <-
  left_join(result_vlx_campanha_xr_all,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 

View(result_vlx_campanha_xr_all_2)


## CREATE FILE  

write.csv2(result_vlx_xr_1123_all,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\result_vlx_xr_1123_all.csv",row.names = FALSE,na="")


write.csv2(result_vlx_campanha_xr_all_2,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\result_vlx_campanha_xr_all_2.csv",row.names = FALSE,na="")



## DINIZ ======================================================


## RESULT XR

result_vlx_xr_1123_g45 <- 
inner_join(CP_RESULT_VLX_XR_1123 %>% filter(PEDDTEMIS>=as.Date('2023-09-01')),cli_campanha_xr_1123_g45 %>% select(-GCLCODIGO),by="CLICODIGO") 

View(result_vlx_xr_1123_g45)


result_vlx_xr_1123_g45_2 <-
result_vlx_xr_1123_g45 %>% group_by(CLICODIGO) %>% 
  summarize(QTD_PAR=sum(QTD)/2) 


## TRANSITIONS

result_vlx_xr_1123_g45_trans <- 
  inner_join(result_vlx_xr_1123_g45,cli_campanha_xr_1123_g45 %>% select(-GCLCODIGO),by="CLICODIGO") %>% filter(TRANSITIONS==1)


result_vlx_xr_1123_g45_trans_2 <-
  result_vlx_xr_1123_g45_trans %>% group_by(CLICODIGO) %>% 
  summarize(QTD_PAR_TRANSITIONS=sum(QTD)/2) 


left_join(result_vlx_xr_1123_g45_2,result_vlx_xr_1123_g45_trans_2,by="CLICODIGO") 

## CREATE COLUMNS

result_vlx_campanha_xr_g45 <-
  left_join(result_vlx_xr_1123_g45_2,result_vlx_xr_1123_g45_trans_2,by="CLICODIGO")  %>% 
     mutate(CONJUNTO_DE_5_PARES = floor(QTD_PAR / 5)) %>% 
      mutate(BONIFICACAO = ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS >= CONJUNTO_DE_5_PARES,
       CONJUNTO_DE_5_PARES * 500, ifelse(CONJUNTO_DE_5_PARES > 0 & QTD_PAR_TRANSITIONS <= CONJUNTO_DE_5_PARES,
                                     QTD_PAR_TRANSITIONS * 500,0)))



View(result_vlx_campanha_xr_g45)


## JOIN CLI INFO 

result_vlx_campanha_xr_g45_2 <-
  left_join(result_vlx_campanha_xr_g45,clien %>% select(CLICODIGO,CLINOMEFANT,SETOR,GCLNOME),by="CLICODIGO") 

View(result_vlx_campanha_xr_g45_2)


## CREATE FILE  

write.csv2(result_vlx_xr_1123_g45,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\result_vlx_xr_1123_g45.csv",row.names = FALSE,na="")


write.csv2(result_vlx_campanha_xr_g45_2,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\result_vlx_campanha_xr_g45_2.csv",row.names = FALSE,na="")


## DADOS PAGAMENTO ===============================================


rbind(
left_join(
result_vlx_campanha_xr_all_2 %>% filter(BONIFICACAO!=0) %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CLICODIGO") 
,
left_join(
  result_vlx_campanha_xr_g45_2 %>% filter(BONIFICACAO!=0) %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA  %>% mutate(CPF=as.character(CPF)),by="CLICODIGO") ) %>% 

  write.csv2(.,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\result_vlx_campanha_xr_dados.csv",row.names = FALSE,na="")














