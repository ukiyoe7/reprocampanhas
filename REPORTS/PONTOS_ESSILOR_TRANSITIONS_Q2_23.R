
## CAMPANHA TRANSITIONS PONTOS ESSILOR Q2 2023

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(glue)
library(readr)
library(gmailr)


con2 <- dbConnect(odbc::odbc(), "reproreplica")

## CLIENTES CAMPANHA ============================================================================================


CLIENTES_TGEN8_MAI23 <- read_sheet(ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",sheet = "CLIENTES")


## EXTRACT ============================================================================================


CP_CLIENTES_TGEN8_MAI23_sql <- glue_sql(read_file('C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\REPORTS\\SQL\\CAMPANHA_TRANSITIONS_Q2.sql'))

CP_TRANSITIONS_MAI23<-  dbGetQuery(con2,CP_CLIENTES_TGEN8_MAI23_sql) 


## REPORT ============================================================================================

## GET CLI

cli <- dbGetQuery(con2, statement = read_file('C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\SQL\\CLIENTS.sql'))


## SUMMARY

RESULT_TRANS_Q2 <-
  CP_TRANSITIONS_MAI23 %>%  group_by(CLICODIGO) %>% summarize(QTD=sum(QTD))

RESULT_TRANS_Q2_2 <-
  left_join(CLIENTES_TGEN8_MAI23 %>% select(CLICODIGO),RESULT_TRANS_Q2,by="CLICODIGO") %>% 
  left_join(.,cli %>% select(CLICODIGO,CLINOMEFANT,SETOR),by="CLICODIGO")%>% 
  .[,c(1,3,4,2)] %>% arrange(desc(QTD))

## WRITE GOOGLE

range_write(ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",data = RESULT_TRANS_Q2_2,range = "A:D",sheet = "RESUMO",reformat = FALSE)

range_write(ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",data = CP_TRANSITIONS_MAI23 ,range = "A:K",sheet = "PEDIDOS",reformat = FALSE)



## SEND MAIL ==================================================================== 


gm_auth_configure(path = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\REPORTS\\sendmail.json")

mymail <- gm_mime() %>% 
  gm_to("sandro.jakoska@repro.com.br,leandro.fritzen@repro.com.br,cristiano.regis@repro.com.br") %>% 
  gm_from ("comunicacao@repro.com.br") %>%
  gm_subject("RELATORIO CAMPANHA PONTOS ESSILOR Q2") %>%
  gm_text_body("RELATÓRIO ATUALIZADO.

ACESSE O LINK https://docs.google.com/spreadsheets/d/1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ/edit?usp=sharing.


ESSE É UM EMAIL AUTOMÁTICO.") 
gm_send_message(mymail)







