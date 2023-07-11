
## CAMPANHA TRANSITIONS PONTOS ESSILOR Q2 2023 APURAÇÃO

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(glue)
library(readr)
library(gmailr)
library(xlsx)
library(readxl)


con2 <- dbConnect(odbc::odbc(), "reproreplica")


## GET FINAL RESULT ============================================

CP_RESULT_CLIENTES_TGEN8_Q2 <- 
read_sheet("1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",sheet = "RESUMO PAGAMENTOS")

View(CP_RESULT_CLIENTES_TGEN8_Q2)

## PEDIDOS ======================================================

CP_RESULT_TGEN8_Q2_sql <- glue_sql(read_file('C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\REPORTS\\SQL\\CAMPANHA_TRANSITIONS_Q2.sql'))

CP_RESULT_TGEN8_Q2<-  dbGetQuery(con2,CP_RESULT_TGEN8_Q2_sql) 


CP_RESULT_TGEN8_Q2 %>% summarize(V=sum(VRVENDA))

CP_PEDIDOS_TGEN8_Q2 <-
inner_join(CP_RESULT_TGEN8_Q2,CP_RESULT_CLIENTES_TGEN8_Q2 %>% select(CLICODIGO),by="CLICODIGO")

View(CP_PEDIDOS_TGEN8_Q2)

## PAGAMENTOS =====================================

CP_DADOSCARTAO_TGEN_Q2_A <-
  left_join(CP_RESULT_CLIENTES_TGEN8_Q2 %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% 
              rename(DATA=1) %>% 
              rename(CLICODIGO=2) %>%
              group_by(CLICODIGO) %>% 
              summarize(DATA = max(DATA)), by = "CLICODIGO") 

view(CP_DADOSCARTAO_TGEN_Q2)


CP_DADOSCARTAO_TGEN_Q2_B <-
  left_join(CP_RESULT_CLIENTES_TGEN8_Q2 %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% 
              rename(DATA=1) %>% 
              rename(CLICODIGO=2) %>%
              rename(NOME=3) 
            ,by="CLICODIGO") 

CP_DADOSCARTAO_TGEN_Q2_C <-
  inner_join(CP_DADOSCARTAO_TGEN_Q2_B ,
             CP_DADOSCARTAO_TGEN_Q2_A,by=c("CLICODIGO","DATA")) %>% .[,1:4] %>% as.data.frame() %>% 
              mutate(CPF=as.character(CPF))

View(CP_DADOSCARTAO_TGEN_Q2_C)


## RESUMO FINAL

write.csv2(CP_RESULT_CLIENTES_TGEN8_Q2,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\PONTOS ESSILOR Q2\\CP_RESULT_CLIENTES_TGEN8_Q2.csv")

write.csv2(CP_PEDIDOS_TGEN8_Q2,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\PONTOS ESSILOR Q2\\CP_PEDIDOS_TGEN8_Q2.csv")

write.csv2(CP_DADOSCARTAO_TGEN_Q2_C,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\PONTOS ESSILOR Q2\\CP_DADOSCARTAO_TGEN_Q2_C.csv")


## PAGAMENTOS ===================================================================


## CARTOES

CARTOES_110723 <- read_excel("~/CAMPANHAS/2023/JUN/CARTOES_110723.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_110723 <- CARTOES_110723 %>% `colnames<-`(cols2)

View(CARTOES_110723)

left_join(
CP_DADOSCARTAO_TGEN_Q2_C,CARTOES_110723,by="CPF") %>% View()

BASE_PAGAMENTOS <-
left_join(CP_RESULT_CLIENTES_TGEN8_Q2,CP_DADOSCARTAO_TGEN_Q2_C,by="CLICODIGO") %>%
  left_join(.,CARTOES_110723 %>% filter(STATUS!="Cancelado"),by="CPF")

view(BASE_PAGAMENTOS)

## CLIENTES COM CARTAO

BASE_PAGAMENTOS2 <-
  BASE_PAGAMENTOS %>% filter(!is.na(NSERIE))

View(BASE_PAGAMENTOS2)

BASE_PAGAMENTOS3 <-
BASE_PAGAMENTOS2 %>% select(NSERIE,CPF,BONUS) %>% 
   mutate(Observacao="CAMPANHAS PONTOS ESSILOR TRANSTIONS Q2 23") %>% 
     rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))

View(BASE_PAGAMENTOS3)

BASE_PAGAMENTOS3 %>%  summarize(V=sum(`Valor da Carga`))

## CLIENTES SEM CARTAO

BASE_PAGAMENTOS4 <-
  BASE_PAGAMENTOS %>% filter(is.na(NSERIE))

View(BASE_PAGAMENTOS4)

