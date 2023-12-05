
## PERIODO DE REFERENCIA 1123
## REBATES
## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readr)


con2 <- dbConnect(odbc::odbc(), "repro", encoding = "latin1")


query_1123 <- dbGetQuery(con2, statement = read_file('C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/RESULT_NOV23.sql'))

View(query_1123)

query_1123 %>% summarize(V=sum(VRVENDA))

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## G139 SCHROEDER 31.12.2023 ===============================================================================
# Rebate 7% para vendedoras e 3% para montador

REBATE_RESULT_G139_1123 <-
  query_1123 %>% 
  filter(GCLCODIGO==139)

View(REBATE_RESULT_G139_1123)

REBATE_RESULT_G139_1123 %>%  summarize(v=sum(VRVENDA)) # total sales
REBATE_RESULT_G139_1123 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
REBATE_RESULT_G139_1123 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(REBATE_RESULT_G139_1123 %>%  summarize(v=sum(VRVENDA)*0.07),REBATE_RESULT_G139_1123 %>% summarize(v=sum(VRVENDA)*0.03)) #totaL



CP_G139_1123_IPEDIDOS <- REBATE_RESULT_G139_1123 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>%
  mutate(DATA=format(DATA,"%d/%m/%y")) 


 write.csv2(CP_G139_1123_IPEDIDOS,file = "C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/CP_G139_1123_IPEDIDOS.CSV", row.names = FALSE,na="")



## obtem dados planilha identificação CPFs

 
 save(CPF_G139_1123,file = "C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/CPF_G139_1123.RData")

CPF_G139_1123 <- get(load("C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/CPF_G139_1123.RData")) %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(ID_PEDIDO=as.character(ID_PEDIDO))

View(CPF_G139_1123)

## GET CONTRACT

REBATE_CONTRATO_G139_1123 <- range_read("1rz7vrxiSq4iavtvJ8dlkAX3JuzrxyaTwa0gyYeUL5h8",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_G139_1123 <- REBATE_CONTRATO_G139_1123 %>% 
  mutate(A=REBATE_RESULT_G139_1123 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G139_1123 <- REBATE_RESULT_G139_1123  %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G139_1123)*VRVENDA)

REBATE_BONIFICACAO_G139_1123 %>% summarize(v=sum(BONUS))


## OBS

REBATE_OBS_G139_1123 <- paste0("SCHROEDER G139 ","REBATE ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))

## LISTA PEDIDOS

REBATE_LISTAGEM_G139_1123 <- inner_join(REBATE_BONIFICACAO_G139_1123 %>% mutate(ID_PEDIDO=as.character(ID_PEDIDO)),CPF_G139_1123 %>% mutate(ID_PEDIDO=as.character(ID_PEDIDO)),by="ID_PEDIDO") %>% 
  mutate(OBS=REBATE_OBS_G139_1123) %>% .[,c(-12,-13,-14)]

View(REBATE_LISTAGEM_G139_1123)

CPF_G139_1123 %>% filter(CPF=='09630382903') %>% summarize(v=sum(VALOR.VENDA))



## PAGAMENTOS 

REBATE_PAGAMENTO_VENDEDORAS_G139_1123 <- REBATE_LISTAGEM_G139_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% as.data.frame() %>% 
  mutate(OBS=REBATE_OBS_G139_1123) 

REBATE_PAGAMENTO_VENDEDORAS_G139_1123 %>% summarize(v=sum(BONUS))

## montador

REBATE_PAGAMENTO_MONTADOR_G139_1123 <- 
    data.frame(CPF=c("88717020930"),
             BONUS=(REBATE_LISTAGEM_G139_1123 %>% 
                      summarize(BONUS=round(sum(VRVENDA)*0.03,0))),OBS=REBATE_OBS_G139_1123) 


REBATE_PAGAMENTO_MONTADOR_G139_1123_2 <- 
  REBATE_PAGAMENTO_MONTADOR_G139_1123 %>% as.data.frame() %>%  mutate(BONUS=BONUS)



## TOTAL
REBATE_PAGAMENTO_G139_1123 <-
  rbind(REBATE_PAGAMENTO_VENDEDORAS_G139_1123,REBATE_PAGAMENTO_MONTADOR_G139_1123_2)

View(REBATE_PAGAMENTO_G139_1123)

REBATE_PAGAMENTO_G139_1123 %>% summarize(v=sum(BONUS))




## SQL 849 VITAL 31.12.2023 ===============================================================================

REBATE_RESULT_849_1123 <- query_1123 %>%  filter(CLICODIGO==849)

View(REBATE_RESULT_849_1123)

REBATE_RESULT_849_1123 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_849_1123 <- range_read("18UYA1GHFm2lADAEm-lSh8c8vYR61M3z1dbXKpZ0Lw-E",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_849_1123 <- REBATE_CONTRATO_849_1123 %>% 
  mutate(A=REBATE_RESULT_849_1123 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_849_1123 <- REBATE_RESULT_849_1123 %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_849_1123)*VRVENDA)


## OBS

REBATE_OBS_849_1123 <- paste0("VITAL 849 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

REBATE_LISTAGEM_849_1123 <- REBATE_BONIFICACAO_849_1123 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(REBATE_RESULT_849_1123))) %>% 
  mutate(OBS=REBATE_OBS_849_1123)

View(REBATE_LISTAGEM_849_1123)


## PAGAMENTOS 

REBATE_PAGAMENTO_849_1123 <- REBATE_LISTAGEM_849_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=REBATE_OBS_849_1123) 


View(REBATE_PAGAMENTO_849_1123)


## SQL 157 DOMBOSCO 31.12.2023 ===============================================================================

REBATE_RESULT_157_1123 <- query_1123 %>%  filter(CLICODIGO==157)

View(REBATE_RESULT_157_1123)

REBATE_RESULT_157_1123 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_157_1123 <- range_read("1Oxon4HQST-MqCsQ4bH4rRQHBP-qV-UUGHxeAtrJvH7A",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_157_1123 <- REBATE_CONTRATO_157_1123 %>% 
  mutate(A=REBATE_RESULT_157_1123 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_157_1123 <- REBATE_RESULT_157_1123 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_157_1123)*VRVENDA)


## OBS

REBATE_OBS_157_1123 <- paste0("DOM BOSCO 157 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_157_1123 <- inner_join(REBATE_BONIFICACAO_157_1123,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_157_1123)

View(REBATE_LISTAGEM_157_1123)

REBATE_LISTAGEM_157_1123 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_157_1123 <- REBATE_LISTAGEM_157_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_157_1123) 

View(REBATE_PAGAMENTO_157_1123)



## SQL 1830  PRECIOSA 01.03.2023 - 31.12.2023 ===============================================================================


REBATE_RESULT_1830_1123 <- query_1123 %>%  filter(CLICODIGO==1830)

View(REBATE_RESULT_1830_1123)

REBATE_RESULT_1830_1123 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_1830_1123 <- range_read("1CtNoqyvBhhK1s2OqNbbvWFPxPO4GNNtFnFYpvE_Zm50",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_1830_1123 <- REBATE_CONTRATO_1830_1123 %>% 
  mutate(A=REBATE_RESULT_1830_1123 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_1830_1123 <- REBATE_RESULT_1830_1123 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_1830_1123)*VRVENDA)


## OBS

REBATE_OBS_1830_1123 <- paste0("PRECIOSA 1830 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_1830_1123 <- inner_join(REBATE_BONIFICACAO_1830_1123,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_1830_1123)

View(REBATE_LISTAGEM_1830_1123)

REBATE_LISTAGEM_1830_1123 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_1830_1123 <- REBATE_LISTAGEM_1830_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_1830_1123) 


View(REBATE_PAGAMENTO_1830_1123)



## SQL 4253 BLUE EYE 01.03.2023 - 31.12.2023 ===============================================================================


REBATE_RESULT_4253_1123 <- query_1123 %>%  filter(CLICODIGO==4253)

View(REBATE_RESULT_4253_1123)

REBATE_RESULT_4253_1123 %>% summarize(v=sum(VRVENDA))

REBATE_RESULT_4253_1123 %>% summarize(v=sum(VRVENDA)*0.04)

## GET CONTRACT

REBATE_CONTRATO_4253_1123 <- range_read("1fGH1u27mr5lumG3zi_OxnMXNmA5qIVo6g7fnImQnKa8",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_4253_1123 <- REBATE_CONTRATO_4253_1123 %>% 
  mutate(A=REBATE_RESULT_4253_1123 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4253_1123 <- REBATE_RESULT_4253_1123 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4253_1123)*VRVENDA)


## OBS

REBATE_OBS_4253_1123 <- paste0("BLUE EYE 4253 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_4253_1123 <- inner_join(REBATE_BONIFICACAO_4253_1123,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4253_1123)

View(REBATE_LISTAGEM_4253_1123)

REBATE_LISTAGEM_4253_1123 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_4253_1123 <- REBATE_LISTAGEM_4253_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4253_1123) 


View(REBATE_PAGAMENTO_4253_1123)


## PAGAMENTOS  =========================================================================


REBATE_PAGAMENTO_1123 <- rbind(REBATE_PAGAMENTO_G139_1123,
                               REBATE_PAGAMENTO_849_1123,
                               REBATE_PAGAMENTO_157_1123,
                               REBATE_PAGAMENTO_1830_1123,
                               REBATE_PAGAMENTO_4253_1123
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="REBATE")

View(REBATE_PAGAMENTO_1123)


## LISTAGEM  =========================================================================


REBATE_LISTAGEM_1123 <- rbind(REBATE_LISTAGEM_G139_1123,
                              REBATE_LISTAGEM_849_1123,
                              REBATE_LISTAGEM_157_1123,
                              REBATE_LISTAGEM_1830_1123,
                              REBATE_LISTAGEM_4253_1123
)


View(REBATE_LISTAGEM_1123)


## CREDITO  =========================================================================


## CREDITO CARTOES

CARTOES_1123 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CARTOES_1123.RData"))



CREDITO_CARTOES_REBATES_1123 <- left_join(REBATE_PAGAMENTO_1123 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_1123 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 

View(CREDITO_CARTOES_REBATES_1123)


## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_1123_2 <- CREDITO_CARTOES_REBATES_1123 %>% 
  filter(!is.na(NSERIE)) %>% filter(!is.na(BONUS))

View(CREDITO_CARTOES_REBATES_1123_2)


# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_1123_3 <- CREDITO_CARTOES_REBATES_1123_2  %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_REBATES_1123_3)  

CREDITO_CARTOES_REBATES_1123_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_REBATES_1123_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_REBATES_1123_3,
           file = "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2023\\NOV\\CREDITO_CARTOES_REBATES_1123.csv",
           row.names=FALSE,quote = FALSE)


## EMISSAO CARTOES  =========================================================================


EMISSAO_CARTOES_REBATES_1123 <- CREDITO_CARTOES_REBATES_1123 %>% 
  filter(is.na(NSERIE)) %>% filter(!is.na(BONUS)) %>% filter(PGTO_MINIMO=='S') %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1,2,3,14,15,16)]

View(EMISSAO_CARTOES_REBATES_1123)






