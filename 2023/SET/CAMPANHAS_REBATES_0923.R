
## PERIODO DE REFERENCIA 0823
## REBATES
## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readr)


con2 <- dbConnect(odbc::odbc(), "reproreplica", encoding = "latin1")


query_0923 <- dbGetQuery(con2, statement = read_file('C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/SET/RESULT_SET23.sql'))

View(query_0923)

query_0923 %>% summarize(V=sum(VRVENDA))

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## G139 SCHROEDER 31.12.2023 ===============================================================================
# Rebate 7% para vendedoras e 3% para montador

REBATE_RESULT_G139_0923 <-
  query_0923 %>% 
  filter(GCLCODIGO==139)

View(REBATE_RESULT_G139_0923)

REBATE_RESULT_G139_0923 %>%  summarize(v=sum(VRVENDA)) # total sales
REBATE_RESULT_G139_0923 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
REBATE_RESULT_G139_0923 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(REBATE_RESULT_G139_0923 %>%  summarize(v=sum(VRVENDA)*0.07),REBATE_RESULT_G139_0923 %>% summarize(v=sum(VRVENDA)*0.03)) #totaL



CP_G139_0923_IPEDIDOS <- REBATE_RESULT_G139_0923 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>%
  mutate(DATA=format(DATA,"%d/%m/%y")) 



range_write("1Nk6fMjPj7byyPTRIObvflxQSXCOq5I-B04NrZUCKuts",data=CP_G139_0923_IPEDIDOS,sheet = "SET23",range = "A1") 



## obtem dados planilha identificação CPFs


CPF_G139_0923 <- read_sheet("1Nk6fMjPj7byyPTRIObvflxQSXCOq5I-B04NrZUCKuts",sheet = "SET23") %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CPF_G139_0923)

## GET CONTRACT

REBATE_CONTRATO_G139_0923 <- range_read("1rz7vrxiSq4iavtvJ8dlkAX3JuzrxyaTwa0gyYeUL5h8",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_G139_0923 <- REBATE_CONTRATO_G139_0923 %>% 
  mutate(A=REBATE_RESULT_G139_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G139_0923 <- REBATE_RESULT_G139_0923  %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G139_0923)*VRVENDA)

REBATE_BONIFICACAO_G139_0923 %>% summarize(v=sum(BONUS))


## OBS

REBATE_OBS_G139_0923 <- paste0("SCHROEDER G139 ","REBATE ",format(floor_date(Sys.Date(),"month"),"%m%y"))

## LISTA PEDIDOS

REBATE_LISTAGEM_G139_0923 <- inner_join(REBATE_BONIFICACAO_G139_0923,CPF_G139_0923,by="ID_PEDIDO") %>% 
  mutate(OBS=REBATE_OBS_G139_0923) %>% .[,c(-12,-13,-14)]

View(REBATE_LISTAGEM_G139_0923)

CPF_G139_0923 %>% filter(CPF=='09630382903') %>% summarize(v=sum(VALOR.VENDA))



## PAGAMENTOS 

REBATE_PAGAMENTO_VENDEDORAS_G139_0923 <- REBATE_LISTAGEM_G139_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% as.data.frame() %>% 
  mutate(OBS=REBATE_OBS_G139_0923) 

REBATE_PAGAMENTO_VENDEDORAS_G139_0923 %>% summarize(v=sum(BONUS))

## montador

REBATE_PAGAMENTO_MONTADOR_G139_0923 <- 
  data.frame(CPF=c("88717020930"),
             BONUS=(REBATE_LISTAGEM_G139_0923 %>% 
                      summarize(BONUS=round(sum(VRVENDA)*0.03,0))),OBS=REBATE_OBS_G139_0923) 


REBATE_PAGAMENTO_MONTADOR_G139_0923_2 <- 
  REBATE_PAGAMENTO_MONTADOR_G139_0923 %>% as.data.frame() %>%  mutate(BONUS=BONUS)



## TOTAL
REBATE_PAGAMENTO_G139_0923 <-
  rbind(REBATE_PAGAMENTO_VENDEDORAS_G139_0923,REBATE_PAGAMENTO_MONTADOR_G139_0923_2)

View(REBATE_PAGAMENTO_G139_0923)

REBATE_PAGAMENTO_G139_0923 %>% summarize(v=sum(BONUS))


## SQL 849 VITAL 31.12.2023 ===============================================================================

REBATE_RESULT_849_0923 <- query_0923 %>%  filter(CLICODIGO==849)

View(REBATE_RESULT_849_0923)

REBATE_RESULT_849_0923 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_849_0923 <- range_read("18UYA1GHFm2lADAEm-lSh8c8vYR61M3z1dbXKpZ0Lw-E",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_849_0923 <- REBATE_CONTRATO_849_0923 %>% 
  mutate(A=REBATE_RESULT_849_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_849_0923 <- REBATE_RESULT_849_0923 %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_849_0923)*VRVENDA)


## OBS

REBATE_OBS_849_0923 <- paste0("VITAL 849 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

REBATE_LISTAGEM_849_0923 <- REBATE_BONIFICACAO_849_0923 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(REBATE_RESULT_849_0923))) %>% 
  mutate(OBS=REBATE_OBS_849_0923)

View(REBATE_LISTAGEM_849_0923)


## PAGAMENTOS 

REBATE_PAGAMENTO_849_0923 <- REBATE_LISTAGEM_849_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=REBATE_OBS_849_0923) 


View(REBATE_PAGAMENTO_849_0923)


## SQL 157 DOMBOSCO 31.12.2023 ===============================================================================

REBATE_RESULT_157_0923 <- query_0923 %>%  filter(CLICODIGO==157)

View(REBATE_RESULT_157_0923)

REBATE_RESULT_157_0923 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_157_0923 <- range_read("1Oxon4HQST-MqCsQ4bH4rRQHBP-qV-UUGHxeAtrJvH7A",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_157_0923 <- REBATE_CONTRATO_157_0923 %>% 
  mutate(A=REBATE_RESULT_157_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_157_0923 <- REBATE_RESULT_157_0923 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_157_0923)*VRVENDA)


## OBS

REBATE_OBS_157_0923 <- paste0("DOM BOSCO 157 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_157_0923 <- inner_join(REBATE_BONIFICACAO_157_0923,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_157_0923)

View(REBATE_LISTAGEM_157_0923)

REBATE_LISTAGEM_157_0923 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_157_0923 <- REBATE_LISTAGEM_157_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_157_0923) 

View(REBATE_PAGAMENTO_157_0923)



## SQL G78 LOCH 01.04.23 - 30.06.23 ===============================================================================


REBATE_RESULT_G78_0923 <- query_0923 %>%  filter(GCLCODIGO==78)

View(REBATE_RESULT_G78_0923)

REBATE_RESULT_G78_0923 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_G78_0923 <- range_read("1yBGqSlRgycaNZWajKNyhh19BVGdvE6MeUpozbOtLCrQ",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_G78_0923 <- REBATE_CONTRATO_G78_0923 %>% 
  mutate(A=REBATE_RESULT_G78_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G78_0923 <- REBATE_RESULT_G78_0923 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G78_0923)*VRVENDA)


## OBS

REBATE_OBS_G78_0923 <- paste0("LOCH G78 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_G78_0923 <- inner_join(REBATE_BONIFICACAO_G78_0923,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_G78_0923)

View(REBATE_LISTAGEM_G78_0923)

REBATE_LISTAGEM_G78_0923 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_G78_0923 <- REBATE_LISTAGEM_G78_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_G78_0923) 


View(REBATE_PAGAMENTO_G78_0923)


## SQL G91 EDUARDO 01.10.2022 - 30.06.2023 ===============================================================================


REBATE_RESULT_G91_0923 <- query_0923 %>%  filter(GCLCODIGO==91)

View(REBATE_RESULT_G91_0923)

REBATE_RESULT_G91_0923 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_G91_0923 <- range_read("1P3379yxWc45IxGttmsGWK3uDM-Zc-BeVp2BQvsynbRg",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_G91_0923 <- REBATE_CONTRATO_G91_0923 %>% 
  mutate(A=REBATE_RESULT_G91_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G91_0923 <- REBATE_RESULT_G91_0923 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G91_0923)*VRVENDA)


## OBS

REBATE_OBS_G91_0923 <- paste0("EDUARDO G91 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_G91_0923 <- inner_join(REBATE_BONIFICACAO_G91_0923,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_G91_0923)

View(REBATE_LISTAGEM_G91_0923)

REBATE_LISTAGEM_G91_0923 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_G91_0923 <- REBATE_LISTAGEM_G91_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_G91_0923) 


View(REBATE_PAGAMENTO_G91_0923)



## SQL 1830  PRECIOSA 01.03.2023 - 31.07.2023 ===============================================================================


REBATE_RESULT_1830_0923 <- query_0923 %>%  filter(CLICODIGO==1830)

View(REBATE_RESULT_1830_0923)

REBATE_RESULT_1830_0923 %>% summarize(v=sum(VRVENDA))


## GET CONTRACT

REBATE_CONTRATO_1830_0923 <- range_read("1CtNoqyvBhhK1s2OqNbbvWFPxPO4GNNtFnFYpvE_Zm50",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_1830_0923 <- REBATE_CONTRATO_1830_0923 %>% 
  mutate(A=REBATE_RESULT_1830_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_1830_0923 <- REBATE_RESULT_1830_0923 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_1830_0923)*VRVENDA)


## OBS

REBATE_OBS_1830_0923 <- paste0("PRECIOSA 1830 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_1830_0923 <- inner_join(REBATE_BONIFICACAO_1830_0923,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_1830_0923)

View(REBATE_LISTAGEM_1830_0923)

REBATE_LISTAGEM_1830_0923 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_1830_0923 <- REBATE_LISTAGEM_1830_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_1830_0923) 


View(REBATE_PAGAMENTO_1830_0923)



## SQL 4253 BLUE EYE 01.03.2023 - 30.06.2023 ===============================================================================


REBATE_RESULT_4253_0923 <- query_0923 %>%  filter(CLICODIGO==4253)

View(REBATE_RESULT_4253_0923)

REBATE_RESULT_4253_0923 %>% summarize(v=sum(VRVENDA))

REBATE_RESULT_4253_0923 %>% summarize(v=sum(VRVENDA)*0.04)

## GET CONTRACT

REBATE_CONTRATO_4253_0923 <- range_read("1fGH1u27mr5lumG3zi_OxnMXNmA5qIVo6g7fnImQnKa8",sheet = "PARAM")


## SET RULES

REBATE_PERCENTUAL_4253_0923 <- REBATE_CONTRATO_4253_0923 %>% 
  mutate(A=REBATE_RESULT_4253_0923 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4253_0923 <- REBATE_RESULT_4253_0923 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4253_0923)*VRVENDA)


## OBS

REBATE_OBS_4253_0923 <- paste0("BLUE EYE 4253 ","REBATE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

REBATE_LISTAGEM_4253_0923 <- inner_join(REBATE_BONIFICACAO_4253_0923,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4253_0923)

View(REBATE_LISTAGEM_4253_0923)

REBATE_LISTAGEM_4253_0923 %>% summarize(v=sum(BONUS))


## PAGAMENTOS 

REBATE_PAGAMENTO_4253_0923 <- REBATE_LISTAGEM_4253_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4253_0923) 


View(REBATE_PAGAMENTO_4253_0923)


## PAGAMENTOS  =========================================================================


REBATE_PAGAMENTO_0923 <- rbind(REBATE_PAGAMENTO_G139_0923,
                               REBATE_PAGAMENTO_849_0923,
                               REBATE_PAGAMENTO_157_0923,
                               REBATE_PAGAMENTO_G78_0923,
                               REBATE_PAGAMENTO_G91_0923,
                               REBATE_PAGAMENTO_1830_0923,
                               REBATE_PAGAMENTO_4253_0923
) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="REBATE")

View(REBATE_PAGAMENTO_0923)


range_write(REBATE_PAGAMENTO_0923,ss="1vnn7_8_PwZwvG8AeTg6Oq1rJosDnl0zNM_yQ6oCweM4",range = "A1",sheet="RESUMO",reformat = FALSE)  


## LISTAGEM  =========================================================================


REBATE_LISTAGEM_0923 <- rbind(REBATE_LISTAGEM_G139_0923,
                              REBATE_LISTAGEM_849_0923,
                              REBATE_LISTAGEM_157_0923,
                              REBATE_LISTAGEM_G78_0923,
                              REBATE_LISTAGEM_G91_0923,
                              REBATE_LISTAGEM_1830_0923,
                              REBATE_LISTAGEM_4253_0923
)

View(REBATE_LISTAGEM_0923)


range_write(REBATE_LISTAGEM_0923,ss="1vnn7_8_PwZwvG8AeTg6Oq1rJosDnl0zNM_yQ6oCweM4",range = "A1",sheet="REBATES",reformat = FALSE)  


## CREDITO  =========================================================================


## CREDITO CARTOES


CREDITO_CARTOES_REBATES_0923 <- left_join(REBATE_PAGAMENTO_0923 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0823 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 

View(CREDITO_CARTOES_REBATES_0923)


## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_0923_2 <- CREDITO_CARTOES_REBATES_0923 %>% 
  filter(!is.na(NSERIE)) %>% filter(!is.na(BONUS)) %>%  filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_REBATES_0923_2)


# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_0923_3 <- CREDITO_CARTOES_REBATES_0923_2  %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_REBATES_0923_3)  

CREDITO_CARTOES_REBATES_0923_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_REBATES_0923_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_REBATES_0923_3,
           file = "C:\\Users\\Repro\\One Drive Comunicacao\\OneDrive - Luxottica Group S.p.A\\CAMPANHAS ALELO\\2023\\SET\\CREDITO_CARTOES_REBATES_0923.csv",
           row.names=FALSE,quote = FALSE)


## EMISSAO CARTOES  =========================================================================


EMISSAO_CARTOES_REBATES_0923 <- CREDITO_CARTOES_REBATES_0923 %>% 
  filter(is.na(NSERIE)) %>% filter(!is.na(BONUS)) %>% filter(PGTO_MINIMO=='S') %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1,2,3,14,15,16)]

View(EMISSAO_CARTOES_REBATES_0923)






