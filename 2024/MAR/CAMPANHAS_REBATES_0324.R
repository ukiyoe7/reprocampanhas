## CAMPANHAS REBATES

## PERIODO DE REFERENCIA 03.2024

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)

con2 <- dbConnect(odbc::odbc(), "repro", encoding = "latin1")

## SQL ==============================================================

query_0324 <- dbGetQuery(con2,"WITH CLI AS (SELECT DISTINCT C.CLICODIGO,
                              CLINOMEFANT,
                               ENDCODIGO,
                                GCLCODIGO,
                                 SETOR
                                  FROM CLIEN C
                                   LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                    LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA 
                                     WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                      E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                       WHERE CLICLIENTE='S' AND (C.CLICODIGO IN (849,157,1830,4253,213) OR GCLCODIGO=139)),
                               
                               
   FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
   
    
    PED AS (SELECT ID_PEDIDO,
                    EMPCODIGO,
                    TPCODIGO,
                     PEDDTBAIXA,
                      PEDAUTORIZOU,
                       P.CLICODIGO,
                        GCLCODIGO,
                         SETOR,
                           CLINOMEFANT,
                            PEDORIGEM
                            FROM PEDID P
                              INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                               WHERE PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)  AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N') ),
                               
      PROD AS (SELECT PROCODIGO,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2)CHAVE,PROTIPO FROM PRODU)

    
      SELECT PD.ID_PEDIDO,
               PEDDTBAIXA,
                CLICODIGO,
                 GCLCODIGO,
                   SETOR,
                    PD.PROCODIGO,
                     PDPDESCRICAO,
                      PEDAUTORIZOU,
                             SUM(PDPQTDADE)QTD,
                              SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
                                FROM PDPRD PD
                                 INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                                  INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
                                   LEFT JOIN PROD PR ON PD.PROCODIGO=PR.PROCODIGO
                                    GROUP BY 1,2,3,4,5,6,7,8 ORDER BY ID_PEDIDO DESC")


## GET CPF ==========================================================================

BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


## G139 SCHROEDER  ===============================================================================

# Rebate 7% para vendedoras e 3% para montador

REBATE_RESULT_G139_0324 <-
  query_0324 %>% 
  as.data.frame() %>% 
  mutate(CPF = gsub("[^0-9]", "", PEDAUTORIZOU)) %>% 
  filter(GCLCODIGO==139)


## GET CONTRACT

REBATE_CONTRATO_G139_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_G139.xlsx")


## SET RULES

REBATE_PERCENTUAL_G139_0324 <- REBATE_CONTRATO_G139_0324 %>% 
  mutate(A=REBATE_RESULT_G139_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G139_0324 <- REBATE_RESULT_G139_0324  %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G139_0324)*VRVENDA)


## OBS

REBATE_OBS_G139_0324 <- paste0("SCHROEDER G139 ","REBATE ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))

## LISTA PEDIDOS E ADICIONA CPF

LIST_REBATES_G139_0324 <- REBATE_BONIFICACAO_G139_0324 %>% 
  mutate(OBS = REBATE_OBS_G139_0324) %>%
  filter(str_detect(CPF, "\\d+"))


## PAGAMENTOS 

REBATE_PAGAMENTO_VENDEDORAS_G139_0324 <- LIST_REBATES_G139_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% as.data.frame() %>% 
  mutate(OBS=REBATE_OBS_G139_0324) 


## montador

REBATE_PAGAMENTO_MONTADOR_G139_0324 <- 
  data.frame(CPF=c("88717020930"),
             BONUS=(REBATE_RESULT_G139_0324 %>% 
                      summarize(BONUS=round(sum(VRVENDA)*0.03,0))),OBS=REBATE_OBS_G139_0324) 


REBATE_PAGAMENTO_MONTADOR_G139_0324_2 <- 
  REBATE_PAGAMENTO_MONTADOR_G139_0324 %>% as.data.frame() %>%  mutate(BONUS=BONUS)



## TOTAL
PAG_REBATES_G139_0324 <-
  rbind(REBATE_PAGAMENTO_VENDEDORAS_G139_0324,REBATE_PAGAMENTO_MONTADOR_G139_0324_2)



## SQL 849 VITAL  ===============================================================================


REBATE_RESULT_849_0324 <- query_0324 %>%  filter(CLICODIGO==849)


## GET CONTRACT

REBATE_CONTRATO_849_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_G139.xlsx")


## SET RULES

REBATE_PERCENTUAL_849_0324 <- REBATE_CONTRATO_849_0324 %>% 
  mutate(A=REBATE_RESULT_849_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_849_0324 <- REBATE_RESULT_849_0324 %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_849_0324)*VRVENDA)


## OBS

REBATE_OBS_849_0324 <- paste0("VITAL 849 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

LIST_REBATES_849_0324 <- REBATE_BONIFICACAO_849_0324 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(REBATE_RESULT_849_0324))) %>% 
  mutate(OBS=REBATE_OBS_849_0324)



## PAGAMENTOS 

PAG_REBATES_849_0324 <- LIST_REBATES_849_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=REBATE_OBS_849_0324) 


## SQL 157 DOMBOSCO ===============================================================================


REBATE_RESULT_157_0324 <- query_0324 %>%  filter(CLICODIGO==157)


## GET CONTRACT

REBATE_CONTRATO_157_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_G139.xlsx")


## SET RULES

REBATE_PERCENTUAL_157_0324 <- REBATE_CONTRATO_157_0324 %>% 
  mutate(A=REBATE_RESULT_157_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_157_0324 <- REBATE_RESULT_157_0324 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_157_0324)*VRVENDA)


## OBS

REBATE_OBS_157_0324 <- paste0("DOM BOSCO 157 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_157_0324 <- inner_join(REBATE_BONIFICACAO_157_0324,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_157_0324)




## PAGAMENTOS 

PAG_REBATES_157_0324 <- LIST_REBATES_157_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_157_0324) 



## SQL 1830  PRECIOSA  ===============================================================================


REBATE_RESULT_1830_0324 <- query_0324 %>%  filter(CLICODIGO==1830)



## GET CONTRACT

REBATE_CONTRATO_1830_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_1830.xlsx")


## SET RULES

REBATE_PERCENTUAL_1830_0324 <- REBATE_CONTRATO_1830_0324 %>% 
  mutate(A=REBATE_RESULT_1830_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_1830_0324 <- REBATE_RESULT_1830_0324 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_1830_0324)*VRVENDA)


## OBS

REBATE_OBS_1830_0324 <- paste0("PRECIOSA 1830 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS E ADICIONA CPF

LIST_REBATES_1830_0324 <- inner_join(REBATE_BONIFICACAO_1830_0324,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_1830_0324)


## PAGAMENTOS 

PAG_REBATES_1830_0324 <- LIST_REBATES_1830_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_1830_0324) 



## SQL 4253 BLUE EYE  ===============================================================================


REBATE_RESULT_4253_0324 <- query_0324 %>%  filter(CLICODIGO==4253)


## GET CONTRACT

REBATE_CONTRATO_4253_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_4253.xlsx")


## SET RULES

REBATE_PERCENTUAL_4253_0324 <- REBATE_CONTRATO_4253_0324 %>% 
  mutate(A=REBATE_RESULT_4253_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4253_0324 <- REBATE_RESULT_4253_0324 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4253_0324)*VRVENDA)


## OBS

REBATE_OBS_4253_0324 <- paste0("BLUE EYE 4253 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_4253_0324 <- inner_join(REBATE_BONIFICACAO_4253_0324,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4253_0324)



## PAGAMENTOS 

PAG_REBATES_4253_0324 <- LIST_REBATES_4253_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4253_0324) 


## SQL 213 LINO   ===============================================================================


REBATE_RESULT_213_0324 <- query_0324 %>%  filter(CLICODIGO==213)

## GET CONTRACT

REBATE_CONTRATO_213_0324 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_213.xlsx")

## SET RULES

REBATE_PERCENTUAL_213_0324 <- REBATE_CONTRATO_213_0324 %>% 
  mutate(A=REBATE_RESULT_213_0324 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 


REBATE_BONIFICACAO_213_0324 <- REBATE_RESULT_213_0324 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_213_0324)*VRVENDA)


## OBS

REBATE_OBS_213_0324 <- paste0("LINO 213 ","REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

LIST_REBATES_213_0324 <- REBATE_BONIFICACAO_213_0324 %>% cross_join(.,BASE_CPF %>% filter(CLICODIGO==213) %>% select(CPF)) %>% mutate(OBS=REBATE_OBS_213_0324)


## PAGAMENTOS 

PAG_REBATES_213_0324 <- LIST_REBATES_213_0324 %>% group_by(CPF) %>% 
  summarize(BONUS=ceiling(sum(BONUS)/as.numeric(count(BASE_CPF %>% filter(CLICODIGO==213))))) %>% 
  mutate(OBS=REBATE_OBS_213_0324) %>% as.data.frame()


## PAGAMENTOS  =========================================================================


PAG_REBATES_0324 <- rbind(PAG_REBATES_G139_0324,
                          PAG_REBATES_849_0324,
                          PAG_REBATES_157_0324,
                          PAG_REBATES_1830_0324,
                          PAG_REBATES_4253_0324,
                          PAG_REBATES_213_0324
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="REBATE")

View(PAG_REBATES_0324)


## LISTAGEM  =========================================================================


LIST_REBATES_0324 <- rbind(LIST_REBATES_G139_0324,
                           LIST_REBATES_849_0324,
                           LIST_REBATES_157_0324,
                           LIST_REBATES_1830_0324,
                           LIST_REBATES_4253_0324,
                           LIST_REBATES_213_0324)


View(LIST_REBATES_0324)


## CREDITO  =========================================================================


## CREDITO CARTOES

CARTOES_ALELO <- 
   read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
    mutate(CPF=sub("\\D+", '', CPF))  %>% 
     mutate(CPF=sub("\\.", '',CPF)) %>% 
      mutate(CPF=sub("\\-", '',CPF)) %>% 
       mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)

CREDITO_CARTOES_REBATES_0324 <- left_join(PAG_REBATES_0324 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_ALELO %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_0324_2 <- CREDITO_CARTOES_REBATES_0324 %>% 
  filter(!is.na(NSERIE)) %>% filter(!is.na(BONUS))


# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_0324_3 <- CREDITO_CARTOES_REBATES_0324_2  %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))


# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_REBATES_0324.csv")

dir_path <- dirname(file_path)
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_REBATES_0324_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)

## EMISSAO CARTOES  =========================================================================


EMISSAO_CARTOES_REBATES_0324 <- CREDITO_CARTOES_REBATES_0324 %>% 
  filter(is.na(NSERIE)) %>% filter(!is.na(BONUS)) %>% filter(PGTO_MINIMO=='S') %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1,2,3,14,15,16)]







