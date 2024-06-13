## CAMPANHAS REBATES

## PERIODO DE REFERENCIA 05.2024

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)

con2 <- dbConnect(odbc::odbc(), "repro", encoding = "latin1")

## CLIENTES ========================================================

clientes_rebates <- dbGetQuery(con2,"
                    WITH CLI AS (SELECT C.CLICODIGO,CLINOMEFANT,C.GCLCODIGO,GCLNOME,SETOR
                    FROM CLIEN C
                    LEFT JOIN GRUPOCLI GC ON GC.GCLCODIGO=C.GCLCODIGO
                    LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI E
                    LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA)Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') 
                    ED ON C.CLICODIGO=ED.CLICODIGO)
           
                    SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,GCLNOME,SETOR FROM CLIPROMO C
                    LEFT JOIN CLI CL ON C.CLICODIGO=CL.CLICODIGO
                    WHERE ID_PROMO=46") 

View(clientes_rebates )

## SQL ==============================================================

query_0524 <- dbGetQuery(con2,"WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR,ENDCODIGO
    FROM CLIEN C
     INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=46)CLP ON C.CLICODIGO=CLP.CLICODIGO
      LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO  FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
        WHERE CLICLIENTE='S'),
                               
                               
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

REBATE_RESULT_G139_0524 <-
  query_0524 %>% 
  as.data.frame() %>% 
  mutate(CPF = gsub("[^0-9]", "", PEDAUTORIZOU)) %>% 
  filter(GCLCODIGO==139)


## GET CONTRACT

REBATE_CONTRATO_G139_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_G139.xlsx")


## SET RULES

REBATE_PERCENTUAL_G139_0524 <- REBATE_CONTRATO_G139_0524 %>% 
  mutate(A=REBATE_RESULT_G139_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_G139_0524 <- REBATE_RESULT_G139_0524  %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_G139_0524)*VRVENDA)


## OBS

REBATE_OBS_G139_0524 <- paste0(clientes_rebates %>% filter(GCLCODIGO==139) %>% select(GCLNOME) %>% distinct()," G139"," - REBATE ","REBATE ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))

## LISTA PEDIDOS E ADICIONA CPF

LIST_REBATES_G139_0524 <- REBATE_BONIFICACAO_G139_0524 %>% 
  mutate(OBS = REBATE_OBS_G139_0524) %>%
  filter(str_detect(CPF, "\\d+"))


## PAGAMENTOS 

REBATE_PAGAMENTO_VENDEDORAS_G139_0524 <- LIST_REBATES_G139_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% as.data.frame() %>% 
  mutate(OBS=REBATE_OBS_G139_0524) 


## montador

REBATE_PAGAMENTO_MONTADOR_G139_0524 <- 
  data.frame(CPF=c("88717020930"),
             BONUS=(REBATE_RESULT_G139_0524 %>% 
                      summarize(BONUS=round(sum(VRVENDA)*0.03,0))),OBS=REBATE_OBS_G139_0524) 


REBATE_PAGAMENTO_MONTADOR_G139_0524_2 <- 
  REBATE_PAGAMENTO_MONTADOR_G139_0524 %>% as.data.frame() %>%  mutate(BONUS=BONUS)



## TOTAL
PAG_REBATES_G139_0524 <-
  rbind(REBATE_PAGAMENTO_VENDEDORAS_G139_0524,REBATE_PAGAMENTO_MONTADOR_G139_0524_2)



## SQL 849 VITAL  ===============================================================================


REBATE_RESULT_849_0524 <- 
  query_0524 %>% filter(CLICODIGO==849) 



## GET CONTRACT

REBATE_CONTRATO_849_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_849.xlsx")


## SET RULES

REBATE_PERCENTUAL_849_0524 <- REBATE_CONTRATO_849_0524 %>% 
  mutate(A=REBATE_RESULT_849_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_849_0524 <- REBATE_RESULT_849_0524 %>%  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_849_0524)*VRVENDA)


## OBS

REBATE_OBS_849_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==849) %>% select(CLINOMEFANT) ," 849"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

LIST_REBATES_849_0524 <- REBATE_BONIFICACAO_849_0524 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(REBATE_RESULT_849_0524))) %>% 
  mutate(OBS=REBATE_OBS_849_0524)



## PAGAMENTOS 

PAG_REBATES_849_0524 <- LIST_REBATES_849_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=REBATE_OBS_849_0524) 


## SQL 157 DOMBOSCO ===============================================================================


REBATE_RESULT_157_0524 <- query_0524 %>%  filter(CLICODIGO==157)


## GET CONTRACT

REBATE_CONTRATO_157_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_157.xlsx")


## SET RULES

REBATE_PERCENTUAL_157_0524 <- REBATE_CONTRATO_157_0524 %>% 
  mutate(A=REBATE_RESULT_157_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_157_0524 <- REBATE_RESULT_157_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_157_0524)*VRVENDA)


## OBS

REBATE_OBS_157_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==157) %>% select(CLINOMEFANT)," 157"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_157_0524 <- inner_join(REBATE_BONIFICACAO_157_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_157_0524)




## PAGAMENTOS 

PAG_REBATES_157_0524 <- LIST_REBATES_157_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_157_0524) 



## SQL 1830  PRECIOSA  ===============================================================================


REBATE_RESULT_1830_0524 <- query_0524 %>%  filter(CLICODIGO==1830)



## GET CONTRACT

REBATE_CONTRATO_1830_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_1830.xlsx")


## SET RULES

REBATE_PERCENTUAL_1830_0524 <- REBATE_CONTRATO_1830_0524 %>% 
  mutate(A=REBATE_RESULT_1830_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_1830_0524 <- REBATE_RESULT_1830_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_1830_0524)*VRVENDA)


## OBS

REBATE_OBS_1830_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==1830) %>% select(CLINOMEFANT)," 1830"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS E ADICIONA CPF

LIST_REBATES_1830_0524 <- inner_join(REBATE_BONIFICACAO_1830_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_1830_0524)


## PAGAMENTOS 

PAG_REBATES_1830_0524 <- LIST_REBATES_1830_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_1830_0524) 



## SQL 4253 BLUE EYE  ===============================================================================


REBATE_RESULT_4253_0524 <- query_0524 %>%  filter(CLICODIGO==4253)


## GET CONTRACT

REBATE_CONTRATO_4253_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_4253.xlsx")


## SET RULES

REBATE_PERCENTUAL_4253_0524 <- REBATE_CONTRATO_4253_0524 %>% 
  mutate(A=REBATE_RESULT_4253_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4253_0524 <- REBATE_RESULT_4253_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4253_0524)*VRVENDA)


## OBS

REBATE_OBS_4253_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==4253) %>% select(CLINOMEFANT)," 4253"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_4253_0524 <- inner_join(REBATE_BONIFICACAO_4253_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4253_0524)



## PAGAMENTOS 

PAG_REBATES_4253_0524 <- LIST_REBATES_4253_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4253_0524) 


## SQL 213 LINO   ===============================================================================


REBATE_RESULT_213_0524 <- query_0524 %>%  filter(CLICODIGO==213)

## GET CONTRACT

REBATE_CONTRATO_213_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_213.xlsx")

## SET RULES

REBATE_PERCENTUAL_213_0524 <- REBATE_CONTRATO_213_0524 %>% 
  mutate(A=REBATE_RESULT_213_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 


REBATE_BONIFICACAO_213_0524 <- REBATE_RESULT_213_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_213_0524)*VRVENDA)


## OBS

REBATE_OBS_213_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==213) %>% select(CLINOMEFANT)," 213"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

LIST_REBATES_213_0524 <- REBATE_BONIFICACAO_213_0524 %>% cross_join(.,BASE_CPF %>% filter(CLICODIGO==213) %>% select(CPF)) %>% mutate(OBS=REBATE_OBS_213_0524)


## PAGAMENTOS 

PAG_REBATES_213_0524 <- LIST_REBATES_213_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=ceiling(sum(BONUS)/as.numeric(count(BASE_CPF %>% filter(CLICODIGO==213))))) %>% 
  mutate(OBS=REBATE_OBS_213_0524) %>% as.data.frame()

## SQL 291 OTICA LUZ ===============================================================================


REBATE_RESULT_291_0524 <- query_0524 %>%  filter(CLICODIGO==291)


## GET CONTRACT

REBATE_CONTRATO_291_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_291.xlsx")


## SET RULES

REBATE_PERCENTUAL_291_0524 <- REBATE_CONTRATO_291_0524 %>% 
  mutate(A=REBATE_RESULT_291_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_291_0524 <- REBATE_RESULT_291_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_291_0524)*VRVENDA)


## OBS

REBATE_OBS_291_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==291) %>% select(CLINOMEFANT)," 291"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_291_0524 <- inner_join(REBATE_BONIFICACAO_291_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_291_0524)



## PAGAMENTOS 

PAG_REBATES_291_0524 <- LIST_REBATES_291_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_291_0524) 


## SQL 436 OTICA ESPECIALISTA ===============================================================================


REBATE_RESULT_436_0524 <- query_0524 %>%  filter(CLICODIGO==436)


## GET CONTRACT

REBATE_CONTRATO_436_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_436.xlsx")


## SET RULES

REBATE_PERCENTUAL_436_0524 <- REBATE_CONTRATO_436_0524 %>% 
  mutate(A=REBATE_RESULT_436_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_436_0524 <- REBATE_RESULT_436_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_436_0524)*VRVENDA)


## OBS

REBATE_OBS_436_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==436) %>% select(CLINOMEFANT)," 436"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_436_0524 <- inner_join(REBATE_BONIFICACAO_436_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_436_0524)



## PAGAMENTOS 

PAG_REBATES_436_0524 <- LIST_REBATES_436_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_436_0524) 

## SQL 4752 OTICA FRANCA ===============================================================================


REBATE_RESULT_4752_0524 <- query_0524 %>%  filter(CLICODIGO==4752)


## GET CONTRACT

REBATE_CONTRATO_4752_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_4752.xlsx")


## SET RULES

REBATE_PERCENTUAL_4752_0524 <- REBATE_CONTRATO_4752_0524 %>% 
  mutate(A=REBATE_RESULT_4752_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4752_0524 <- REBATE_RESULT_4752_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4752_0524)*VRVENDA)


## OBS

REBATE_OBS_4752_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==4752) %>% select(CLINOMEFANT)," 168"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_4752_0524 <- inner_join(REBATE_BONIFICACAO_4752_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4752_0524)



## PAGAMENTOS 

PAG_REBATES_4752_0524 <- LIST_REBATES_4752_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4752_0524) 

## SQL 4072 PRECO POPULAR ===============================================================================


REBATE_RESULT_4072_0524 <- query_0524 %>%  filter(CLICODIGO==4072)


## GET CONTRACT

REBATE_CONTRATO_4072_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_4072.xlsx")


## SET RULES

REBATE_PERCENTUAL_4072_0524 <- REBATE_CONTRATO_4072_0524 %>% 
  mutate(A=REBATE_RESULT_4072_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_4072_0524 <- REBATE_RESULT_4072_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_4072_0524)*VRVENDA)


## OBS

REBATE_OBS_4072_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==4072) %>% select(CLINOMEFANT)," 4072"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_4072_0524 <- inner_join(REBATE_BONIFICACAO_4072_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_4072_0524)



## PAGAMENTOS 

PAG_REBATES_4072_0524 <- LIST_REBATES_4072_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_4072_0524) 

## SQL 3801 VISIONE ===============================================================================


REBATE_RESULT_3801_0524 <- query_0524 %>%  filter(CLICODIGO==3801)


## GET CONTRACT

REBATE_CONTRATO_3801_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_3801.xlsx")


## SET RULES

REBATE_PERCENTUAL_3801_0524 <- REBATE_CONTRATO_3801_0524 %>% 
  mutate(A=REBATE_RESULT_3801_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_3801_0524 <- REBATE_RESULT_3801_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_3801_0524)*VRVENDA)


## OBS

REBATE_OBS_3801_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==3801) %>% select(CLINOMEFANT)," 3801"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_3801_0524 <- inner_join(REBATE_BONIFICACAO_3801_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_3801_0524)



## PAGAMENTOS 

PAG_REBATES_3801_0524 <- LIST_REBATES_3801_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_3801_0524)

## SQL 198 KREICHE & CIA ===============================================================================


REBATE_RESULT_198_0524 <- query_0524 %>%  filter(CLICODIGO==198)


## GET CONTRACT

REBATE_CONTRATO_198_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_198.xlsx")


## SET RULES

REBATE_PERCENTUAL_198_0524 <- REBATE_CONTRATO_198_0524 %>% 
  mutate(A=REBATE_RESULT_198_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_198_0524 <- REBATE_RESULT_198_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_198_0524)*VRVENDA)


## OBS

REBATE_OBS_198_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==198) %>% select(CLINOMEFANT)," 198"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_198_0524 <- inner_join(REBATE_BONIFICACAO_198_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_198_0524)



## PAGAMENTOS 

PAG_REBATES_198_0524 <- LIST_REBATES_198_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_198_0524)

## SQL 168 KREICHE & CIA ===============================================================================


REBATE_RESULT_168_0524 <- query_0524 %>%  filter(CLICODIGO==168)


## GET CONTRACT

REBATE_CONTRATO_168_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_168.xlsx")


## SET RULES

REBATE_PERCENTUAL_168_0524 <- REBATE_CONTRATO_168_0524 %>% 
  mutate(A=REBATE_RESULT_168_0524 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM)  %>% filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

REBATE_BONIFICACAO_168_0524 <- REBATE_RESULT_168_0524 %>%  
  mutate(BONUS=as.numeric(REBATE_PERCENTUAL_168_0524)*VRVENDA)


## OBS

REBATE_OBS_168_0524 <- paste0(clientes_rebates %>% filter(CLICODIGO==168) %>% select(CLINOMEFANT)," 168"," - REBATE "," ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y"))


## LISTA PEDIDOS

## JOIN CPF

LIST_REBATES_168_0524 <- inner_join(REBATE_BONIFICACAO_168_0524,BASE_CPF,by="CLICODIGO") %>%
  mutate(OBS=REBATE_OBS_168_0524)



## PAGAMENTOS 

PAG_REBATES_168_0524 <- LIST_REBATES_168_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=REBATE_OBS_168_0524)

## PAGAMENTOS  =========================================================================


PAG_REBATES_0524 <- rbind(PAG_REBATES_G139_0524,
                          PAG_REBATES_849_0524,
                          PAG_REBATES_157_0524,
                          PAG_REBATES_1830_0524,
                          PAG_REBATES_4253_0524,
                          PAG_REBATES_213_0524,
                          PAG_REBATES_291_0524,
                          PAG_REBATES_436_0524,
                          PAG_REBATES_4752_0524,
                          PAG_REBATES_4072_0524,
                          PAG_REBATES_3801_0524,
                          PAG_REBATES_198_0524,
                          PAG_REBATES_168_0524
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="REBATE")

View(PAG_REBATES_0524)


## LISTAGEM  =========================================================================


LIST_REBATES_0524 <- rbind(LIST_REBATES_G139_0524,
                           LIST_REBATES_849_0524,
                           LIST_REBATES_157_0524,
                           LIST_REBATES_1830_0524,
                           LIST_REBATES_4253_0524,
                           LIST_REBATES_213_0524,
                           LIST_REBATES_291_0524,
                           LIST_REBATES_436_0524,
                           LIST_REBATES_4752_0524
)


View(LIST_REBATES_0524)


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

CREDITO_CARTOES_REBATES_0524 <- left_join(PAG_REBATES_0524 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_ALELO %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_0524_2 <- CREDITO_CARTOES_REBATES_0524 %>% 
  filter(!is.na(NSERIE)) %>% filter(!is.na(BONUS))


# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_0524_3 <- CREDITO_CARTOES_REBATES_0524_2  %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))


# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_REBATES_0524.csv")

dir_path <- dirname(file_path)
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_REBATES_0524_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)

## EMISSAO CARTOES  =========================================================================


EMISSAO_CARTOES_REBATES_0524 <- CREDITO_CARTOES_REBATES_0524 %>% 
  filter(is.na(NSERIE)) %>% filter(!is.na(BONUS)) %>% filter(PGTO_MINIMO=='S') %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1,2,3,14,15,16)]







