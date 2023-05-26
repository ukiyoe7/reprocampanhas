
## PERIODO DE REFERENCIA 0423
## REBATES
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")


## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## G139 SCHROEDER
# Rebate 7% para vendedoras e 3% para montador

CP_G139_0423 <-dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE GCLCODIGO=139),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,CLINOMEFANT,SETOR,GCLCODIGO,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
 PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
     PROCODIGO,
      PDPDESCRICAO,
       PEDAUTORIZOU,
       SUM(PDPQTDADE) QTD,
       SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
       SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.07 BONUS
       FROM
       PDPRD
       INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
       GROUP BY 1,2,3,4,5,6,7,8") 

View(CP_G139_0423)
CP_G139_0423 %>%  summarize(v=sum(VRVENDA)) # total sales
CP_G139_0423 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
CP_G139_0423 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(CP_G139_0423 %>%  summarize(v=sum(VRVENDA)*0.07),CP_G139_0423 %>% summarize(v=sum(VRVENDA)*0.03)) #total

## OBS

OBS_G139_0423 <- paste0("SCHROEDER G139 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


### Cria Planilha identificação CPFs

CP_G139_0423_IPEDIDOS <- CP_G139_0423 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>%
  mutate(DATA=format(DATA,"%d/%m/%y")) 

range_write("1FNsiDUqecT-R-eyApkzyom-XzSnN1MQ7EZKhqOkEE2Q",data=CP_G139_0423_IPEDIDOS,sheet = "ABR23",
            range = "A1") 


View(CP_G139_0423_IPEDIDOS)

## obtem dados planilha identificação CPFs


CPF_G139_0423_CPF <- read_sheet("1FNsiDUqecT-R-eyApkzyom-XzSnN1MQ7EZKhqOkEE2Q",sheet = "ABR23") %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CPF_G139_0423_CPF)


#lista pedidos

LIST_REBATES_G139_0423 <- inner_join(CP_G139_0423,CPF_G139_0423_CPF,by="ID_PEDIDO") %>% 
  mutate(OBS=OBS_G139_0423) %>% 
  .[,c(-12,-13,-14)]

View(LIST_REBATES_G139_0423)

LIST_REBATES_G139_0423 %>% summarize(BONUS=sum(BONUS)) 

### pagamentos


# Calculo vendedoras

PAG_G139_0423_VENDEDORAS <- LIST_REBATES_G139_0423 %>% group_by(CPF) %>% summarize(BONUS=sum(BONUS)) 

#calculo com montador

PAG_G139_0423_MONTADOR <- data.frame(CPF=c("88717020930"),BONUS=(CP_G139_0423 %>% 
                                                                   summarize(BONUS=sum(VRVENDA)*0.03))) 

#PAGAMENTO
PAG_REBATES_G139_0423 <- rbind(PAG_G139_0423_VENDEDORAS,PAG_G139_0423_MONTADOR) %>%  
  mutate(OBS=OBS_G139_0423) %>% 
  mutate(BONUS=round(BONUS,0))

View(PAG_REBATES_G139_0423)

PAG_REBATES_G139_0423 %>% summarize(BONUS=sum(BONUS)) 


## ============================================================================================

## SQL 849 VITAL

CP_849_0423 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO, CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE C.CLICODIGO=849),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
       FROM PEDID  
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
      PDPRD.ID_PEDIDO,
       PEDDTBAIXA,
        CLICODIGO,
         GCLCODIGO,
          SETOR,
           PROCODIGO,
            PDPDESCRICAO,
             PEDAUTORIZOU,
              SUM(PDPQTDADE) QTD,
               SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") 

View(CP_849_0423)

CP_849_0423 %>% summarize(v=sum(VRVENDA))

CP_849_0423 %>% summarize(v=sum(VRVENDA)*0.06)

## GET RULES
ctc_cp_849 <- range_read("18UYA1GHFm2lADAEm-lSh8c8vYR61M3z1dbXKpZ0Lw-E",sheet = "PARAM")


## SET RULES
ctc_cp_849_bonus <- ctc_cp_849 %>% 
  mutate(A=CP_849_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_849_0423_2 <- CP_849_0423 %>% mutate(BONUS=as.numeric(ctc_cp_849_bonus)*VRVENDA)


## OBS
OBS_849_0423 <-paste0("VITAL 849 ","REBATE "," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))



## LISTA PEDIDOS
LIST_REBATES_849_0423 <- CP_849_0423_2 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(CP_849_0423))) %>% 
  mutate(OBS=OBS_849_0423)


View(LIST_REBATES_849_0423)

## PAGAMENTOS 


PAG_REBATES_849_0423 <- LIST_REBATES_849_0423 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=OBS_849_0423) 


View(PAG_REBATES_849_0423)



### ===========================================================================================

## SQL 157 DOMBOSCO

CP_157_0423 <- dbGetQuery(con2,"
  WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
  
  CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
         LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
         INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
         WHERE C.CLICODIGO=157),
  
  PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
         FROM PEDID 
         INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
         INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
         WHERE
         PEDDTBAIXA
        BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
         AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))
  
  
  SELECT 
        PDPRD.ID_PEDIDO,
         PEDDTBAIXA,
          CLICODIGO,
           GCLCODIGO,
            SETOR,
             PROCODIGO,
              PDPDESCRICAO,
               PEDAUTORIZOU,
                SUM(PDPQTDADE) QTD,
                 SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                  SUM(PDPUNITLIQUIDO*PDPQTDADE)*0 BONUS
                   FROM
                   PDPRD
                   INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                   GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_157_0423)

CP_157_0423 %>% summarize(v=sum(VRVENDA))

## GET RULES
ctc_cp_157 <- range_read("1Oxon4HQST-MqCsQ4bH4rRQHBP-qV-UUGHxeAtrJvH7A",sheet = "PARAM")


## SET RULES
ctc_cp_157_bonus <- ctc_cp_157 %>% 
  mutate(A=CP_157_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_157_0423_2 <- CP_157_0423 %>% mutate(BONUS=as.numeric(ctc_cp_157_bonus)*VRVENDA)


## OBS

OBS_157_0423 <- paste0("DOM BOSCO 157 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_157_0423 <- inner_join(CP_157_0423_2,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_157_0423)

View(LIST_REBATES_157_0423)

LIST_REBATES_157_0423 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_157_0423 <- LIST_REBATES_157_0423 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_157_0423) 

View(PAG_REBATES_157_0423)



### ===========================================================================================

## SQL OTICA EDUARDO G91
## SEM RESULTADO

CP_G91_0423 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE GCLCODIGO=91),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
        BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
      PDPRD.ID_PEDIDO,
       PEDDTBAIXA,
        CLICODIGO,
         GCLCODIGO,
          SETOR,
           PROCODIGO,
            PDPDESCRICAO,
             PEDAUTORIZOU,
              SUM(PDPQTDADE) QTD,
               SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.00 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_G91_0423)

CP_G91_0423 %>% summarize(v=sum(VRVENDA))


## GET RULES
ctc_cp_G91 <- range_read("1P3379yxWc45IxGttmsGWK3uDM-Zc-BeVp2BQvsynbRg",sheet = "PARAM")


## SET RULES
ctc_cp_G91_bonus <- ctc_cp_G91 %>% 
  mutate(A=CP_G91_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_G91_0423_2 <- CP_G91_0423 %>% mutate(BONUS=as.numeric(ctc_cp_G91_bonus)*VRVENDA)


## OBS

OBS_G91_0423 <- paste0("OTICAS EDUARDO REBATE G",CP_G91_0423_2  %>% 
                         distinct(GCLCODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_G91_0423 <- inner_join(CP_G91_0423_2,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_G91_0423)

View(LIST_REBATES_G91_0423)

LIST_REBATES_G91_0423 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_G91_0423 <- LIST_REBATES_G91_0423 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_G91_0423) 

View(PAG_REBATES_G91_0423)

### ===========================================================================================

## SQL OTICA BLUE EYE 4253 REBATE

CP_4253_0423 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE C.CLICODIGO=4253),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
      PDPRD.ID_PEDIDO,
       PEDDTBAIXA,
        CLICODIGO,
         GCLCODIGO,
          SETOR,
           PROCODIGO,
            PDPDESCRICAO,
             PEDAUTORIZOU,
              SUM(PDPQTDADE) QTD,
               SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.00 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_4253_0423)


CP_4253_0423 %>% summarize(v=sum(VRVENDA))


## GET RULES
ctc_cp_4253 <- range_read("1fGH1u27mr5lumG3zi_OxnMXNmA5qIVo6g7fnImQnKa8",sheet = "PARAM")


## SET RULES
ctc_cp_4253_bonus <- ctc_cp_4253 %>% 
  mutate(A=CP_4253_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_4253_0423_2 <- CP_4253_0423 %>% mutate(BONUS=as.numeric(ctc_cp_4253_bonus)*VRVENDA)


## OBS

OBS_4253_0423 <- paste0("BLUE EYE 4253 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_4253_0423 <- inner_join(CP_4253_0423_2,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_4253_0423)

View(LIST_REBATES_4253_0423)

LIST_REBATES_4253_0423 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_4253_0423 <- LIST_REBATES_4253_0423 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_4253_0423) 

View(PAG_REBATES_4253_0423)


### ===========================================================================================

## SQL OTICA BLUE EYE 4253 REBATE

CP_1830_0423 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE C.CLICODIGO=1830),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
      PDPRD.ID_PEDIDO,
       PEDDTBAIXA,
        CLICODIGO,
         GCLCODIGO,
          SETOR,
           PROCODIGO,
            PDPDESCRICAO,
             PEDAUTORIZOU,
              SUM(PDPQTDADE) QTD,
               SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.00 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_1830_0423)


CP_1830_0423 %>% summarize(v=sum(VRVENDA))


## GET RULES
ctc_cp_1830 <- range_read("1CtNoqyvBhhK1s2OqNbbvWFPxPO4GNNtFnFYpvE_Zm50",sheet = "PARAM")


## SET RULES
ctc_cp_1830_bonus <- ctc_cp_1830 %>% 
  mutate(A=CP_1830_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_1830_0423_2 <- CP_1830_0423 %>% mutate(BONUS=as.numeric(ctc_cp_1830_bonus)*VRVENDA)


## OBS

OBS_1830_0423 <- paste0("PRECIOSA 1830 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_1830_0423 <- inner_join(CP_1830_0423_2,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_1830_0423)

View(LIST_REBATES_1830_0423)

LIST_REBATES_1830_0423 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_1830_0423 <- LIST_REBATES_1830_0423 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_1830_0423) 

View(PAG_REBATES_1830_0423)


### ===========================================================================================

## SQL G78 LOCH

CP_G78_0423 <- dbGetQuery(con2,"
  WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
  
  CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
         LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
         INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
         WHERE C.GCLCODIGO=78),
  
  PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
         FROM PEDID 
         INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
         INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
         WHERE
         PEDDTBAIXA
        BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
         AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))
  
  
  SELECT 
        PDPRD.ID_PEDIDO,
         PEDDTBAIXA,
          CLICODIGO,
           GCLCODIGO,
            SETOR,
             PROCODIGO,
              PDPDESCRICAO,
               PEDAUTORIZOU,
                SUM(PDPQTDADE) QTD,
                 SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
                  SUM(PDPUNITLIQUIDO*PDPQTDADE)*0 BONUS
                   FROM
                   PDPRD
                   INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                   GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_G78_0423)

CP_G78_0423 %>% summarize(v=sum(VRVENDA))

## GET RULES
ctc_cp_G78 <- range_read("1yBGqSlRgycaNZWajKNyhh19BVGdvE6MeUpozbOtLCrQ",sheet = "PARAM")


## SET RULES
ctc_cp_G78_bonus <- ctc_cp_G78 %>% 
  mutate(A=CP_G78_0423 %>% summarize(v=sum(VRVENDA))) %>% 
  mutate(B=A-PARAM) %>%  filter(B>0) %>% 
  filter(B==min(B)) %>% .[,c(2)] 

CP_G78_0423_2 <- CP_G78_0423 %>% mutate(BONUS=as.numeric(ctc_cp_G78_bonus)*VRVENDA)


## OBS

OBS_G78_0423 <- paste0("LOCH G78 "," REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_G78_0423 <- inner_join(CP_G78_0423_2,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_G78_0423)

View(LIST_REBATES_G78_0423)

LIST_REBATES_G78_0423 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_G78_0423 <- LIST_REBATES_G78_0423 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_G78_0423) 

View(PAG_REBATES_G78_0423)


## =========================================================================


## PAGAMENTOS


PAG_REBATES_0423 <- rbind(PAG_REBATES_G139_0423,
                          PAG_REBATES_849_0423,
                          PAG_REBATES_157_0423,
                          PAG_REBATES_G91_0423,
                          PAG_REBATES_4253_0423,
                          PAG_REBATES_1830_0423,
                          PAG_REBATES_G78_0423)

View(PAG_REBATES_0423)


range_write(PAG_REBATES_0423,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A1",sheet="RESUMO",reformat = FALSE)  



LIST_REBATES_0423 <- rbind(LIST_REBATES_G139_0423,
                           LIST_REBATES_849_0423,
                           LIST_REBATES_157_0423,
                           LIST_REBATES_G91_0423,
                           LIST_REBATES_4253_0423,
                           LIST_REBATES_1830_0423,
                           LIST_REBATES_G78_0423)

View(LIST_REBATES_0423)


range_write(LIST_REBATES_0423,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A1",sheet="REBATES",reformat = FALSE)  


### ===========================================================================================


## CREDITO CARTOES


CREDITO_CARTOES_REBATES_0423 <- left_join(PAG_REBATES_0423 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0523 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 

View(CREDITO_CARTOES_REBATES_0423)


## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_0423_2 <- CREDITO_CARTOES_REBATES_0423 %>% 
  filter(!is.na(NSERIE)) %>% filter(!is.na(BONUS))

View(CREDITO_CARTOES_REBATES_0423_2)

# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_0423_3 <- CREDITO_CARTOES_REBATES_0423_2  %>% 
  .[,c(4,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_REBATES_0423_3)  

CREDITO_CARTOES_REBATES_0423_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_REBATES_0423_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_REBATES_0423_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\ABR\\CREDITO_CARTOES_REBATES.csv",
           row.names=FALSE,quote = FALSE)


