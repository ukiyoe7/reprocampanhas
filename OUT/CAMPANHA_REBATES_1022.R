
## PERIODO DE REFERENCIA 1022
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

CP_G139_1022 <-dbGetQuery(con2,"
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

View(CP_G139_1022)
CP_G139_1022 %>%  summarize(v=sum(VRVENDA)) # total sales
CP_G139_1022 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
CP_G139_1022 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(CP_G139_1022 %>%  summarize(v=sum(VRVENDA)*0.07),CP_G139_1022 %>% summarize(v=sum(VRVENDA)*0.03)) #total

## OBS

OBS_G139 <- paste0("SCHROEDER ","G",CP_G139_1022 %>% 
                     distinct(GCLCODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


### Cria Planilha identificação CPFs

CP_G139_1022_IPEDIDOS <- CP_G139_1022 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>% 
  mutate(DATA=format(DATA,"%d/%m/%y")) 

range_write("1keXCWCZ983aEtWyZQBvSsNrbQEn2szCxZnFuDwfiw7k",data=CP_G139_1022_IPEDIDOS,sheet = "OUT22",
            range = "A1") 


View(CP_G139_1022_IPEDIDOS)

## obtem dados planilha identificação CPFs


CPF_G139_CPF_1022 <- read_sheet("1keXCWCZ983aEtWyZQBvSsNrbQEn2szCxZnFuDwfiw7k",sheet = "OUT22") %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CPF_G139_CPF_1022)


#lista pedidos

LIST_G139_1022 <- inner_join(CP_G139_1022,CPF_G139_CPF_1022,by="ID_PEDIDO") %>% 
  mutate(OBS=OBS_G139) %>% 
  .[,c(-12,-13,-14)]

View(LIST_G139_1022)

LIST_G139_1022 %>% summarize(BONUS=sum(BONUS)) 

### pagamentos


# Calculo vendedoras

PAG_G139_1022_VENDEDORAS <- LIST_G139_1022 %>% group_by(CPF) %>% summarize(BONUS=sum(BONUS)) 

#calculo com montador

PAG_G139_1022_MONTADOR <- data.frame(CPF=c("88717020930"),BONUS=(CP_G139_1022 %>% 
                                                                   summarize(BONUS=sum(VRVENDA)*0.03))) 


PAG_G139_1022 <- rbind(PAG_G139_1022_VENDEDORAS,PAG_G139_1022_MONTADOR) %>%  mutate(OBS=OBS_G139) %>% 
  mutate(BONUS=round(BONUS,0))

View(PAG_G139_1022)

PAG_G139_1022 %>% summarize(BONUS=sum(BONUS)) 


## ============================================================================================

## SQL 849 VITAL

CP_849_1022 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO, CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE C.CLICODIGO=849),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
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
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.08 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") 

View(CP_849_1022)

CP_849_1022 %>% summarize(v=sum(VRVENDA))

CP_849_1022 %>% summarize(v=sum(VRVENDA)*0.08)

## OBS
OBS_849 <-paste0("VITAL ",CP_849_1022  %>% distinct(CLICODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))



## LISTA PEDIDOS
LIST_849_1022 <- CP_849_1022 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(CP_849_1022))) %>% 
  mutate(OBS=OBS_849)


View(LIST_849_1022)

## PAGAMENTOS 

PAG_849_1022 <- LIST_849_1022 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=OBS_849) 


View(PAG_849_1022)



### ===========================================================================================

## SQL 157 DOMBOSCO

CP_157_1022 <- dbGetQuery(con2,"
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
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.1 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_157_1022)

CP_157_1022 %>% summarize(v=sum(VRVENDA))

CP_157_1022 %>% summarize(v=sum(VRVENDA)*0.1)

## OBS

OBS_157 <- paste0("DOM BOSCO ","G",CP_157_1022  %>% 
                    distinct(CLICODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_157_1022 <- inner_join(CP_157_1022,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_157)

View(LIST_157_1022)

LIST_157_1022 %>% summarize(v=sum(BONUS))


## PAY

PAG_157_1022 <- LIST_157_1022 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_157) 

View(PAG_157_1022)

### ===========================================================================================

## SQL 4253 

CP_4253_1022 <- dbGetQuery(con2,"
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
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.04 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_4253_1022)

CP_4253_1022 %>% summarize(v=sum(VRVENDA))

CP_4253_1022 %>% summarize(v=sum(VRVENDA)*0.04)

## OBS

OBS_4253 <- paste0("BLUE EYE REBATE",CP_4253_1022  %>% 
                    distinct(CLICODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_4253_1022 <- inner_join(CP_4253_1022,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_4253)

View(LIST_4253_1022)

LIST_4253_1022 %>% summarize(v=sum(BONUS))


## PAY

PAG_4253_1022 <- LIST_4253_1022 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_4253) 

View(PAG_4253_1022)



### ===========================================================================================

## SQL OTICA EDUARDO G91

CP_G91_1022 <- dbGetQuery(con2,"
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
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.04 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CP_G91_1022)

CP_G91_1022 %>% summarize(v=sum(VRVENDA))

CP_G91_1022 %>% summarize(v=sum(VRVENDA)*0.00)

## OBS

OBS_G91 <- paste0("BLUE EYE REBATE",CP_G91_1022  %>% 
                     distinct(CLICODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_G91_1022 <- inner_join(CP_G91_1022,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_G91)

View(LIST_G91_1022)

LIST_G91_1022 %>% summarize(v=sum(BONUS))


## PAY

PAG_G91_1022 <- LIST_G91_1022 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_G91) 

View(PAG_G91_1022)