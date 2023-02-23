
## PERIODO DE REFERENCIA 0123
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

CP_G139_0123 <-dbGetQuery(con2,"
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

View(CP_G139_0123)
CP_G139_0123 %>%  summarize(v=sum(VRVENDA)) # total sales
CP_G139_0123 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
CP_G139_0123 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(CP_G139_0123 %>%  summarize(v=sum(VRVENDA)*0.07),CP_G139_0123 %>% summarize(v=sum(VRVENDA)*0.03)) #total

## OBS

OBS_G139_0123 <- paste0("SCHROEDER G139 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


### Cria Planilha identificação CPFs

CP_G139_0123_IPEDIDOS <- CP_G139_0123 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>%
  mutate(DATA=format(DATA,"%d/%m/%y")) 

range_write("1maoUiDSZkkF3kxvaYv0efTX4G_JEoaWzP45wB23nFJw",data=CP_G139_0123_IPEDIDOS,sheet = "JAN23",
            range = "A1") 


View(CP_G139_0123_IPEDIDOS)

## obtem dados planilha identificação CPFs


CPF_G139_0123_CPF <- read_sheet("1maoUiDSZkkF3kxvaYv0efTX4G_JEoaWzP45wB23nFJw",sheet = "JAN23") %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CPF_G139_0123_CPF)


#lista pedidos

LIST_REBATES_G139_0123 <- inner_join(CP_G139_0123,CPF_G139_0123_CPF,by="ID_PEDIDO") %>% 
  mutate(OBS=OBS_G139_0123) %>% 
  .[,c(-12,-13,-14)]

View(LIST_REBATES_G139_0123)

LIST_REBATES_G139_0123 %>% summarize(BONUS=sum(BONUS)) 

### pagamentos


# Calculo vendedoras

PAG_G139_0123_VENDEDORAS <- LIST_REBATES_G139_0123 %>% group_by(CPF) %>% summarize(BONUS=sum(BONUS)) 

#calculo com montador

PAG_G139_0123_MONTADOR <- data.frame(CPF=c("88717020930"),BONUS=(CP_G139_0123 %>% 
                                                                   summarize(BONUS=sum(VRVENDA)*0.03))) 

#PAGAMENTO
PAG_REBATES_G139_0123 <- rbind(PAG_G139_0123_VENDEDORAS,PAG_G139_0123_MONTADOR) %>%  
  mutate(OBS=OBS_G139_0123) %>% 
  mutate(BONUS=round(BONUS,0))

View(PAG_REBATES_G139_0123)

PAG_REBATES_G139_0123 %>% summarize(BONUS=sum(BONUS)) 


## ============================================================================================

## SQL 849 VITAL

CP_849_0123 <- dbGetQuery(con2,"
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
                SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.08 BONUS
                 FROM
                 PDPRD
                 INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
                 GROUP BY 1,2,3,4,5,6,7,8") 

View(CP_849_0123)

CP_849_0123 %>% summarize(v=sum(VRVENDA))

CP_849_0123 %>% summarize(v=sum(VRVENDA)*0.08)

## OBS
OBS_849_0123 <-paste0("VITAL 849 ","REBATE "," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))



## LISTA PEDIDOS
LIST_REBATES_849_0123 <- CP_849_0123 %>% 
  mutate(CPF=rep(c("04455447911","06532582913"), length.out=nrow(CP_849_0123))) %>% 
  mutate(OBS=OBS_849_0123)


View(LIST_REBATES_849_0123)

## PAGAMENTOS 


PAG_REBATES_849_0123 <- LIST_REBATES_849_0123 %>% group_by(CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% as.data.frame() %>% 
  mutate(BONUS=round(sum(BONUS)/2,0)) %>% 
  mutate(OBS=OBS_849_0123) 


View(PAG_REBATES_849_0123)



### ===========================================================================================

## SQL 157 DOMBOSCO

CP_157_0123 <- dbGetQuery(con2,"
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

View(CP_157_0123)

CP_157_0123 %>% summarize(v=sum(VRVENDA))

CP_157_0123 %>% summarize(v=sum(VRVENDA)*0.1)

## OBS

OBS_157_0123 <- paste0("DOM BOSCO 157 ","REBATE ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_157_0123 <- inner_join(CP_157_0123,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_157_0123)

View(LIST_REBATES_157_0123)

LIST_REBATES_157_0123 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_157_0123 <- LIST_REBATES_157_0123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_157_0123) 

View(PAG_REBATES_157_0123)



### ===========================================================================================

## SQL OTICA EDUARDO G91
## SEM RESULTADO

CP_G91_0123 <- dbGetQuery(con2,"
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

View(CP_G91_0123)

CP_G91_0123 %>% summarize(v=sum(VRVENDA))

CP_G91_0123 %>% summarize(v=sum(VRVENDA)*0.00)

## OBS

OBS_G91_0123 <- paste0("OTICAS EDUARDO REBATE G",CP_G91_0123  %>% 
                         distinct(GCLCODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## JOIN CPF

LIST_REBATES_G91_0123 <- inner_join(CP_G91_0123,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_G91_0123)

View(LIST_REBATES_G91_0123)

LIST_REBATES_G91_0123 %>% summarize(v=sum(BONUS))


## PAY

PAG_REBATES_G91_0123 <- LIST_REBATES_G91_0123 %>% group_by(CPF) %>% summarize(BONUS=round(sum(BONUS),0)) %>% 
  mutate(OBS=OBS_G91_0123) 

View(PAG_REBATES_G91_0123)


### ===========================================================================================


## PAGAMENTOS


PAG_REBATES_0123 <- rbind(PAG_REBATES_G139_0123,
                          PAG_REBATES_849_0123,
                          PAG_REBATES_157_0123)

View(PAG_REBATES_0123)


range_write(PAG_REBATES_0123,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",range = "A1",sheet="RESUMO",reformat = FALSE)  



LIST_REBATES_0123 <- rbind(LIST_REBATES_G139_0123,
                           LIST_REBATES_849_0123,
                           LIST_REBATES_157_0123)

View(LIST_REBATES_0123)


range_write(LIST_REBATES_0123,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",range = "A1",sheet="REBATES",reformat = FALSE)  



## CREDITO CARTOES


CREDITO_CARTOES_REBATES_0123 <- left_join(PAG_REBATES_0123 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0123 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 




View(CREDITO_CARTOES_REBATES_0123)

## EXCLUI SEM CARTAO


CREDITO_CARTOES_REBATES_0123_2 <- CREDITO_CARTOES_REBATES_0123 %>% 
  filter(!is.na(NSERIE))

View(CREDITO_CARTOES_REBATES_0123_2)

# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_REBATES_0123_3 <- CREDITO_CARTOES_REBATES_0123_2  %>% 
  .[,c(4,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_REBATES_0123_3)  

CREDITO_CARTOES_REBATES_0123_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_REBATES_0123_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_REBATES_0123_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\DEZ\\CREDITO_CARTOES_REBATES.csv",
           row.names=FALSE,quote = FALSE)


