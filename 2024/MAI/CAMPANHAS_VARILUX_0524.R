
## VARILUX ANTIGA
## PERIODO DE REFERENCIA 05.2024

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readxl)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


## G121 RINALDI ATE   =============================================================================================================       

CP_VARILUX_G121_0524 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R') OR FISCODIGO IN ('5.910')),

CLI AS (SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=121),

PD AS (SELECT ID_PEDIDO,
               FISCODIGO1,
               PEDDTBAIXA,
                PEDID.CLICODIGO,
                 GCLCODIGO,
                  SETOR,
                   CLINOMEFANT,
                    PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       
AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%XR%'),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%E DESIGN%'),

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%COMFORT%' OR PRODESCRICAO LIKE '%LIBERTY%' OR PRODESCRICAO LIKE '%PHYSIO%') AND LEFT(PROCODIGO,2)='LD'),
 
PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PD ON P.ID_PEDIDO=PD.ID_PEDIDO
   WHERE PROCODIGO='PAP'),
   
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PD ON P.ID_PEDIDPROMOCAO=PD.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN)

SELECT 
 PDPRD.ID_PEDIDO,
 FISCODIGO1 CFOP,
   PEDDTBAIXA,
     CLICODIGO,
     GCLCODIGO,
      SETOR,
        PROD1.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(50 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT
PDPRD.ID_PEDIDO,
FISCODIGO1 CFOP,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD2.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(40 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD2 ON PDPRD.PROCODIGO=PROD2.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2  UNION


SELECT 
PDPRD.ID_PEDIDO,
 FISCODIGO1 CFOP,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD3.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(30 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD3 ON PDPRD.PROCODIGO=PROD3.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_VARILUX_G121_0524 %>% summarize(v=sum(VRVENDA))

CP_VARILUX_G121_0524 %>% summarize(v=sum(BONUS))

## OBS

OBS_VARILUX_G121_0524 <- paste0("RINALDI G121 ","VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## JOIN CPF

LIST_VARILUX_G121_0524 <- inner_join(CP_VARILUX_G121_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>%
  .[,c(-2,-13)] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_VARILUX_G121_0524)


### pagamentos

PAG_VARILUX_G121_0524 <- LIST_VARILUX_G121_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>%
  mutate(OBS=OBS_VARILUX_G121_0524) 



## CP G224 ITACU ATE  =============================================================================================================         


VARILUX_G224_0524 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=224),

         PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
           FROM PEDID 
             INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
               INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
                 WHERE
                    PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
                          AND PEDSITPED <>'C'),

PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57)

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(25 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD ON PDPRD.PROCODIGO=PROD.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


VARILUX_G224_0524 %>% summarize(v=sum(VRVENDA))

VARILUX_G224_0524 %>% summarize(v=sum(BONUS))


OBS_VARILUX_G224_0524 <-  paste0("OTICA ITAÇU G224 TODOS VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## ASSOCIATE CPF

LIST_VARILUX_G224_0524 <- 
  VARILUX_G224_0524 %>% mutate(CPF='00524459924') %>% 
  mutate(OBS=OBS_VARILUX_G224_0524)



### pagamentos

PAG_VARILUX_G224_0524 <- LIST_VARILUX_G224_0524 %>% 
  group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_VARILUX_G224_0524) 



## PAGAMENTOS  =============================================================================================================       


PAG_VARILUX_0524 <-  
  rbind(
    PAG_VARILUX_G121_0524,
    PAG_VARILUX_G224_0524) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX")


PAG_VARILUX_0524 %>% summarize(v=sum(BONUS))

View(PAG_VARILUX_0524)

## LISTAGEM   =============================================================================================================         


LIST_VARILUX_0524 <-  rbind(
  LIST_VARILUX_G121_0524,
  LIST_VARILUX_G224_0524) 


LIST_VARILUX_0524 %>% summarize(v=sum(BONUS))

View(LIST_VARILUX_0524)
## CREDITO CARTOES ==============================================================================================================

CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)

CREDITO_CARTOES_VARILUX_0524 <- left_join(PAG_VARILUX_0524 %>%
                                            mutate(CPF=as.character(CPF)),
                                          CARTOES_ALELO %>% filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 



## EXCLUI SEM CARTAO

CREDITO_CARTOES_VARILUX_0524_2 <- CREDITO_CARTOES_VARILUX_0524 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VARILUX_0524_3 <- CREDITO_CARTOES_VARILUX_0524_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))



CREDITO_CARTOES_VARILUX_0524_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VARILUX_0524_3 %>% .[duplicated(.$CPF),]


# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_VARILUX_0524.csv")


# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_VARILUX_0524_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)



