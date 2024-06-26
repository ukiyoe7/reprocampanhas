
## VARILUX ANTIGA
## PERIODO DE REFERENCIA 0124
## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro")


BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## G121 RINALDI ATE   =============================================================================================================       

CP_VARILUX_G121_0124 <- dbGetQuery(con2,"
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

View(CP_VARILUX_G121_0124)

CP_VARILUX_G121_0124 %>% summarize(v=sum(VRVENDA))

CP_VARILUX_G121_0124 %>% summarize(v=sum(BONUS))

## OBS

OBS_VARILUX_G121_0124 <- paste0("RINALDI G121 ","VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## JOIN CPF

LIST_VARILUX_G121_0124 <- inner_join(CP_VARILUX_G121_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>%
  .[,c(-2,-13)] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_VARILUX_G121_0124)

View(LIST_VARILUX_G121_0124)


### pagamentos

PAG_VARILUX_G121_0124 <- LIST_VARILUX_G121_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>%
  mutate(OBS=OBS_VARILUX_G121_0124) 

View(PAG_VARILUX_G121_0124)

# Calc Bonus

PAG_VARILUX_G121_0124 %>% summarize(v=sum(BONUS))



## 1225 VINI ATE  =============================================================================================================         


CP_VARILUX_1225_0124 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE C.CLICODIGO=1225),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%COMFORT MAX%' AND PRODESCRICAO LIKE '%TGEN8%')),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%COMFORT MAX%' AND PRODESCRICAO NOT LIKE '%TGEN8%')),

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%VARILUX E DESIGN%' AND PRODESCRICAO LIKE '%TGEN8%')),

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%VARILUX E DESIGN%' AND PRODESCRICAO NOT LIKE '%TGEN8%')),

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%XR%' AND PRODESCRICAO LIKE '%TGEN8%')),

PROD6 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%XR%' AND PRODESCRICAO NOT LIKE '%TGEN8%')),


PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PD ON P.ID_PEDIDO=PD.ID_PEDIDO
   WHERE PROCODIGO='PAP'),
   
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PD ON P.ID_PEDIDPROMOCAO=PD.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN)

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD1.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(30 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD2.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(60 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD2 ON PDPRD.PROCODIGO=PROD2.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
      PROD3.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(50 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD3 ON PDPRD.PROCODIGO=PROD3.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION


SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD4.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(80 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD4 ON PDPRD.PROCODIGO=PROD4.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD5.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(70 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD5 ON PDPRD.PROCODIGO=PROD5.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD6.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(100 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD6 ON PDPRD.PROCODIGO=PROD6.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_VARILUX_1225_0124)

CP_VARILUX_1225_0124 %>% summarize(v=sum(VRVENDA))

CP_VARILUX_1225_0124 %>% summarize(v=sum(BONUS))

OBS_VARILUX_1225 <- paste0("VINI 1225 ","VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## JOIN CPF

LIST_VARILUX_1225_0124  <- inner_join(CP_VARILUX_1225_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_VARILUX_1225)

View(LIST_VARILUX_1225_0124)


### PAY

PAG_VARILUX_1225_0124 <- LIST_VARILUX_1225_0124 %>% 
  group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_VARILUX_1225) 

View(PAG_VARILUX_1225_0124)


PAG_VARILUX_1225_0124 %>% summarize(v=sum(BONUS))


## 206 RH ATE  =============================================================================================================         


CP_206_0124 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE C.CLICODIGO=206),

         PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
           FROM PEDID 
             INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
               INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
                 WHERE
                    PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
                          AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%XR%'),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%E DESIGN%'),

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%COMFORT%' OR PRODESCRICAO LIKE '%LIBERTY%') AND LEFT(PROCODIGO,2)='LD'),
 
PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PD ON P.ID_PEDIDO=PD.ID_PEDIDO
   WHERE PROCODIGO='PAP'),

PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PD ON P.ID_PEDIDPROMOCAO=PD.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN)

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD1.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(40 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT
PDPRD.ID_PEDIDO,
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
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
      PROD3.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(20 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD3 ON PDPRD.PROCODIGO=PROD3.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_206_0124)

CP_206_0124 %>% summarize(v=sum(VRVENDA))

CP_206_0124 %>% summarize(v=sum(BONUS))


OBS_206_0124 <-  paste0("OTICA RH ",CP_206_0124 %>% distinct(CLICODIGO)," ","VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_VARILUX_206_0124 <- inner_join(CP_206_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% 
  rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_206_0124)


View(LIST_VARILUX_206_0124)


### pagamentos

PAG_VARILUX_206_0124 <- LIST_VARILUX_206_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_206_0124)


View(PAG_VARILUX_206_0124)



## CP 4576 OJO ATE  =============================================================================================================         


CP_VARILUX_4576_0124 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE C.CLICODIGO=4576),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%XCLUSIVE%'),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%X 4D%'),

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%X TRACK%')),

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%X DESIGN%')),

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND (PRODESCRICAO LIKE '%PHYSIO%')),

PROD6 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%COMFORT MAX%'),

PROD7 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PRODESCRICAO LIKE '%EYEZEN%'),


PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PD ON P.ID_PEDIDO=PD.ID_PEDIDO
   WHERE PROCODIGO='PAP'),
   
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PD ON P.ID_PEDIDPROMOCAO=PD.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN)

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD1.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(130 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD2.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(110 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD2 ON PDPRD.PROCODIGO=PROD2.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
      PROD3.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(90 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD3 ON PDPRD.PROCODIGO=PROD3.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION


SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD4.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(80 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD4 ON PDPRD.PROCODIGO=PROD4.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD5.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(60 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD5 ON PDPRD.PROCODIGO=PROD5.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD6.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(50 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD6 ON PDPRD.PROCODIGO=PROD6.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION


SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD7.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(40 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD7 ON PDPRD.PROCODIGO=PROD7.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PDPRD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_VARILUX_4576_0124)

CP_VARILUX_4576_0124 %>% summarize(v=sum(VRVENDA))

CP_VARILUX_4576_0124 %>% summarize(v=sum(BONUS))

OBS_VARILUX_4576_0124 <- paste0("OJO 4576 " ,"VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## JOIN CPF

LIST_VARILUX_4576_0124  <- inner_join(CP_VARILUX_4576_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_VARILUX_4576_0124)

View(LIST_VARILUX_4576_0124)


### PAY

PAG_VARILUX_4576_0124 <- LIST_VARILUX_4576_0124 %>% 
  group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_VARILUX_4576_0124) 

View(PAG_VARILUX_4576_0124)


PAG_VARILUX_4576_0124 %>% summarize(v=sum(BONUS))


## CP G224 ITACU ATE  =============================================================================================================         


VARILUX_G224_0124 <- dbGetQuery(con2,"
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

View(VARILUX_G224_0124)

VARILUX_G224_0124 %>% summarize(v=sum(VRVENDA))

VARILUX_G224_0124 %>% summarize(v=sum(BONUS))


OBS_VARILUX_G224_0124 <-  paste0("OTICA ITAÇU G224 TODOS VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## ASSOCIATE CPF

LIST_VARILUX_G224_0124 <- 
  VARILUX_G224_0124 %>% mutate(CPF='00524459924') %>% 
  mutate(OBS=OBS_VARILUX_G224_0124)


View(LIST_VARILUX_G224_0124)


### pagamentos

PAG_VARILUX_G224_0124 <- LIST_VARILUX_G224_0124 %>% 
  group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_VARILUX_G224_0124) 

View(PAG_VARILUX_G224_0124)


## PAGAMENTOS  =============================================================================================================       


PAG_VARILUX_0124 <-  
  rbind(
    PAG_VARILUX_G121_0124,
    PAG_VARILUX_1225_0124,
    PAG_VARILUX_206_0124, 
    PAG_VARILUX_G224_0124) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX")


View(PAG_VARILUX_0124)

PAG_VARILUX_0124 %>% summarize(v=sum(BONUS))



## LISTAGEM FINAL   =============================================================================================================         


LIST_VARILUX_0124 <-  rbind(
  LIST_VARILUX_G121_0124,
  LIST_VARILUX_1225_0124,
  LIST_VARILUX_206_0124,
  LIST_VARILUX_G224_0124) 


View(LIST_VARILUX_0124)

LIST_VARILUX_0124 %>% summarize(v=sum(BONUS))


## CREDITO CARTOES ==============================================================================================================

CARTOES_260124 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\CARTOES_260124.RData"))


CREDITO_CARTOES_VARILUX_0124 <- left_join(PAG_VARILUX_0124 %>%
                                            mutate(CPF=as.character(CPF)),
                                          CARTOES_260124 %>% filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VARILUX_0124)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_VARILUX_0124_2 <- CREDITO_CARTOES_VARILUX_0124 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_VARILUX_0124_2)


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VARILUX_0124_3 <- CREDITO_CARTOES_VARILUX_0124_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_VARILUX_0124_3)  

CREDITO_CARTOES_VARILUX_0124_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VARILUX_0124_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_VARILUX_0124_3,
           file = "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\JAN\\CREDITO_CARTOES_VARILUX_0124.csv",
           row.names=FALSE,quote = FALSE)




