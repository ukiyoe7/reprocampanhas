
## CAMPANHAS VARILUX ECONOMICA
## PERIODO REFERENCIA 05.2024

## LOAD ======================================================

library(DBI)
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")


BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


## CLIENTES ======================================================


clientes_vlx_0524 <- dbGetQuery(con2,"SELECT CP.CLICODIGO,CLINOMEFANT NOME,GCLCODIGO GRUPO,SETOR FROM CLIPROMO CP
                                                LEFT JOIN CLIEN C ON C.CLICODIGO=CP.CLICODIGO
                                                 LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                                  LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                                   E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON CP.CLICODIGO=A.CLICODIGO
                                                    WHERE ID_PROMO=43") %>% mutate(CAMPANHA='VLX ECONOMICA')

View(clientes_vlx_0524)

## SQL ======================================================

query_vlx_0524 <- dbGetQuery(con2,"WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=43)CLP ON C.CLICODIGO=CLP.CLICODIGO
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO),

PED AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
AND PEDSITPED <>'C'),

PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PED PE ON  P.ID_PEDIDO=PE.ID_PEDIDO
   WHERE PROCODIGO='PAP'),                 
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PED PE ON P.ID_PEDIDPROMOCAO=PE.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE (MARCODIGO=57 OR PRODESCRICAO LIKE '%EYEZEN%')),

-- EYEZEN

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, 20 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO, 60 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%EYEZEN%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                 
                  
-- LIBERTY

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO, 20 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%LIBERTY%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO, 60 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%LIBERTY%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

-- COMFORT                 

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO, 40 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%COMFORT%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD6 AS (SELECT PROCODIGO,PRODESCRICAO, 100 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%COMFORT%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                 

-- VARILUX PHYSIO

PROD7 AS (SELECT PROCODIGO,PRODESCRICAO, 50 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PHYSIO%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD8 AS (SELECT PROCODIGO,PRODESCRICAO, 120 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PHYSIO%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),


-- VARILUX E

           
PROD9 AS (SELECT PROCODIGO,PRODESCRICAO, 60 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%VARILUX E%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD10 AS (SELECT PROCODIGO,PRODESCRICAO, 140 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%VARILUX E%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),               

-- FINAL SELECT

RESULT AS (                
SELECT 
PD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
      CASE 
      WHEN  PROD1.PROCODIGO IS NOT NULL THEN PROD1.BONUS
      WHEN  PROD2.PROCODIGO IS NOT NULL THEN PROD2.BONUS
      WHEN  PROD3.PROCODIGO IS NOT NULL THEN PROD3.BONUS
      WHEN  PROD4.PROCODIGO IS NOT NULL THEN PROD4.BONUS
      WHEN  PROD5.PROCODIGO IS NOT NULL THEN PROD5.BONUS
      WHEN  PROD6.PROCODIGO IS NOT NULL THEN PROD6.BONUS
      WHEN  PROD7.PROCODIGO IS NOT NULL THEN PROD7.BONUS
      WHEN  PROD8.PROCODIGO IS NOT NULL THEN PROD8.BONUS
      WHEN  PROD9.PROCODIGO IS NOT NULL THEN PROD9.BONUS
      WHEN  PROD10.PROCODIGO IS NOT NULL THEN PROD10.BONUS

      ELSE 0 END BONUS, 
        PD.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED p ON PD.ID_PEDIDO=P.ID_PEDIDO
INNER JOIN PROD  ON PD.PROCODIGO=PROD.PROCODIGO
LEFT JOIN PROD1 ON PD.PROCODIGO=PROD1.PROCODIGO
LEFT JOIN PROD2 ON PD.PROCODIGO=PROD2.PROCODIGO
LEFT JOIN PROD3 ON PD.PROCODIGO=PROD3.PROCODIGO
LEFT JOIN PROD4 ON PD.PROCODIGO=PROD4.PROCODIGO
LEFT JOIN PROD5 ON PD.PROCODIGO=PROD5.PROCODIGO
LEFT JOIN PROD6 ON PD.PROCODIGO=PROD6.PROCODIGO
LEFT JOIN PROD7 ON PD.PROCODIGO=PROD7.PROCODIGO
LEFT JOIN PROD8 ON PD.PROCODIGO=PROD8.PROCODIGO
LEFT JOIN PROD9 ON PD.PROCODIGO=PROD9.PROCODIGO
LEFT JOIN PROD10 ON PD.PROCODIGO=PROD10.PROCODIGO

LEFT OUTER JOIN PED_PROMO_UNION ON PD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2)
                             
SELECT * FROM RESULT WHERE BONUS<>0 ")



## CAMPANHA VARILUX ECONOMICA G148 HEUSI   =====================================


CP_G148_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_G148_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_0524 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_0524 <- inner_join(CP_G148_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G148_VLX_ECONO_0524)


### pagamentos

PAG_G148_VLX_ECONO_0524 <- LIST_G148_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA G257 ZOLET  =====================================


CP_G257_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G257_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_0524 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_0524 <- inner_join(CP_G257_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_G257_VLX_ECONO_0524)


### pagamentos

PAG_G257_VLX_ECONO_0524 <- LIST_G257_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_0524)



## CAMPANHA VARILUX ECONOMICA 986 OTICA LUZ =====================================

CP_986_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_986_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_0524 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_0524 <- inner_join(CP_986_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_986_VLX_ECONO_0524)


### pagamentos

PAG_986_VLX_ECONO_0524 <- LIST_986_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0524)



## CAMPANHA VARILUX ECONOMICA 1923 ART JOIAS  =====================================


CP_1923_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1923_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_ECONO_0524 <-  paste0("RELOJOARIA ART JOIAS 1923 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_ECONO_0524 <- inner_join(CP_1923_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_1923_VLX_ECONO_0524)


### pagamentos

PAG_1923_VLX_ECONO_0524 <- LIST_1923_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA 1818 OTICA TIAGO  =====================================


CP_1818_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1818) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1818_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1818_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1818_VLX_ECONO_0524 <-  paste0("OTICA JOREL 1818 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1818_VLX_ECONO_0524 <- inner_join(CP_1818_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_1818_VLX_ECONO_0524)


### pagamentos

PAG_1818_VLX_ECONO_0524 <- LIST_1818_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1818_VLX_ECONO_0524)




## CAMPANHA VARILUX ECONOMICA 1225 VINI  =====================================


CP_1225_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1225) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1225_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1225_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1225_VLX_ECONO_0524 <-  paste0("OTICA VINI 1225 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1225_VLX_ECONO_0524 <- inner_join(CP_1225_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_1225_VLX_ECONO_0524)


### pagamentos

PAG_1225_VLX_ECONO_0524 <- LIST_1225_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1225_VLX_ECONO_0524)



## CAMPANHA VARILUX ECONOMICA 206 RH  =====================================


CP_206_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==206) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_206_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_206_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_206_VLX_ECONO_0524 <-  paste0("OTICA RH 206 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_206_VLX_ECONO_0524 <- inner_join(CP_206_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_206_VLX_ECONO_0524)


### pagamentos

PAG_206_VLX_ECONO_0524 <- LIST_206_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_206_VLX_ECONO_0524)



## CAMPANHA VARILUX ECONOMICA 360 UNIVERSAL =====================================


CP_360_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==360) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_360_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_360_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_360_VLX_ECONO_0524 <-  paste0("OTICA UNIVERSAL 360  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_360_VLX_ECONO_0524 <- inner_join(CP_360_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_360_VLX_ECONO_0524)


### pagamentos

PAG_360_VLX_ECONO_0524 <- LIST_360_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_360_VLX_ECONO_0524)



## CAMPANHA VARILUX ECONOMICA G12 PALADIO =====================================


CP_G12_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==12) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G12_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G12_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G12_VLX_ECONO_0524 <-  paste0("OTICAS PALADIO G12 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G12_VLX_ECONO_0524 <- inner_join(CP_G12_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12]  %>% mutate(OBS=OBS_G12_VLX_ECONO_0524)


### pagamentos

PAG_G12_VLX_ECONO_0524 <- LIST_G12_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G12_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA G77 DANJU =====================================

CP_G77_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==77) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) %>% 
  filter(BONUS!=0)


CP_G77_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G77_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G77_VLX_ECONO_0524 <-  paste0("OTICA DANJU G77 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G77_VLX_ECONO_0524 <- inner_join(CP_G77_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G77_VLX_ECONO_0524)


### pagamentos

PAG_G77_VLX_ECONO_0524 <- LIST_G77_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G77_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA G60 GUZZATTI =====================================

CP_G60_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==60) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G60_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G60_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G60_VLX_ECONO_0524 <-  paste0("OTICA GUZZATI G60 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G60_VLX_ECONO_0524 <- inner_join(CP_G60_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G60_VLX_ECONO_0524)


### pagamentos

PAG_G60_VLX_ECONO_0524 <- LIST_G60_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G60_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA 2183 MADRID =====================================


CP_2183_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==2183) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_2183_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_2183_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_2183_VLX_ECONO_0524 <-  paste0("OTICA MADRID 2183  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_2183_VLX_ECONO_0524 <- inner_join(CP_2183_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_2183_VLX_ECONO_0524)


### pagamentos

PAG_2183_VLX_ECONO_0524 <- LIST_2183_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_2183_VLX_ECONO_0524)




## CAMPANHA VARILUX ECONOMICA 1836 DIGITUS =====================================


CP_1836_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1836) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1836_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1836_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1836_VLX_ECONO_0524 <-  paste0("OTICA DIGITUS 1836  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1836_VLX_ECONO_0524 <- inner_join(CP_1836_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_1836_VLX_ECONO_0524)


### pagamentos

PAG_1836_VLX_ECONO_0524 <- LIST_1836_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1836_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA 719 FISCH =====================================


CP_719_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==719) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_719_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_719_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_719_VLX_ECONO_0524 <-  paste0("OTICA FISCH 719  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_719_VLX_ECONO_0524 <- inner_join(CP_719_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_719_VLX_ECONO_0524)


### pagamentos

PAG_719_VLX_ECONO_0524 <- LIST_719_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_719_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA G180 HELENA =====================================

CP_G180_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==180) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G180_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G180_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G180_VLX_ECONO_0524 <-  paste0("OTICA HELENA G180 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G180_VLX_ECONO_0524 <- inner_join(CP_G180_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G180_VLX_ECONO_0524)


### pagamentos

PAG_G180_VLX_ECONO_0524 <- LIST_G180_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G180_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA 4796 OJO =====================================


CP_4796_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==4796) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_4796_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_4796_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_4796_VLX_ECONO_0524 <-  paste0("OTICA OJO 4796  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_4796_VLX_ECONO_0524 <- inner_join(CP_4796_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_4796_VLX_ECONO_0524)


### pagamentos

PAG_4796_VLX_ECONO_0524 <- LIST_4796_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4796_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA 400 UNIVERSAL =====================================


CP_400_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==400) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_400_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_400_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_400_VLX_ECONO_0524 <-  paste0("OTICA UNIVERSAL 400  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_400_VLX_ECONO_0524 <- inner_join(CP_400_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_400_VLX_ECONO_0524)


### pagamentos

PAG_400_VLX_ECONO_0524 <- LIST_400_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_400_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA G31 HUSADEL =====================================


CP_G31_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==31) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G31_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G31_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G31_VLX_ECONO_0524 <-  paste0("OTICA HUSADEL G31  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G31_VLX_ECONO_0524 <- inner_join(CP_G31_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G31_VLX_ECONO_0524)


### pagamentos

PAG_G31_VLX_ECONO_0524 <- LIST_G31_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G31_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA G187 KRAFTS =====================================


CP_G187_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==187) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G187_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G187_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G187_VLX_ECONO_0524 <-  paste0("OTICA KRAFTS G187 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G187_VLX_ECONO_0524 <- inner_join(CP_G187_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G187_VLX_ECONO_0524)


### pagamentos

PAG_G187_VLX_ECONO_0524 <- LIST_G187_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G187_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA 569 GUI =====================================


CP_569_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==569) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_569_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_569_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_569_VLX_ECONO_0524 <-  paste0("OTICA GUI 569  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_569_VLX_ECONO_0524 <- inner_join(CP_569_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_569_VLX_ECONO_0524)


### pagamentos

PAG_569_VLX_ECONO_0524 <- LIST_569_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_569_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA G133 SAFIRA =====================================


CP_G133_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(GCLCODIGO==133) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_G133_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_G133_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_G133_VLX_ECONO_0524 <-  paste0("OTICA SAFIRA G133 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G133_VLX_ECONO_0524 <- inner_join(CP_G133_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G133_VLX_ECONO_0524)


### pagamentos

PAG_G133_VLX_ECONO_0524 <- LIST_G133_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G133_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA 4442 AUTENTIKA =====================================


CP_4442_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==4442) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_4442_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_4442_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_4442_VLX_ECONO_0524 <-  paste0("OTICA AUTENTIKA 4442  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_4442_VLX_ECONO_0524 <- inner_join(CP_4442_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_4442_VLX_ECONO_0524)


### pagamentos

PAG_4442_VLX_ECONO_0524 <- LIST_4442_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4442_VLX_ECONO_0524)

## CAMPANHA VARILUX ECONOMICA 1669 VEJA MAIS =====================================


CP_1669_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1669) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1669_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1669_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1669_VLX_ECONO_0524 <-  paste0("OTICA VEJA MAIS 1669  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1669_VLX_ECONO_0524 <- inner_join(CP_1669_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_1669_VLX_ECONO_0524)


### pagamentos

PAG_1669_VLX_ECONO_0524 <- LIST_1669_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1669_VLX_ECONO_0524)


## CAMPANHA VARILUX ECONOMICA 1050 OTICA ELEGANCE =====================================


CP_1050_VLX_ECONO_0524 <- 
  query_vlx_0524 %>% 
  filter(CLICODIGO==1050) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 


CP_1050_VLX_ECONO_0524 %>% summarize(v=sum(VRVENDA))

CP_1050_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))


OBS_1050_VLX_ECONO_0524 <-  paste0("OTICA ELEGANCE 1050  VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1050_VLX_ECONO_0524 <- inner_join(CP_1050_VLX_ECONO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_1050_VLX_ECONO_0524)


### pagamentos

PAG_1050_VLX_ECONO_0524 <- LIST_1050_VLX_ECONO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1050_VLX_ECONO_0524)



## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_0524 <-  
  rbind(
    PAG_G148_VLX_ECONO_0524,
    PAG_G257_VLX_ECONO_0524,
    PAG_986_VLX_ECONO_0524,
    PAG_1923_VLX_ECONO_0524,
    PAG_1818_VLX_ECONO_0524,
    PAG_1225_VLX_ECONO_0524,
    PAG_206_VLX_ECONO_0524,
    PAG_360_VLX_ECONO_0524,
    PAG_G12_VLX_ECONO_0524,
    PAG_G77_VLX_ECONO_0524,
    PAG_G60_VLX_ECONO_0524,
    PAG_2183_VLX_ECONO_0524,
    PAG_1836_VLX_ECONO_0524,
    PAG_719_VLX_ECONO_0524,
    PAG_G180_VLX_ECONO_0524,
    PAG_4796_VLX_ECONO_0524,
    PAG_400_VLX_ECONO_0524,
    PAG_G31_VLX_ECONO_0524,
    PAG_G187_VLX_ECONO_0524,
    PAG_569_VLX_ECONO_0524,
    PAG_G133_VLX_ECONO_0524,
    PAG_4442_VLX_ECONO_0524,
    PAG_1669_VLX_ECONO_0524,
    PAG_1050_VLX_ECONO_0524
  ) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX ECONOMICA")


View(PAG_VLX_ECONO_0524)

PAG_VLX_ECONO_0524 %>% summarize(v=sum(BONUS))



## LISTAGEM PEDIDOS   =============================================================================================================         


LIST_VLX_ECONO_0524 <-  rbind(
  LIST_G148_VLX_ECONO_0524,
  LIST_G257_VLX_ECONO_0524,
  LIST_986_VLX_ECONO_0524,
  LIST_1923_VLX_ECONO_0524,
  LIST_1818_VLX_ECONO_0524,
  LIST_1225_VLX_ECONO_0524,
  LIST_206_VLX_ECONO_0524,
  LIST_360_VLX_ECONO_0524,
  LIST_G12_VLX_ECONO_0524,
  LIST_G77_VLX_ECONO_0524,
  LIST_G60_VLX_ECONO_0524,
  LIST_2183_VLX_ECONO_0524,
  LIST_1836_VLX_ECONO_0524,
  LIST_719_VLX_ECONO_0524,
  LIST_G180_VLX_ECONO_0524,
  LIST_4796_VLX_ECONO_0524,
  LIST_400_VLX_ECONO_0524,
  LIST_G31_VLX_ECONO_0524,
  LIST_G187_VLX_ECONO_0524,
  LIST_569_VLX_ECONO_0524,
  LIST_G133_VLX_ECONO_0524,
  LIST_4442_VLX_ECONO_0524,
  LIST_1669_VLX_ECONO_0524,
  LIST_1050_VLX_ECONO_0524
) 

View(LIST_VLX_ECONO_0524)

LIST_VLX_ECONO_0524%>% summarize(v=sum(BONUS))



## CREDITO CARTOES ==============================================================================================================


CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)



CREDITO_CARTOES_VLX_ECONO_0524 <- left_join(PAG_VLX_ECONO_0524 %>%
                                              mutate(CPF=as.character(CPF)),
                                            CARTOES_ALELO %>% filter(STATUS!="Cancelado") %>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 



## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_ECONO_0524_2 <- CREDITO_CARTOES_VLX_ECONO_0524 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')



## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_ECONO_0524_3 <- CREDITO_CARTOES_VLX_ECONO_0524_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))




# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_VLX_ECONO_0524.csv")


# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_VLX_ECONO_0524_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)


