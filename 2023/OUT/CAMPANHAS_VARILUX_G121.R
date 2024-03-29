
## APURACAO CAMPANHAS 
## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## G121 RINALDI XR  =============================================================================================================       

CP_VARILUX_G121_XR <- dbGetQuery(con2,"
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
WHERE PEDDTBAIXA BETWEEN '01.08.2023' AND '31.10.2023'
       
AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%XR%'),

 
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
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_VARILUX_G121_XR)

## OBS

OBS_VARILUX_G121_XR <- paste0("RINALDI G121 ","VARILUX RETROATIVA XR ") 


## JOIN CPF

LIST_VARILUX_G121_XR <- inner_join(CP_VARILUX_G121_XR,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>%
  .[,c(-2,-13)] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_VARILUX_G121_XR) %>% 
   mutate(CPF=as.character(CPF))

View(LIST_VARILUX_G121_XR)

write.csv2(LIST_VARILUX_G121_XR,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_VARILUX_G121_XR.csv" ,row.names = FALSE )



## PAGAMENTOS

PAG_VARILUX_G121_XR <- LIST_VARILUX_G121_XR %>% group_by(CLICODIGO,CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>%
  mutate(OBS=OBS_VARILUX_G121_XR) %>% 
  mutate(CPF=as.character(CPF))

View(PAG_VARILUX_G121_XR)

write.csv2(PAG_VARILUX_G121_XR,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\PAG_VARILUX_G121_XR.csv" ,row.names = FALSE )




