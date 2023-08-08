
## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 0723
## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")


BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## G45 DINIZ =============================================================================================================         

# SQL 

CP_G45_0723 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=45),

         PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
           FROM PEDID 
             INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
               INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
                 WHERE
                    PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
                          AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROCODIGO='LD0438')

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
            CAST(10 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G45_0723)

CP_G45_0723 %>% summarize(v=sum(VRVENDA))

CP_G45_0723 %>% summarize(v=sum(BONUS))


OBS_G45 <-  paste0("GRUPO DINIZ G45 IMAGEM",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_VARILUX_G45_0723 <- inner_join(CP_G45_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_G45)


View(LIST_VARILUX_G45_0723)


### pagamentos

PAG_VARILUX_G45_0723 <- LIST_VARILUX_G45_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G45)


View(PAG_VARILUX_G45_0723)
