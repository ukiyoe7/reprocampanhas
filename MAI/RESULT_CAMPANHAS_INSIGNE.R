## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 0522
## SANDRO JAKOSKA

## =======================================================================================================  

## LIBRARIES

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

## =======================================================================================================  

## CONNECT DB

con2 <- dbConnect(odbc::odbc(), "reproreplica")

## ======================================================================================================= 

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## =======================================================================================================  

## SQL 

CP_INSIGNE_0522 <-dbGetQuery(con2,"
WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
  FROM CLIEN C
  INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
  LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
  INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
   WHERE CLICLIENTE='S'),
  
FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
  
PED AS (SELECT ID_PEDIDO,PEDCODIGO,P.CLICODIGO,CLINOMEFANT,GCLCODIGO,PEDDTEMIS,SETOR,
PEDDTBAIXA,PEDORIGEM,TPCODIGO,PEDAUTORIZOU FROM PEDID P
   INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
    INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
     WHERE PEDDTBAIXA
     BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
      AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND PEDSITPED<>'C'),

AUX AS (SELECT PROCODIGO,PROCODIGO2,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2)CHAVE FROM PRODU
WHERE MARCODIGO=189 AND PROTIPO<>'T'),

VALORES_PROMO AS (SELECT PROCODIGO,GRVALORES
FROM NGRUPOS 
 INNER JOIN GRUPOVALORES ON NGRUPOS.GRCODIGO=GRUPOVALORES.GRCODIGO
  WHERE NGRUPOS.GRCODIGO IN (125,127,129,158))
  
SELECT 
PD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
     GCLCODIGO,
      SETOR,
        PD.PROCODIGO,
         PDPDESCRICAO,
          PEDAUTORIZOU,
           CAST(LEFT(GRVALORES,3) AS DOUBLE PRECISION) BONUS,
            SUM(PDPQTDADE) QTD,
             SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
              
  FROM PDPRD PD
   INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
    INNER JOIN AUX A ON PD.PROCODIGO=A.PROCODIGO
     INNER JOIN VALORES_PROMO ON PD.PROCODIGO=VALORES_PROMO.PROCODIGO 
      GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2
")  %>% mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  

## =======================================================================================================  

## OBS

OBS_SIG <- paste0("INSIGNE"," ",format(floor_date(Sys.Date(),"month"),"%m/%y"))

## =======================================================================================================  

##REMOVE DUPLICATES

CP_INSIGNE_0522_2 <- CP_INSIGNE_0522 %>% filter(CLICODIGO!=213)

## =======================================================================================================  

## INTERSECTION

LIST_INSIGNE_0522 <- left_join(CP_INSIGNE_0522_2,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
   mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>% 
    mutate(OBS=OBS_SIG) 

## =======================================================================================================  

## PAY

PAG_INSIGNE_0522 <-  LIST_INSIGNE_0522 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## =======================================================================================================  

## FILTER DUPLICATED

CP_INSIGNE_0522_213 <- CP_INSIGNE_0522 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_0522_213 <- left_join(CP_INSIGNE_0522_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_0522_213 <-  LIST_INSIGNE_0522_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0522_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## ======================================================================================================= 

## PAGAMENTOS FINAL  


PAG_INSIGNE_0522_ALL <-  rbind( 
  PAG_INSIGNE_0522,
  PAG_INSIGNE_0522_213
) 

range_write(PAG_INSIGNE_0522_ALL,ss="1GYqiPa3H-v-bt_YAzixLzD4gwHMyGEA916zabrL_RUE",range = "A1",sheet="INSIGNE",reformat = FALSE)  


## =============================================================================================================         

## LISTAGEM FINAL  


LIST_INSIGNE_0522_ALL <-  rbind( 
  LIST_INSIGNE_0522,
  LIST_INSIGNE_0522_213
) 

range_write(LIST_INSIGNE_0522_ALL,ss="1GYqiPa3H-v-bt_YAzixLzD4gwHMyGEA916zabrL_RUE",range = "A1",sheet="PEDIDOS INSIGNE",reformat = FALSE)  

## =======================================================================================================  


## NAO BONIFICAR  


LIST_INSIGNE_0522 %>% filter(is.na(CPF3))

range_write(LIST_INSIGNE_0522 %>% filter(is.na(CPF3)),ss="1GYqiPa3H-v-bt_YAzixLzD4gwHMyGEA916zabrL_RUE",range = "A1",sheet="NAO BONIFICAR",reformat = FALSE)  

## ======================================================================================================= 


## VIEW AND CHECK

View(CP_INSIGNE_0522)

CP_INSIGNE_0522 %>% summarize(B=sum(BONUS))

CP_INSIGNE_0522_2 %>% summarize(B=sum(BONUS))


## =======================================================================================================  


View(LIST_INSIGNE_0522)


LIST_INSIGNE_0522 %>% filter(CPF2=='NULL') %>% View()

LIST_INSIGNE_0522 %>% filter(is.na(CPF3)) %>% View()

## =======================================================================================================  


PAG_INSIGNE_0522 %>% summarize(V=sum(BONUS))

CP_INSIGNE_0522 %>% filter(is.na(CPF)) %>% View()

CP_INSIGNE_0522 %>% filter(CLICODIGO==213) %>% summarize(V=sum(BONUS))

CP_INSIGNE_0522 %>% filter(nchar(CPF)<11) %>% View()

CP_INSIGNE_0522_2 <- CP_INSIGNE_0522 %>% filter(CLICODIGO!=213)

View(CP_INSIGNE_0522_2)

## LINO

LIST_INSIGNE_0522_213 %>% filter(CLICODIGO==213) %>% View()

PAG_INSIGNE_0522_213 %>% View()

## FINAL

PAG_ALL_0522 %>% summarize(v=sum(BONUS))

LIST_INSIGNE_0522_ALL %>% summarize(v=sum(BONUS))






