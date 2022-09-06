## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 0822
## SANDRO JAKOSKA

## =======================================================================================================  

## LIBRARIES

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(xlsx)

## =======================================================================================================  

## CONNECT DB

con2 <- dbConnect(odbc::odbc(), "reproreplica")

## ======================================================================================================= 

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## =======================================================================================================  

## SQL 

CP_INSIGNE_0822 <-dbGetQuery(con2,"
WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
  FROM CLIEN C
  INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
  LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
  INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
   WHERE CLICLIENTE='S'),
  
FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
  
PED AS (SELECT ID_PEDIDO,PEDCODIGO,P.CLICODIGO,CLINOMEFANT,GCLCODIGO,PEDDTEMIS,SETOR,
PEDDTBAIXA,PEDORIGEM,TPCODIGO,PEDAUTORIZOU,PEDORIGEM ORIGEM FROM PEDID P
   INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
    INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
     WHERE PEDDTBAIXA
     BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
      AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND PEDSITPED<>'C' AND PEDORIGEM='W'),

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
           ORIGEM,
            CAST(LEFT(GRVALORES,3) AS DOUBLE PRECISION) BONUS,
             SUM(PDPQTDADE) QTD,
              SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
              
  FROM PDPRD PD
   INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
    INNER JOIN AUX A ON PD.PROCODIGO=A.PROCODIGO
     INNER JOIN VALORES_PROMO ON PD.PROCODIGO=VALORES_PROMO.PROCODIGO 
      GROUP BY 1,2,3,4,5,6,7,8,9,10 HAVING SUM(PDPQTDADE)>=2
")  %>% mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  

## =======================================================================================================  

## OBS

OBS_SIG <- paste0("INSIGNE"," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))

## =======================================================================================================  

##REMOVE DUPLICATES

CP_INSIGNE_0822_2 <- CP_INSIGNE_0822 %>% filter(CLICODIGO!=213)

## =======================================================================================================  

## INTERSECTION

LIST_INSIGNE_0822 <- left_join(CP_INSIGNE_0822_2,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>% 
  mutate(OBS=OBS_SIG) 

## =======================================================================================================  

## PAY

PAG_INSIGNE_0822 <-  LIST_INSIGNE_0822 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## =======================================================================================================  

## FILTER DUPLICATED

CP_INSIGNE_0822_213 <- CP_INSIGNE_0822 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_0822_213 <- left_join(CP_INSIGNE_0822_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_0822_213 <-  LIST_INSIGNE_0822_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0822_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## ======================================================================================================= 

## PAGAMENTOS FINAL  


PAG_INSIGNE_0822_ALL <-  rbind( 
  PAG_INSIGNE_0822,
  PAG_INSIGNE_0822_213
) 

range_write(PAG_INSIGNE_0822_ALL,ss="1Ri4NCSv4zO1iSxj4AqzgQtxthf1qbnz4MSzz3OpVsRg",range = "A:P",sheet="INSIGNE",reformat = FALSE)  



insigne_emissao <- left_join(PAG_INSIGNE_0822_ALL %>% mutate(CPF=as.character(CPF)),CARTOES_0822,by="CPF") %>% View() 
filter(is.na(`Número do Série`)) 

View(insigne_emissao)

insigne_emissao %>% summarize(v=sum(BONUS))

PAGAMENTOS_ALELO_0822 %>% filter(.$Observações=='INSIGNE 0722') %>% summarize(v=sum(.$`Valor (R$)`))

left_join(PAG_INSIGNE_0822_ALL,CARTOES_ALELO,by="CPF")


## =============================================================================================================         

## LISTAGEM FINAL  


LIST_INSIGNE_0822_ALL <-  rbind( 
  LIST_INSIGNE_0822,
  LIST_INSIGNE_0822_213
) 

range_write(LIST_INSIGNE_0822_ALL,ss="1Ri4NCSv4zO1iSxj4AqzgQtxthf1qbnz4MSzz3OpVsRg",range = "A:P",sheet="PEDIDOS INSIGNE",reformat = FALSE)  

## =======================================================================================================  


## NAO BONIFICAR  


LIST_INSIGNE_0822 %>% filter(is.na(CPF3))

range_write(LIST_INSIGNE_0822 %>% filter(nchar(CPF3)<11 | is.na(CPF3)),ss="1Ri4NCSv4zO1iSxj4AqzgQtxthf1qbnz4MSzz3OpVsRg",range = "A1",sheet="NAO BONIFICAR",reformat = FALSE)  

## ======================================================================================================= 


## VIEW AND CHECK

View(CP_INSIGNE_0822)

CP_INSIGNE_0822 %>% summarize(B=sum(BONUS))

CP_INSIGNE_0822_2 %>% summarize(B=sum(BONUS))


## =======================================================================================================  


View(LIST_INSIGNE_0822)


LIST_INSIGNE_0822 %>% filter(CPF2=='NULL') %>% View()

LIST_INSIGNE_0822 %>% filter(is.na(CPF3)) %>% View()

## =======================================================================================================  


PAG_INSIGNE_0822 %>% summarize(V=sum(BONUS))

CP_INSIGNE_0822 %>% filter(is.na(CPF)) %>% View()

CP_INSIGNE_0822 %>% filter(CLICODIGO==213) %>% summarize(V=sum(BONUS))

CP_INSIGNE_0822 %>% filter(nchar(CPF)<11) %>% View()

CP_INSIGNE_0822_2 <- CP_INSIGNE_0822 %>% filter(CLICODIGO!=213)

View(CP_INSIGNE_0822_2)

## LINO

LIST_INSIGNE_0822_213 %>% filter(CLICODIGO==213) %>% View()

PAG_INSIGNE_0822_213 %>% View()

## FINAL

PAG_ALL_0822 %>% summarize(v=sum(BONUS))

LIST_INSIGNE_0822_ALL %>% summarize(v=sum(BONUS))



## =======================================================================================================  

## SQL 

CP_INSIGNE_0822_DIGITADOS <-dbGetQuery(con2,"
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
      AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND PEDSITPED<>'C' AND PEDORIGEM IN ('M','D')),

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
         PEDORIGEM,
          PEDAUTORIZOU,
           CAST(LEFT(GRVALORES,3) AS DOUBLE PRECISION) BONUS,
            SUM(PDPQTDADE) QTD,
             SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
              
  FROM PDPRD PD
   INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
    INNER JOIN AUX A ON PD.PROCODIGO=A.PROCODIGO
     INNER JOIN VALORES_PROMO ON PD.PROCODIGO=VALORES_PROMO.PROCODIGO 
      GROUP BY 1,2,3,4,5,6,7,8,9,10 HAVING SUM(PDPQTDADE)>=2
")  %>% mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  

View(CP_INSIGNE_0822_DIGITADOS)

range_write(CP_INSIGNE_0822_DIGITADOS,ss="1Ri4NCSv4zO1iSxj4AqzgQtxthf1qbnz4MSzz3OpVsRg",range = "A:P",sheet="PEDIDOS INSIGNE DIGITADOS",reformat = FALSE)  

## CREDITO

credito_cartoes_0922 <- left_join(PAG_INSIGNE_0822_ALL %>%
                                     mutate(CPF=as.character(CPF)),CARTOES_0822,by="CPF") %>% 
                                       .[,c(7,1,2,3)] %>% 
                                         rename_at(., ~ c("Número de Série","CPF","Valor da Carga","Observacao"))



## CARTOES ==========================================================================


emissao_cartoes_0922 <- left_join(PAG_INSIGNE_0822_ALL %>% mutate(CPF=as.character(CPF)),CARTOES_0822,by="CPF") %>% 
       left_join(.,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
         filter(!is.na(NSERIE)) 


emissao_cartoes_0922_min <- emissao_cartoes_0922 %>% .[,c(1,2,3,11:15)]

  
  range_write(emissao_cartoes_0922_min ,ss="10WezW63OwIO2-eKkYhhOQxbD9cu1ozLf8YF55gwCxdA",
               range = "A1",sheet="DADOS",reformat = FALSE)  






