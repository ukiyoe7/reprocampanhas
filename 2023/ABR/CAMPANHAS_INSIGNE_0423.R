## PERIODO DE REFERENCIA 0423
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")

## =======================================================================================================  

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## =======================================================================================================  

## SQL 

CP_INSIGNE_0423 <-dbGetQuery(con2,"
WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
  FROM CLIEN C
  INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
  LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
  INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
   WHERE CLICLIENTE='S'),
  
FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
  
PED AS (SELECT ID_PEDIDO,
                PEDCODIGO,
                 P.CLICODIGO,
                  CLINOMEFANT,
                   GCLCODIGO,
                    PEDDTEMIS,
                     SETOR,
                      PEDDTBAIXA,
                       PEDORIGEM,
                        TPCODIGO,
                         PEDAUTORIZOU,
                          PEDORIGEM ORIGEM FROM PEDID P
   INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
    INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
     WHERE 

       PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
     
       AND
         PEDSITPED<>'C' AND PEDORIGEM='W'),

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
")  %>% 
  mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  

## =======================================================================================================  

## OBS

OBS_SIG <- paste0("INSIGNE"," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## RESUMO

CP_INSIGNE_0423 %>% summarize(v=sum(VRVENDA))

CP_INSIGNE_0423 %>% summarize(v=sum(BONUS))

CP_INSIGNE_0423 %>% filter(CLICODIGO==151) %>% summarize(v=sum(BONUS))



##REMOVE DUPLICATES

CP_INSIGNE_0423_2 <- CP_INSIGNE_0423 %>% filter(CLICODIGO!=213)

CP_INSIGNE_0423_3 <- CP_INSIGNE_0423_2 %>% filter(CLICODIGO!=151)

## INTERSECTION

LIST_INSIGNE_0423 <- left_join(CP_INSIGNE_0423_3,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%
  mutate(OBS=OBS_SIG) 


## PAY

PAG_INSIGNE_0423 <-  LIST_INSIGNE_0423 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## 151 ========

CP_INSIGNE_0423_151 <- CP_INSIGNE_0423 %>% filter(CLICODIGO==151)

View(CP_INSIGNE_0423_151)

LIST_INSIGNE_0423_151 <- left_join(CP_INSIGNE_0423_151,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%  
  mutate(CPF3=ifelse(nchar(gsub("[^[:alnum:]]", "", PEDAUTORIZOU)) < 6,'00773625941',CPF2))

View(LIST_INSIGNE_0423_151)

PAG_INSIGNE_0423_151 <-  LIST_INSIGNE_0423_151 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

View(PAG_INSIGNE_0423_151)

## 213  ========

CP_INSIGNE_0423_213 <- CP_INSIGNE_0423 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_0423_213 <- left_join(CP_INSIGNE_0423_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_0423_213 <-  LIST_INSIGNE_0423_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0423_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## ======================================================================================================= 

## PAGAMENTOS FINAL  


PAG_INSIGNE_0423_ALL <-  rbind( 
  PAG_INSIGNE_0423,
  PAG_INSIGNE_0423_213,
  PAG_INSIGNE_0423_151
) 

View(PAG_INSIGNE_0423_ALL)


PAG_INSIGNE_0423_ALL %>% summarize(v=sum(BONUS))


range_write(PAG_INSIGNE_0423_ALL,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A14",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## =============================================================================================================         

## LISTAGEM FINAL  


LIST_INSIGNE_0423_ALL <-  rbind( 
  LIST_INSIGNE_0423,
  LIST_INSIGNE_0423_213,
  LIST_INSIGNE_0423_151
) 

View(LIST_INSIGNE_0423_ALL)


range_write(LIST_INSIGNE_0423_ALL ,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A:P",sheet="INSIGNE",reformat = FALSE)  


## =======================================================================================================  

range_write(LIST_INSIGNE_0423_ALL %>% filter(CLICODIGO==151) ,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A:P",sheet="DADOS",reformat = FALSE)  


## ======================================================================================================= 


## CREDITO CARTOES

CREDITO_CARTOES_INSIGNE_0423 <- left_join(PAG_INSIGNE_0423_ALL %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0523 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_INSIGNE_0423)


## EXCLUI SEM CARTAO

CREDITO_CARTOES_INSIGNE_0423_2 <- CREDITO_CARTOES_INSIGNE_0423 %>% 
  filter(!is.na(NSERIE))

View(CREDITO_CARTOES_INSIGNE_0423_2)

CREDITO_CARTOES_INSIGNE_0423_2 %>% .[duplicated(.$CPF),] 

CREDITO_CARTOES_INSIGNE_0423_2 %>% n_distinct(.$CPF)


left_join(CREDITO_CARTOES_INSIGNE_0423_2,CARTOES_0423,by="CPF") %>% View()


# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_INSIGNE_0423_3 <- CREDITO_CARTOES_INSIGNE_0423_2  %>% 
  .[,c(4,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_0423_3)  

CREDITO_CARTOES_INSIGNE_0423_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_INSIGNE_0423_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_INSIGNE_0423_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\ABR\\CREDITO_CARTOES_INSIGNE_0423.csv",
           row.names=FALSE,quote = FALSE)


## EMISSAO CARTAO ======================================================================================================= 


EMISSAO_CARTOES_INSIGNE_0423_1 <- CREDITO_CARTOES_INSIGNE_0423 %>% 
  filter(is.na(NSERIE)) %>%
  left_join(.,PARTICIPANTES_CAMPANHA %>% 
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1:3,11:14)] 

View(EMISSAO_CARTOES_INSIGNE_0423_1)

range_write("1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",data =EMISSAO_CARTOES_INSIGNE_0423_1,
            sheet = "EMISSAO CARTOES",range = "A3")


## DIGITADOS =======================================================================================================  



CP_INSIGNE_0423_DIGITADOS <-dbGetQuery(con2,"
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

View(CP_INSIGNE_0423_DIGITADOS)

range_write(CP_INSIGNE_0423_DIGITADOS,ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",range = "A:P",sheet="INSIGNE DIGITADOS",reformat = FALSE)  

## =======================================================================================================  


## CHECK PAYMENTS

insigne_pagos_0423 <- read_sheet("",sheet = "INSIGNE") %>% 
  mutate(CPF=as.character(CPF))
view(insigne_pagos_0423)




ALELO_0423_INSIGNE <- ALELO_0423 %>% rename(OBS=6) %>% 
  filter(str_detect(OBS,"INSIGNE")) %>% 
  rename(NSERIE=4)

View(ALELO_0423_INSIGNE)



PAG_ALELO_0423 <- left_join(ALELO_0423_INSIGNE,CARTOES_0423 %>% 
                              rename(NSERIE=5) %>% mutate(as.character(CPF)),by="NSERIE")

View(PAG_ALELO_0423) 


left_join(insigne_pagos_0423 ,PAG_ALELO_0423,by="CPF") %>% View()


