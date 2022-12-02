## PERIODO DE REFERENCIA 1122
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)

con2 <- dbConnect(odbc::odbc(), "reproreplica")

## =======================================================================================================  

## GET CPF

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## =======================================================================================================  

## SQL 

CP_INSIGNE_1122 <-dbGetQuery(con2,"
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
     WHERE 
      PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND 
        CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND 
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
")  %>% mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  

## =======================================================================================================  

## OBS

OBS_SIG <- paste0("INSIGNE"," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


##REMOVE DUPLICATES

CP_INSIGNE_1122_2 <- CP_INSIGNE_1122 %>% filter(CLICODIGO!=213)


## INTERSECTION

LIST_INSIGNE_1122 <- left_join(CP_INSIGNE_1122_2,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>% 
  mutate(OBS=OBS_SIG) 


## PAY

PAG_INSIGNE_1122 <-  LIST_INSIGNE_1122 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## FILTER DUPLICATED

CP_INSIGNE_1122_213 <- CP_INSIGNE_1122 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_1122_213 <- left_join(CP_INSIGNE_1122_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_1122_213 <-  LIST_INSIGNE_1122_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_1122_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## ======================================================================================================= 

## PAGAMENTOS FINAL  


PAG_INSIGNE_1122_ALL <-  rbind( 
  PAG_INSIGNE_1122,
  PAG_INSIGNE_1122_213
) 

View(PAG_INSIGNE_1122_ALL)


PAG_INSIGNE_1122_ALL %>% summarize(v=sum(BONUS))


range_write(PAG_INSIGNE_1122_ALL,ss="1OTNU8i8AU_ot8NDgkPKfAd65-nrKUTDiS5Tqz9il3Vo",range = "A9",sheet="RESUMO",reformat = FALSE)  


## =============================================================================================================         

## LISTAGEM FINAL  


LIST_INSIGNE_1122_ALL <-  rbind( 
  LIST_INSIGNE_1122,
  LIST_INSIGNE_1122_213
) 

View(LIST_INSIGNE_1122_ALL)


range_write(LIST_INSIGNE_1122_ALL,ss="1OTNU8i8AU_ot8NDgkPKfAd65-nrKUTDiS5Tqz9il3Vo",range = "A:P",sheet="INSIGNE",reformat = FALSE)  

## =======================================================================================================  



## ======================================================================================================= 


## 1 DADOS CARTOES

CARTOES_1222  <- CARTOES_1222 %>%  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>%
  mutate(CPF=sub("\\.", '',CPF)) 


## 2 ASSOCIA CARTOS COM PAGAMENTOS

CREDITO_CARTOES_INSIGNE_1122 <- left_join(PAG_INSIGNE_1122_ALL %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_1222,by="CPF") 


## 2 EXCLUIR CANCELADOS 

030000094247944
030000103414415

CREDITO_CARTOES_INSIGNE_1122_2 <- CREDITO_CARTOES_INSIGNE_1122 %>% 
                                 filter(NSERIE!="030000094247944") %>% 
                                  filter(NSERIE!="030000103414415")

View(CREDITO_CARTOES_INSIGNE_1122_2)

CREDITO_CARTOES_INSIGNE_1122_2 %>% .[duplicated(.$CPF),] %>% View()

# EXCLUI SEM CARTAO

CREDITO_CARTOES_INSIGNE_1122_3 <- CREDITO_CARTOES_INSIGNE_1122_2 %>% 
  filter(!is.na(NSERIE))

View(CREDITO_CARTOES_INSIGNE_1122_3)

# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_INSIGNE_1122_4 <- CREDITO_CARTOES_INSIGNE_1122_3 %>% 
  .[,c(5,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_1122_4)  

CREDITO_CARTOES_INSIGNE_1122_4 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_INSIGNE_1122_4 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_INSIGNE_1122_4,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\NOV\\CREDITO_CARTOES_INSIGNE_1122.csv",
           row.names=FALSE,quote = FALSE)


## ======================================================================================================= 

## EMISSAO CARTAO

ALELO_1122 <- left_join(
  ALELO_CREDITO_1122 %>% 
    rename(NSERIE=4),ALELO_CARTOES_1122 %>% 
    rename(NSERIE=5),by="NSERIE") 

EMISSAO_CARTOES_INSIGNE_1122 <- 
  left_join(
    PAG_INSIGNE_1122_ALL %>% 
      mutate(CPF=as.character(CPF)),ALELO_1122,by="CPF") %>% 
  filter(is.na(NSERIE)) %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>%  
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1:3,18:22)] %>% 
  distinct(.$CPF,.keep_all=TRUE) %>%
  .[,-9] 

range_write("1Nt2KOnyrijKRHsTMVQU-KkrqHGGBrYKphp1wcHotjwY",data =EMISSAO_CARTOES_INSIGNE_1122,
            sheet = "DADOS")



## NAO BONIFICAR  


LIST_INSIGNE_1122 %>% filter(is.na(CPF3))

range_write(LIST_INSIGNE_1122 %>% filter(nchar(CPF3)<11 | is.na(CPF3)),ss="1tF9por2Q4mrXJvDm0hh8ta3T3EmSXkGqQ0Xc1K9efmY",range = "A1",sheet="NAO BONIFICAR",reformat = FALSE)  




## =======================================================================================================  
## PARTICIPANTES

PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                     sheet = 'DADOS') %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
  mutate(CLICODIGO=as.numeric(CLICODIGO))


left_join(PAG_INSIGNE_1122_ALL %>% 
            mutate(CPF=as.character(CPF)),CARTOES_1122,by="CPF") %>% 
  filter(!is.na(NSERIE) & NSERIE!='030000103414415') %>% View()








## =======================================================================================================  

## SQL 

CP_INSIGNE_1122_DIGITADOS <-dbGetQuery(con2,"
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

View(CP_INSIGNE_1122_DIGITADOS)

range_write(CP_INSIGNE_1122_DIGITADOS,ss="1OTNU8i8AU_ot8NDgkPKfAd65-nrKUTDiS5Tqz9il3Vo",range = "A:P",sheet="INSIGNE DIGITADOS",reformat = FALSE)  

## =======================================================================================================  


## CHECK PAYMENTS

insigne_pagos_1122 <- read_sheet("",sheet = "INSIGNE") %>% 
  mutate(CPF=as.character(CPF))
view(insigne_pagos_1122)




ALELO_1122_INSIGNE <- ALELO_1122 %>% rename(OBS=6) %>% 
  filter(str_detect(OBS,"INSIGNE")) %>% 
  rename(NSERIE=4)

View(ALELO_1122_INSIGNE)



PAG_ALELO_1122 <- left_join(ALELO_1122_INSIGNE,CARTOES_1122 %>% 
                              rename(NSERIE=5) %>% mutate(as.character(CPF)),by="NSERIE")

View(PAG_ALELO_1122) 


left_join(insigne_pagos_1122 ,PAG_ALELO_1122,by="CPF") %>% View()


