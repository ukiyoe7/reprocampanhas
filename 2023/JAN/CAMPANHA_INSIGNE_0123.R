## PERIODO DE REFERENCIA 1222
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

CP_INSIGNE_0123 <-dbGetQuery(con2,"
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
    --  PEDDTBAIXA BETWEEN (CURRENT_DATE-1) - EXTRACT(DAY FROM (CURRENT_DATE-1)) + 1 AND 'TODAY'
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


##REMOVE DUPLICATES

CP_INSIGNE_0123_2 <- CP_INSIGNE_0123 %>% filter(CLICODIGO!=213)


## INTERSECTION

LIST_INSIGNE_0123 <- left_join(CP_INSIGNE_0123_2,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>% 
  mutate(CPF3=ifelse(CLICODIGO==959,'03237636932',CPF3)) %>% 
  mutate(OBS=OBS_SIG) 



## PAY

PAG_INSIGNE_0123 <-  LIST_INSIGNE_0123 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## FILTER DUPLICATED

CP_INSIGNE_0123_213 <- CP_INSIGNE_0123 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_0123_213 <- left_join(CP_INSIGNE_0123_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_0123_213 <-  LIST_INSIGNE_0123_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0123_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## ======================================================================================================= 

## PAGAMENTOS FINAL  


PAG_INSIGNE_0123_ALL <-  rbind( 
  PAG_INSIGNE_0123,
  PAG_INSIGNE_0123_213
) 

View(PAG_INSIGNE_0123_ALL)


PAG_INSIGNE_0123_ALL %>% summarize(v=sum(BONUS))


range_write(PAG_INSIGNE_0123_ALL,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",range = "A18",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## =============================================================================================================         

## LISTAGEM FINAL  


LIST_INSIGNE_0123_ALL <-  rbind( 
  LIST_INSIGNE_0123,
  LIST_INSIGNE_0123_213
) 

View(LIST_INSIGNE_0123_ALL)


range_write(LIST_INSIGNE_0123_ALL,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",range = "A:P",sheet="INSIGNE",reformat = FALSE)  

## =======================================================================================================  



## ======================================================================================================= 


## CREDITO CARTOES

CREDITO_CARTOES_INSIGNE_0123 <- left_join(PAG_INSIGNE_0123_ALL %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0123 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_INSIGNE_0123)


## EXCLUI SEM CARTAO

CREDITO_CARTOES_INSIGNE_0123_2 <- CREDITO_CARTOES_INSIGNE_0123 %>% 
  filter(!is.na(NSERIE))

View(CREDITO_CARTOES_INSIGNE_0123_2)

# CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_INSIGNE_0123_3 <- CREDITO_CARTOES_INSIGNE_0123_2  %>% 
  .[,c(4,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_0123_3)  

CREDITO_CARTOES_INSIGNE_0123_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_INSIGNE_0123_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_INSIGNE_0123_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\DEZ\\CREDITO_CARTOES_INSIGNE.csv",
           row.names=FALSE,quote = FALSE)
## ======================================================================================================= 

## EMISSAO CARTAO

ALELO_0123 <- left_join(
  ALELO_CREDITO_0123 %>% 
    rename(NSERIE=4),ALELO_CARTOES_0123 %>% 
    rename(NSERIE=5),by="NSERIE") 

EMISSAO_CARTOES_INSIGNE_0123 <- 
  left_join(
    PAG_INSIGNE_0123_ALL %>% 
      mutate(CPF=as.character(CPF)),ALELO_0123,by="CPF") %>% 
  filter(is.na(NSERIE)) %>% 
  left_join(.,PARTICIPANTES_CAMPANHA %>%  
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(1:3,18:22)] %>% 
  distinct(.$CPF,.keep_all=TRUE) %>%
  .[,-9] 

range_write("",data =EMISSAO_CARTOES_INSIGNE_0123,
            sheet = "DADOS")



## NAO BONIFICAR  


LIST_INSIGNE_0123 %>% filter(is.na(CPF3))

range_write(LIST_INSIGNE_0123 %>% filter(nchar(CPF3)<11 | is.na(CPF3)),ss="1nwmb96gjl_2cRuAGFDzzWFhzxKhaiInStQs6UXvbI6Q",range = "A1",sheet="NAO BONIFICAR",reformat = FALSE)  




## =======================================================================================================  
## PARTICIPANTES

PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                     sheet = 'DADOS') %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
  mutate(CLICODIGO=as.numeric(CLICODIGO))


left_join(PAG_INSIGNE_0123_ALL %>% 
            mutate(CPF=as.character(CPF)),CARTOES_0123,by="CPF") %>% 
  filter(!is.na(NSERIE) & NSERIE!='030000103414415') %>% View()








## =======================================================================================================  

## SQL 

CP_INSIGNE_0123_DIGITADOS <-dbGetQuery(con2,"
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

View(CP_INSIGNE_0123_DIGITADOS)

range_write(CP_INSIGNE_0123_DIGITADOS,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",range = "A:P",sheet="INSIGNE DIGITADOS",reformat = FALSE)  

## =======================================================================================================  


## CHECK PAYMENTS

insigne_pagos_0123 <- read_sheet("",sheet = "INSIGNE") %>% 
  mutate(CPF=as.character(CPF))
view(insigne_pagos_0123)




ALELO_0123_INSIGNE <- ALELO_0123 %>% rename(OBS=6) %>% 
  filter(str_detect(OBS,"INSIGNE")) %>% 
  rename(NSERIE=4)

View(ALELO_0123_INSIGNE)



PAG_ALELO_0123 <- left_join(ALELO_0123_INSIGNE,CARTOES_0123 %>% 
                              rename(NSERIE=5) %>% mutate(as.character(CPF)),by="NSERIE")

View(PAG_ALELO_0123) 


left_join(insigne_pagos_0123 ,PAG_ALELO_0123,by="CPF") %>% View()


