## PERIODO DE REFERENCIA 0523
## SANDRO JAKOSKA

## LOAD =======================================================================================================  


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")



BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## SQL =======================================================================================================  


CP_INSIGNE_0623 <-dbGetQuery(con2,"
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
     LEFT JOIN VALORES_PROMO ON PD.PROCODIGO=VALORES_PROMO.PROCODIGO 
      GROUP BY 1,2,3,4,5,6,7,8,9,10 HAVING SUM(PDPQTDADE)>=2
")  %>% 
  mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  


OBS_SIG <- paste0("INSIGNE"," ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y"))


## RESUMO

CP_INSIGNE_0623 %>% summarize(v=sum(VRVENDA))

CP_INSIGNE_0623 %>% filter(CLICODIGO==275) %>% summarize(v=sum(BONUS))

CP_INSIGNE_0623 %>% filter(CLICODIGO==151) %>% summarize(v=sum(BONUS))


CP_INSIGNE_0623 %>% group_by(PDPDESCRICAO) %>% summarize(M=mean(BONUS)) %>% View()



##REMOVE DUPLICATES

CP_INSIGNE_0623_2 <- CP_INSIGNE_0623 %>% filter(CLICODIGO!=213)

CP_INSIGNE_0623_3 <- CP_INSIGNE_0623_2 %>% filter(CLICODIGO!=151)

CP_INSIGNE_0623_4 <- CP_INSIGNE_0623_3 %>% filter(CLICODIGO!=849)

## INTERSECTION

LIST_INSIGNE_0623 <- left_join(CP_INSIGNE_0623_4,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%
  mutate(OBS=OBS_SIG) 


## PAY

PAG_INSIGNE_0623 <-  LIST_INSIGNE_0623 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  rename(CPF=CPF3)


## 151 ==================================================================================

CP_INSIGNE_0623_151 <- CP_INSIGNE_0623 %>% filter(CLICODIGO==151)

View(CP_INSIGNE_0623_151)

LIST_INSIGNE_0623_151 <- left_join(CP_INSIGNE_0623_151,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%  
  mutate(CPF3=ifelse(nchar(gsub("[^[:alnum:]]", "", PEDAUTORIZOU)) < 6,'00773625941',CPF2))

View(LIST_INSIGNE_0623_151)

PAG_INSIGNE_0623_151 <-  LIST_INSIGNE_0623_151 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

View(PAG_INSIGNE_0623_151)

## 213  ========

CP_INSIGNE_0623_213 <- CP_INSIGNE_0623 %>% filter(CLICODIGO==213)

## INTERSECT

LIST_INSIGNE_0623_213 <- left_join(CP_INSIGNE_0623_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

## PAY

PAG_INSIGNE_0623_213 <-  LIST_INSIGNE_0623_213 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0623_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)


## 849  ========

CP_INSIGNE_0623_849 <- CP_INSIGNE_0623 %>% filter(CLICODIGO==849)

## INTERSECT

LIST_INSIGNE_0623_849 <- left_join(CP_INSIGNE_0623_849,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(OBS=OBS_SIG) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

LIST_INSIGNE_0623_849 %>%  summarize(BONUS=sum(BONUS))

## PAY

PAG_INSIGNE_0623_849 <-  LIST_INSIGNE_0623_849 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_0623_213 %>% distinct(CPF3) %>% lengths())) %>% 
  mutate(OBS=OBS_SIG) %>% rename(CPF=CPF3)

## PAGAMENTOS ======================================================================================================= 


PAG_INSIGNE_0623_ALL <-  rbind( 
  PAG_INSIGNE_0623,
  PAG_INSIGNE_0623_213,
  PAG_INSIGNE_0623_151,
  PAG_INSIGNE_0623_849
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="INSIGNE")

View(PAG_INSIGNE_0623_ALL)


PAG_INSIGNE_0623_ALL %>% summarize(v=sum(BONUS))


range_write(PAG_INSIGNE_0623_ALL,ss="19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",range = "A15",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## LISTAGEM FINAL  =============================================================================================================         

LIST_INSIGNE_0623_ALL <-  rbind( 
  LIST_INSIGNE_0623,
  LIST_INSIGNE_0623_213,
  LIST_INSIGNE_0623_151
) 

View(LIST_INSIGNE_0623_ALL)


range_write(LIST_INSIGNE_0623_ALL ,ss="19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",range = "A:P",sheet="INSIGNE",reformat = FALSE)  


range_write(LIST_INSIGNE_0623_ALL %>% filter(CLICODIGO==151) ,ss="19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",range = "A:P",sheet="DADOS",reformat = FALSE)  


## CREDITO CARTOES ======================================================================================================= 


CREDITO_CARTOES_INSIGNE_0623 <- left_join(PAG_INSIGNE_0623_ALL %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_0706 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_INSIGNE_0623)


## EXCLUI SEM CARTAO

CREDITO_CARTOES_INSIGNE_0623_2 <- CREDITO_CARTOES_INSIGNE_0623 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_INSIGNE_0623_2)

CREDITO_CARTOES_INSIGNE_0623_2 %>% .[duplicated(.$CPF),] 

CREDITO_CARTOES_INSIGNE_0623_2 %>% n_distinct(.$CPF)



## CRIA BASE DE PAGAMENTO ============================================================

CREDITO_CARTOES_INSIGNE_0623_3 <- CREDITO_CARTOES_INSIGNE_0623_2  %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_0623_3)  

CREDITO_CARTOES_INSIGNE_0623_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_INSIGNE_0623_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_INSIGNE_0623_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\JUN\\CREDITO_CARTOES_INSIGNE_0623.csv",
           row.names=FALSE,quote = FALSE)


## NAO BONIFICADOS ============================================================

NAO_BONIFICADOS_INSIGNE_0623 <-
  
  left_join(CREDITO_CARTOES_INSIGNE_0623_2 <- CREDITO_CARTOES_INSIGNE_0623 %>% 
              filter(!is.na(NSERIE)) %>% 
              filter(PGTO_MINIMO=='N'),BASE_CPF %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  left_join(.,cli %>% select(CLICODIGO,SETOR),by="CLICODIGO") %>% 
  .[,c(12,13,1,2,3)] 


range_write("19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",data =NAO_BONIFICADOS_INSIGNE_0623,
            sheet = "NAO BONIFICADOS",range = "A1")


## EMISSAO CARTAO ======================================================================================================= 


EMISSAO_CARTOES_INSIGNE_0623_1 <- CREDITO_CARTOES_INSIGNE_0623 %>% 
  filter(is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S') %>%  
  left_join(.,PARTICIPANTES_CAMPANHA %>% 
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(13,1:3,14:16)] %>% rename(CLICODIGO=1)

View(EMISSAO_CARTOES_INSIGNE_0623_1)

range_write("1a-u9ZH9RLQnQGrCUHzDRpao0EEuzhfSlBfgHAn27ry8",data =EMISSAO_CARTOES_INSIGNE_0623_1,
            sheet = "EMISSAO CARTOES",range = "A3")


## DIGITADOS =======================================================================================================  



CP_INSIGNE_0623_DIGITADOS <-dbGetQuery(con2,"
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

View(CP_INSIGNE_0623_DIGITADOS)

range_write(CP_INSIGNE_0623_DIGITADOS,ss="1a-u9ZH9RLQnQGrCUHzDRpao0EEuzhfSlBfgHAn27ry8",range = "A:P",sheet="INSIGNE DIGITADOS",reformat = FALSE)  

##  =======================================================================================================  







## CHECK PAYMENTS ============================================================

insigne_pagos_0623 <- read_sheet("",sheet = "INSIGNE") %>% 
  mutate(CPF=as.character(CPF))
view(insigne_pagos_0623)




ALELO_0623_INSIGNE <- ALELO_0623 %>% rename(OBS=6) %>% 
  filter(str_detect(OBS,"INSIGNE")) %>% 
  rename(NSERIE=4)

View(ALELO_0623_INSIGNE)



PAG_ALELO_0623 <- left_join(ALELO_0623_INSIGNE,CARTOES_0623 %>% 
                              rename(NSERIE=5) %>% mutate(as.character(CPF)),by="NSERIE")

View(PAG_ALELO_0623) 


left_join(insigne_pagos_0623 ,PAG_ALELO_0623,by="CPF") %>% View()


