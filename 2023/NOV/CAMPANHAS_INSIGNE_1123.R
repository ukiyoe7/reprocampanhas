## PERIODO DE REFERENCIA 1123
## SANDRO JAKOSKA

## LOAD =======================================================================================================  


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")



BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## PRODUTOS =======================================================================================================  


save(VALORES_INSIGNE,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\VALORES_INSIGNE.RData")


VALORES_INSIGNE <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\VALORES_INSIGNE.RData"))



## SQL =======================================================================================================  


CP_INSIGNE_1123 <-dbGetQuery(con2,"
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
        PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND
          PEDSITPED<>'C' AND PEDORIGEM='W'),
  
  AUX AS (SELECT PROCODIGO,PROCODIGO2,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2)CHAVE FROM PRODU
  WHERE MARCODIGO=189 AND PROTIPO<>'T')
  
    
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
               SUM(PDPQTDADE) QTD,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
                
    FROM PDPRD PD
     INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
      INNER JOIN AUX A ON PD.PROCODIGO=A.PROCODIGO
        GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2
  ")  %>%
  mutate(PEDAUTORIZOU=as.character(PEDAUTORIZOU))  %>%
  mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  %>% 
  mutate(OBS_SIG=paste0("INSIGNE"," ",if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G', GCLCODIGO))," ",format(floor_date(Sys.Date(),"month"),"%m%y")))


## BONUS ===============================================================


RESULT_INSIGNE_1123 <-
CP_INSIGNE_1123 %>% 
  mutate(LINHA = str_extract(PDPDESCRICAO, "(?<=\\bINSIGNE\\b\\s)\\w+")) %>% 
   left_join(.,VALORES_INSIGNE,by=c("LINHA"="NOME"))


##REMOVE DUPLICATES =====================================================

RESULT_INSIGNE_1123_2 <- RESULT_INSIGNE_1123 %>% filter(CLICODIGO!=213)

RESULT_INSIGNE_1123_3 <- RESULT_INSIGNE_1123_2 %>% filter(CLICODIGO!=151)

RESULT_INSIGNE_1123_4 <- RESULT_INSIGNE_1123_3 %>% filter(CLICODIGO!=849)



## INTERSECTION ========================================================

LIST_INSIGNE_1123 <- left_join(RESULT_INSIGNE_1123_4,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 


## PAY ==================================================================

PAG_INSIGNE_1123 <-  LIST_INSIGNE_1123 %>% filter(nchar(CPF3)==11) %>% group_by(CPF3,OBS_SIG) %>% 
  summarize(BONUS=sum(BONUS)) %>%  rename(CPF=CPF3)


## 151 ==================================================================================

RESULT_INSIGNE_1123_151 <- RESULT_INSIGNE_1123 %>% filter(CLICODIGO==151)


LIST_INSIGNE_1123_151 <- left_join(RESULT_INSIGNE_1123_151,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>%
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%  
  mutate(CPF3=ifelse(nchar(gsub("[^[:alnum:]]", "", PEDAUTORIZOU)) < 6,'00773625941',CPF2))


PAG_INSIGNE_1123_151 <-  LIST_INSIGNE_1123_151 %>% group_by(CPF3,OBS_SIG) %>% 
  summarize(BONUS=sum(BONUS)) %>% rename(CPF=CPF3)


## 213  =============================================================================

RESULT_INSIGNE_1123_213 <- RESULT_INSIGNE_1123 %>% filter(CLICODIGO==213)


LIST_INSIGNE_1123_213 <- left_join(RESULT_INSIGNE_1123_213,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

PAG_INSIGNE_1123_213 <-  LIST_INSIGNE_1123_213 %>% group_by(CPF3,OBS_SIG) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_1123_213 %>% distinct(CPF3) %>% lengths())) %>% 
  rename(CPF=CPF3)


## 849  =============================================================================


RESULT_INSIGNE_1123_849 <- RESULT_INSIGNE_1123 %>% filter(CLICODIGO==849)

## INTERSECT

LIST_INSIGNE_1123_849 <- left_join(CP_INSIGNE_1123_849,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y)  %>% mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) 

LIST_INSIGNE_1123_849 %>%  summarize(BONUS=sum(BONUS))

## PAY

PAG_INSIGNE_1123_849 <-  LIST_INSIGNE_1123_849 %>% group_by(CPF3) %>% 
  summarize(BONUS=sum(BONUS)/(LIST_INSIGNE_1123_849%>% distinct(CPF3) %>% lengths())) %>% rename(CPF=CPF3)



## PAGAMENTOS ======================================================================================================= 


PAG_INSIGNE_1123_ALL <-  rbind( 
  PAG_INSIGNE_1123,
  PAG_INSIGNE_1123_213,
  PAG_INSIGNE_1123_151
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="INSIGNE") %>% rename(OBS=OBS_SIG)

View(PAG_INSIGNE_1123_ALL)


PAG_INSIGNE_1123_ALL %>% summarize(v=sum(BONUS))



## LISTAGEM FINAL  =============================================================================================================         

LIST_INSIGNE_1123_ALL <-  rbind( 
  LIST_INSIGNE_1123,
  LIST_INSIGNE_1123_213,
  LIST_INSIGNE_1123_151
) %>%  .[,c(-12,-16)] %>% rename(CPF=CPF3)

View(LIST_INSIGNE_1123_ALL)


## CREDITO CARTOES ======================================================================================================= 


CARTOES_1123 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CARTOES_1123.RData"))

CREDITO_CARTOES_INSIGNE_1123 <- left_join(PAG_INSIGNE_1123_ALL %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_1123 %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_INSIGNE_1123)


## EXCLUI SEM CARTAO

CREDITO_CARTOES_INSIGNE_1123_2 <- CREDITO_CARTOES_INSIGNE_1123 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_INSIGNE_1123_2)

CREDITO_CARTOES_INSIGNE_1123_2 %>% .[duplicated(.$CPF),] 

CREDITO_CARTOES_INSIGNE_1123_2 %>% n_distinct(.$CPF)



## CRIA BASE DE PAGAMENTO ============================================================

CREDITO_CARTOES_INSIGNE_1123_3 <- CREDITO_CARTOES_INSIGNE_1123_2  %>% 
  .[,c(6,1,3,2)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_1123_3)  

CREDITO_CARTOES_INSIGNE_1123_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_INSIGNE_1123_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_INSIGNE_1123_3,
           file = "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2023\\NOV\\CREDITO_CARTOES_INSIGNE_1123.csv",
           row.names=FALSE,quote = FALSE)


## NAO BONIFICADOS ============================================================

clien <- get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\SQL\\clien.RData"))


NAO_BONIFICADOS_INSIGNE_1123 <-
  
  left_join(CREDITO_CARTOES_INSIGNE_1123_2 <- CREDITO_CARTOES_INSIGNE_1123 %>% 
              filter(!is.na(NSERIE)) %>% 
              filter(PGTO_MINIMO=='N'),BASE_CPF %>% mutate(CPF=as.character(CPF)),by="CPF") %>% 
  left_join(.,clien %>% select(CLICODIGO,SETOR),by="CLICODIGO") %>% 
  .[,c(12,13,1,2,3)] 


View(NAO_BONIFICADOS_INSIGNE_1123)


## EMISSAO CARTAO ======================================================================================================= 


EMISSAO_CARTOES_INSIGNE_1123_1 <- CREDITO_CARTOES_INSIGNE_1123 %>% 
  filter(is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S') %>%  
  left_join(.,PARTICIPANTES_CAMPANHA %>% 
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(13,1:3,14:16)] %>% rename(CLICODIGO=1)

View(EMISSAO_CARTOES_INSIGNE_1123_1)




## DIGITADOS =======================================================================================================  



CP_INSIGNE_1123_DIGITADOS <-dbGetQuery(con2,"
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

View(CP_INSIGNE_1123_DIGITADOS)




