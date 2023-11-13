## PERIODO DE REFERENCIA 0910_2023
## SANDRO JAKOSKA

## LOAD =======================================================================================================  


library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")



BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

## SQL =======================================================================================================  


CP_INSIGNE_SETOR4_1023 <-dbGetQuery(con2,"
  WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
    FROM CLIEN C
     INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
      LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
        WHERE CLICLIENTE='S' AND C.CLICODIGO IN (4588,345)),
    
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
  
         PEDDTBAIXA BETWEEN '01.09.2023' AND '31.10.2023'
       
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


OBS_SIG <- paste0("INSIGNE SETOR 4"," ","SET OUT 23")


## RESUMO

CP_INSIGNE_SETOR4_1023 %>% summarize(v=sum(VRVENDA))



## INTERSECTION

LIST_INSIGNE_SETOR4_1023 <- left_join(CP_INSIGNE_SETOR4_1023,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%
  mutate(OBS=OBS_SIG) %>% select(-CPF,-CPF2) %>% rename(CPF=CPF3) %>% mutate(CPF=as.character(CPF))

View(LIST_INSIGNE_SETOR4_1023)


write.csv2(LIST_INSIGNE_SETOR4_1023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_INSIGNE_SETOR4_1023.csv" ,row.names = FALSE )


## PAY

PAG_INSIGNE_SETOR4_1023 <-  LIST_INSIGNE_SETOR4_1023 %>% 
  group_by(CLICODIGO,CPF) %>% 
  summarize(BONUS=sum(BONUS)) %>% 
  mutate(OBS=OBS_SIG) %>%  
  mutate(CPF=as.character(CPF))


View(PAG_INSIGNE_SETOR4_1023)


write.csv2(PAG_INSIGNE_SETOR4_1023,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\PAG_INSIGNE_SETOR4_1023.csv",row.names = FALSE )



