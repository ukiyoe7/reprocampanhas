
## APURAÇÃO CAMPANHA 1744

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


## CAMPANHA MREPRO ============================================================


CAMPANHA_MPREPRO_1744 <- dbGetQuery(con2, statement = read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CAMPANHA_MPREPRO_1744.sql')) 

View(CAMPANHA_MPREPRO_1744)



## CAMPANHA INSIGNE ============================================================


CP_INSIGNE_1744 <-dbGetQuery(con2,"
  WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
    FROM CLIEN C
     INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
      LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
        WHERE CLICLIENTE='S' AND C.CLICODIGO=1744),
    
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
  
         PEDDTBAIXA BETWEEN
         
         '01.08.2023' AND 
         '31.10.2023'
       
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


View(CP_INSIGNE_1744)


OBS_SIG_4699 <- paste0("OJO 4699 " ,"RETROATIVA INSIGNE "," ",format(floor_date(Sys.Date(),"month"),"%m%y"))


## JOIN CPF

LIST_SIG_4699  <- inner_join(CP_INSIGNE_4699,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF1=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  .[,-12] %>% 
  rename(.,"CPF"="CPF2") %>% 
  mutate(OBS=OBS_SIG_4699) %>% mutate(CPF=as.character(CPF))

View(LIST_SIG_4699)

write.csv2(LIST_SIG_4699 ,file = "LIST_SIG_4699.csv",row.names = FALSE,na="")
