
## PERIODO DE REFERENCIA 011023 - 011223
## CAMPANHA DINIZ MP G51

## SANDRO JAKOSKA

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro")


CP_MP_G45_101123 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS (SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=45),

PED AS (SELECT ID_PEDIDO,
                FISCODIGO1,
                 PEDDTBAIXA,
                  PEDID.CLICODIGO,
                   GCLCODIGO,
                    SETOR,
                     CLINOMEFANT,
                      PEDAUTORIZOU
                       FROM PEDID 
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE PEDDTBAIXA BETWEEN '01.10.2023' AND '01.12.2023' AND PEDSITPED <>'C'),

PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROSITUACAO='A' AND MARCODIGO=103)

SELECT 
 PD.ID_PEDIDO,
   PEDDTBAIXA,
    CLICODIGO,
     GCLCODIGO,
      SETOR,
       PD.PROCODIGO,
        PDPDESCRICAO,
         PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED P ON P.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
INNER JOIN PROD PR ON PD.PROCODIGO=PR.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 
                      
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_MP_G45_101123)



write.csv2(CP_MP_G45_101123,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CP_MP_G45_101123.csv", row.names = FALSE,na="")



CP_MP_G45_101123 %>% group_by(CLICODIGO,PEDAUTORIZOU) %>% tally() %>% View()


CP_MP_G45_101123 %>% group_by(CLICODIGO,PEDAUTORIZOU) %>% tally()  %>% mutate(CPF=PEDAUTORIZOU) %>% glimpse()



left_join(CP_MP_G45_101123 %>% group_by(CLICODIGO) %>% tally() ,PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CLICODIGO") %>% as.data.frame() %>% 

write.csv2(.,file = "C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\CP_MP_G45_PARTICIPANTES_101123.csv", row.names = FALSE,na="")

  
  
  