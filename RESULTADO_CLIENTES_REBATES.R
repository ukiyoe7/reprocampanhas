
library(DBI)
library(tidyverse)
library(lubridate)

con2 <- dbConnect(odbc::odbc(), "reproreplica")

rebates <- dbGetQuery(con2,"
    
   WITH CLI AS (SELECT DISTINCT C.CLICODIGO,
                         CLINOMEFANT,
                          IIF(C.GCLCODIGO IS NULL,C.CLICODIGO || ' ' || 
                           CLINOMEFANT,'G' || C.GCLCODIGO || ' ' || GCLNOME) CLIENTE,
                            C.GCLCODIGO,
                             SETOR
                              FROM CLIEN C
                               LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                LEFT JOIN (SELECT CLICODIGO,
                                                E.ZOCODIGO,
                                                  ZODESCRICAO SETOR,
                                                   ENDCODIGO FROM ENDCLI E
                                                    LEFT JOIN (SELECT ZOCODIGO,
                                                                       ZODESCRICAO 
                                                                        FROM ZONA WHERE ZOCODIGO 
                                                                         IN (20,21,22,23,24,25,28))Z ON 
                                                                          E.ZOCODIGO=Z.ZOCODIGO 
                                                                           WHERE ENDFAT='S')A ON 
                                                                            C.CLICODIGO=A.CLICODIGO
                                                                             WHERE CLICLIENTE='S' AND
                                                                              C.CLICODIGO IN (849,157,352,2157)),
                               
FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
    
    PED AS (SELECT ID_PEDIDO,
                      PEDDTBAIXA,
                       CLIENTE
                            FROM PEDID P
                             LEFT JOIN FIS ON P.FISCODIGO1=FIS.FISCODIGO
                              INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO 
                               WHERE PEDDTEMIS BETWEEN '01.01.2022' AND '31.08.2022' 
                                AND PEDSITPED<>'C') 
    
    
      SELECT
                PEDDTBAIXA,
                 CLIENTE,
                          SUM(PDPQTDADE)QTD,
                           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA 
                            FROM PDPRD PD
                             INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                              GROUP BY 1,2")  


View(rebates)


rebates %>% 
  group_by(MES=floor_date(PEDDTBAIXA,"month")) %>% 
  summarize(v=sum(VRVENDA)) %>% mutate(VAR=((v/lag(v))-1)*100) %>% View()

rebates %>% 
   group_by(CLIENTE,MES=floor_date(PEDDTBAIXA,"month")) %>% 
     summarize(v=sum(VRVENDA)) %>% mutate(VAR=((v/lag(v))-1)*100) %>% View()






