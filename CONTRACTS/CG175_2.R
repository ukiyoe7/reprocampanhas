## CHILI BEANS G175 STYLE COLORS

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")

## =============================================================================================================         

# CHILI BEANS G175

CP_G175_2_0722 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=175),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE PEDDTBAIXA
BETWEEN '01.06.2022' AND '31.07.2022'
AND PEDSITPED <>'C'),

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) CHAVE FROM PRODU WHERE 
PROCODIGO2=IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) AND PRODESCRICAO LIKE '%CRIZAL%' AND PRODESCRICAO NOT LIKE '%TGEN8%' AND GR1CODIGO=2),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) CHAVE FROM PRODU WHERE 
PROCODIGO2=IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) AND PRODESCRICAO LIKE '%TGEN8%' AND GR1CODIGO=2),

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) CHAVE FROM PRODU WHERE PROCODIGO2=IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) AND PRODESCRICAO LIKE '%SCL%'),

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2) CHAVE FROM PRODU WHERE PRODESCRICAO LIKE '%EXTRACTIVE%')

--- PROD1

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD1.CHAVE,
        (SELECT DISTINCT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE)DESCRICAO,
          PEDAUTORIZOU,
           SUM(PDPQTDADE)QTD,
            SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
             CAST(5 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD1 ON PDPRD.PROCODIGO=PROD1.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 UNION

--- PROD2

SELECT
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD2.CHAVE,
        (SELECT DISTINCT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE)DESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(10 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD2 ON PDPRD.PROCODIGO=PROD2.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8  HAVING SUM(PDPQTDADE)>=2 UNION

--- PROD3

SELECT
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD3.CHAVE,
        (SELECT DISTINCT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE)DESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(25 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD3 ON PDPRD.PROCODIGO=PROD3.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8  HAVING SUM(PDPQTDADE)>=2 UNION

--- PROD4

SELECT
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD4.CHAVE,
        (SELECT DISTINCT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE)DESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(25 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD4 ON PDPRD.PROCODIGO=PROD4.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2



 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G175_2_0722)

CP_G175_2_0722 %>% summarize(v=sum(VRVENDA))

CP_G175_2_0722 %>% summarize(v=sum(BONUS))

CP_G175_2_0722 %>% group_by(CLICODIGO) %>% summarize(v=sum(BONUS))


OBS_G175 <- paste0("CHILI BEANS ","G175 ",format(floor_date(Sys.Date()-months(1),"month"),"%m/%y")) 


View(LIST_G175_0722)

CP_G175_2_0722 %>% .[duplicated(.$ID_PEDIDO),]


range_write(CP_G175_2_0722,ss="13zAIjdDXf-s_w7jTNdXcABN9xQ3pClWQSJH9UMA1l6E",range = "A:P",sheet="DADOS",reformat = FALSE)  


### PAY

PAG_G175_2_0722 <- CP_G175_2_0722 %>% group_by(CLICODIGO) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G175) 

View(PAG_G175_2_0722)

range_write(PAG_G175_2_0722,ss="13zAIjdDXf-s_w7jTNdXcABN9xQ3pClWQSJH9UMA1l6E",
            range = "A:P",sheet="RESUMO PAGAMENTOS",reformat = FALSE)  



PAG_4253_0622 %>% summarize(v=sum(BONUS))


