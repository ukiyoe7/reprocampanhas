
## CAMPANHA OTICA ITAÇU G224 
## PERIODO DE REFERENCIA 1023 - 1223

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "repro")


BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## VARILUX_G224_1023 ATE 31.12.2023 =============================================================================================================         


VARILUX_G224_1023 <- dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE GCLCODIGO=224),

         PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
           FROM PEDID 
             INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
               INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
                 WHERE
                    PEDDTBAIXA BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
                          AND PEDSITPED <>'C'),

PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE MARCODIGO=57)

SELECT 
PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
       PROD.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
            CAST(25 AS INTEGER) BONUS
FROM
PDPRD
INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
INNER JOIN PROD ON PDPRD.PROCODIGO=PROD.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2 
 ") %>%
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(VARILUX_G224_1023)

VARILUX_G224_1023 %>% summarize(v=sum(VRVENDA))

VARILUX_G224_1023 %>% summarize(v=sum(BONUS))


OBS_VARILUX_G224_1023 <-  paste0("OTICA ITAÇU G224 TODOS VARILUX ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 


## ASSOCIATE CPF

LIST_VARILUX_G224_1023 <- 
VARILUX_G224_1023 %>% mutate(CPF='00524459924')


View(LIST_VARILUX_G224_1023)


### pagamentos

PAG_VARILUX_G224_1023 <- LIST_VARILUX_G224_1023 %>% 
   group_by(CPF) %>% 
    summarize(BONUS=round(sum(BONUS),2)) %>% 
     mutate(OBS=OBS_VARILUX_G224_1023) %>% 
      mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
       mutate(TIPO="VARILUX")


View(PAG_VARILUX_G224_1023)


## CREDITO CARTOES ==============================================================================================================



CARTOES_081123 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\CARTOES_081123.RData"))



CREDITO_CARTOES_VARILUX_G224_1023 <- left_join(PAG_VARILUX_G224_1023 %>%
                                              mutate(CPF=as.character(CPF)),
                                            CARTOES_081123%>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VARILUX_G224_1023)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_ECONO_1023_2 <- CREDITO_CARTOES_VLX_ECONO_1023 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_VLX_ECONO_1023_2)


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_ECONO_1023_3 <- CREDITO_CARTOES_VLX_ECONO_1023_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_VLX_ECONO_1023_3)  

CREDITO_CARTOES_VLX_ECONO_1023_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VLX_ECONO_1023_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_VLX_ECONO_1023_3,
           file = "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2023\\OUT\\CREDITO_CARTOES_VLX_ECONO_1023.csv",
           row.names=FALSE,quote = FALSE)


left_join(CREDITO_CARTOES_1023_2,ALELO_1023 %>% rename(NSERIE=`Número de Série`),by="NSERIE") %>%.[,c(-4,-5,-6,-7,-8,-9)] %>% View()


