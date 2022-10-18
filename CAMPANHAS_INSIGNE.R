## CAMPANHAS INSIGNE 
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

##PARTICIPANTES
clientes_insigne <- dbGetQuery(con2,"
                    WITH CLI AS (SELECT C.CLICODIGO,CLINOMEFANT,C.GCLCODIGO,GCLNOME,SETOR
                    FROM CLIEN C
                    LEFT JOIN GRUPOCLI GC ON GC.GCLCODIGO=C.GCLCODIGO
                    LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI E
                    LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA)Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') 
                    ED ON C.CLICODIGO=ED.CLICODIGO)
           
                    SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,GCLNOME,SETOR FROM CLIPROMO C
                    LEFT JOIN CLI CL ON C.CLICODIGO=CL.CLICODIGO
                    WHERE ID_PROMO=18") 

View(clientes_insigne)

## GET CLIENTS FROM CONTROL ======================================================================

CAMPANHAS_2022 <- read_sheet("1Tt7VLY1oHoirHduSaJoj62SlkyZCMVIZ9eHzLAhpqIY",
                                     sheet = 'CAMPANHAS ATIVAS')

View(CAMPANHAS_2022)

#LOJAS
CAMPANHAS_2022 %>% 
   filter(`PRODUTO 1`=='INSIGNE') %>% 
    select(CLICODIGO) %>%
    filter(!is.na(CLICODIGO)) %>%
     left_join(.,clientes_insigne,by="CLICODIGO") %>% 
      View()

#GRUPOS
CAMPANHAS_2022 %>% 
  filter(`PRODUTO 1`=='INSIGNE') %>% 
  select(GCLCODIGO) %>%
  filter(!is.na(GCLCODIGO)) %>%
  left_join(.,clientes_insigne,by="GCLCODIGO") %>% 
  View()


##PARTICIPANTES =========================================================================================


PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                     sheet = 'DADOS')

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

PARTICIPANTES_CAMPANHA %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
  mutate(CLICODIGO=as.numeric(CLICODIGO))  %>% View()



left_join(clientes_insigne,PARTICIPANTES_CAMPANHA %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
            mutate(CLICODIGO=as.numeric(CLICODIGO)) %>% 
            select(CLICODIGO,CPF,`NOME COMPLETO DO PARTICIPANTE`,`Carimbo de data/hora`) ,by="CLICODIGO") %>% 
  left_join(.,BASE_CPF,by="CLICODIGO") %>% 
  View()



