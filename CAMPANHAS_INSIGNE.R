## APURAÇÃO CAMPANHAS ABRIL 2022
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")
## CAMPANHAS INSIGNE 

##PARTICIPANTES
clientes_insigne <- dbGetQuery(con2,"
                    WITH CLI AS (SELECT C.CLICODIGO,CLINOMEFANT,C.GCLCODIGO,GCLNOME,SETOR
                    FROM CLIEN C
                    LEFT JOIN GRUPOCLI GC ON GC.GCLCODIGO=C.GCLCODIGO
                    LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI E
                    LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA)Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') ED ON C.CLICODIGO=ED.CLICODIGO)
           
                    SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,GCLNOME,SETOR FROM CLIPROMO C
                    LEFT JOIN CLI CL ON C.CLICODIGO=CL.CLICODIGO
                    WHERE ID_PROMO=18") 

View(clientes_insigne)


range_write("1TO0CM5rnZgkesQUG444H8jqH2WRpUaVgvKilV-7uttY",data=clientes_insigne,sheet = "DADOS",
            range = "A1",reformat = FALSE)


PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                     sheet = 'DADOS')

PARTICIPANTES_CAMPANHA %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
  mutate(CLICODIGO=as.numeric(CLICODIGO))  %>% View()



left_join(clientes_insigne,PARTICIPANTES_CAMPANHA %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
            mutate(CLICODIGO=as.numeric(CLICODIGO)) %>% 
            select(CLICODIGO,CPF,`NOME COMPLETO DO PARTICIPANTE`) ,by="CLICODIGO") %>% View()


dbGetQuery(con2,"SELECT * FROM CLIPROMO") 

