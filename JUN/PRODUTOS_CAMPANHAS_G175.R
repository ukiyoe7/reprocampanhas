## APURAÇÃO CAMPANHAS ABRIL 2022
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)


con2 <- dbConnect(odbc::odbc(), "reproreplica")
## CAMPANHAS INSIGNE 

##PARTICIPANTES
lacrizal <- dbGetQuery(con2,"SELECT PROCODIGO,PRODESCRICAO FROM PRODU 
                    WHERE (LEFT(PROCODIGO,2)='LA' AND 
                          PRODESCRICAO LIKE '%CRIZAL%' AND 
                            PRODESCRICAO NOT LIKE '%TGEN8%') AND PROSITUACAO='A'") %>% mutate(BONUS=5)

View(lacrizal)

lacrizal_trans <- dbGetQuery(con2,"SELECT PROCODIGO,PRODESCRICAO FROM PRODU 
                    WHERE (LEFT(PROCODIGO,2)='LA' AND 
                          PRODESCRICAO LIKE '%CRIZAL%' AND 
                            PRODESCRICAO LIKE '%TGEN8%') AND PROSITUACAO='A'") %>% mutate(BONUS=10)

View(lacrizal_trans)

  trans_scl <- dbGetQuery(con2,"SELECT PROCODIGO,PRODESCRICAO FROM PRODU 
                      WHERE (PRODESCRICAO LIKE '%SCL%') AND GR1CODIGO<>17 AND PROSITUACAO='A'
             AND PROCODIGO2 IS NULL AND PROTIPO NOT IN ('Z','W')") %>% mutate(BONUS=25)
  
  View(trans_scl)
  
  trans_extc <- dbGetQuery(con2,"SELECT PROCODIGO,PRODESCRICAO FROM PRODU 
                      WHERE (PRODESCRICAO LIKE '%EXTRACTIVE%') AND GR1CODIGO<>17 AND PROSITUACAO='A'
             AND PROCODIGO2 IS NULL AND PROTIPO NOT IN ('Z','W')") %>% mutate(BONUS=25)
  
  View(trans_extc)
  
  
  
  prdg175 <- rbind(lacrizal,lacrizal_trans,trans_scl,trans_extc)
  
  
  write_sheet(data=prdg175,ss="1x670F192gfRPpxEx0qM0TORHMujapUk6jz3OYdjW01Y",sheet = "PRODUTOS")
  
  