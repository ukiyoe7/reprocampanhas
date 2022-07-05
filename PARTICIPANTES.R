
## PARTICIPANTES


library(DBI)
library(dplyr)
library(googlesheets4)
con2 <- dbConnect(odbc::odbc(), "reproreplica")
## CAMPANHAS INSIGNE 



PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                     sheet = 'DADOS') %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
                                       mutate(CLICODIGO=as.numeric(CLICODIGO))



## =======================================================================================================  

## SQL 

clicp <-dbGetQuery(con2,"
SELECT DISTINCT CLICODIGO FROM CLIEN WHERE GCLCODIGO=175
") 

View(clicp)


left_join(clicp,PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% .[,c(1,2,3)] %>% View()

range_write("1x670F192gfRPpxEx0qM0TORHMujapUk6jz3OYdjW01Y",data = left_join(clicp,PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% .[,c(1,2,3)])


