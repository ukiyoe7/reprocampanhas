## CAMPANHA MARCAS REPRO

library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_marcas_repro_0723 <- dbGetQuery(con2, statement = read_file('C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/JUL/MARCAS_REPRO_0723.sql'))

View(query_marcas_repro_0723)

## G339 OTICA MED =====================================

CP_G339_MRP_0723 <- 
  query_marcas_repro_0723 %>% 
  filter(GCLCODIGO==339) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G339_MRP_0723)

CP_G339_MRP_0723 %>% summarize(v=sum(VRVENDA))

CP_G339_MRP_0723 %>% summarize(v=sum(BONUS))


OBS_G339_MRP_0723 <-  paste0("OTICA MED G339 MARCAS REPRO ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G339_MRP_0723 <- inner_join(CP_G339_MRP_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G339_MRP_0723)


### pagamentos

PAG_G339_MRP_0723 <- LIST_G339_MRP_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G339_MRP_0723)


View(PAG_G339_MRP_0723)





## PAGAMENTOS  =============================================================================================================       


PAG_MRP_0723 <-  
  rbind(
    PAG_G339_MRP_0723
  ) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="MARCAS REPRO")


View(PAG_MRP_0723)

PAG_MRP_0723 %>% summarize(v=sum(BONUS))

range_write(PAG_MRP_0723,ss="1bgKkW2aReox24kSi0FvsUQD-b0rOk9RkCXrb66hFhpM",range = "",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## LISTAGEM FINAL   =============================================================================================================         


LIST_MRP_0723 <-  rbind(
  LIST_G339_MRP_0723
) 


View(LIST_MRP_0723)

LIST_MRP_0723 %>% summarize(v=sum(BONUS))

range_write(LIST_G339_MRP_0723,ss="1bgKkW2aReox24kSi0FvsUQD-b0rOk9RkCXrb66hFhpM",
            range = "A:P",sheet="MARCAS REPRO",reformat = FALSE)  



