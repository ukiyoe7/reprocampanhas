
## CAMPANHA MARCA REPRO 

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


## CAMPANHA MREPRO ============================================================


CAMPANHA_MREPRO_0124 <- dbGetQuery(con2, statement = read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\MARCA_REPRO_0124.sql')) 


## 1744 OTICA CARVALHO 05.02.2024 - 01.05.2024 =====================================


CP_1744_MREPRO_0124 <- 
  CAMPANHA_MREPRO_0124 %>% 
  filter(CLICODIGO==1744) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_1744_MREPRO_0124 %>% summarize(v=sum(VRVENDA))

CP_1744_MREPRO_0124%>% summarize(v=sum(BONUS))


OBS_1744_MREPRO_0124 <-  paste0("JOALHERIA E OTICA CARVALHO 1744 MARCA REPRO",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1744_MREPRO_0124 <- inner_join(CP_1744_MREPRO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1744_MREPRO_0124)


### pagamentos

PAG_1744_MREPRO_0124 <- LIST_1744_MREPRO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1744_MREPRO_0124)


View(PAG_1744_MREPRO_0124)


## 4825 OJO 05.02.2024 - 01.05.2024 =====================================


CP_4825_MREPRO_0124 <- 
  CAMPANHA_MREPRO_0124 %>% 
  filter(CLICODIGO==4825) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_4825_MREPRO_0124 %>% summarize(v=sum(VRVENDA))

CP_4825_MREPRO_0124%>% summarize(v=sum(BONUS))


OBS_4825_MREPRO_0124 <-  paste0("OJO MR RAY 4825 MARCA REPRO",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_4825_MREPRO_0124 <- inner_join(CP_4825_MREPRO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4825_MREPRO_0124)


### pagamentos

PAG_4825_MREPRO_0124 <- LIST_4825_MREPRO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4825_MREPRO_0124)


View(PAG_4825_MREPRO_0124)
