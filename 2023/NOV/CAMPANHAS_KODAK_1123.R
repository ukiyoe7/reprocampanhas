
## CAMPANHA TRANSITIONS ECONOMICA 112023


library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_kdk_1123 <- dbGetQuery(con2, statement = read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\KODAK_1123.sql'))

View(query_kdk_1123)


## CAMPANHA KODAK =====================================


CP_KDK_20ANOS_1123 <- 
  query_kdk_1123 %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_KDK_1123)

CP_KDK_20ANOS_1123 %>% summarize(v=sum(VRVENDA))

CP_KDK_20ANOS_1123 %>% summarize(v=sum(BONUS))


OBS_KDK_20ANOS_1123 <-  paste0("CAMPANHA KODAK 20 ANOS ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_KDK_20ANOS_1123 <- left_join(CP_KDK_20ANOS_1123,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y)  %>% mutate(OBS=OBS_KDK_20ANOS_1123) %>% select(- CPF.x) %>% 
   mutate(CPF=as.character(CPF))


View(LIST_KDK_20ANOS_1123)

write.csv2(LIST_KDK_20ANOS_1123,file = "C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/LIST_KDK_20ANOS_1123.csv" ,row.names = FALSE,na="")


### pagamentos

PAG_KDK_20ANOS_1123 <- LIST_KDK_20ANOS_1123 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_KDK_20ANOS_1123)


View(PAG_KDK_20ANOS_1123)



LIST_KDK_20ANOS_1123_PARTICIPANTES <-
left_join(LIST_KDK_20ANOS_1123 %>% distinct(CLICODIGO,GCLCODIGO) %>% as.data.frame(),
          PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% .[,c(1,2,4,5)] %>% 
            mutate(CPF=as.character(CPF)) %>% as.data.frame()

View(LIST_KDK_20ANOS_1123_PARTICIPANTES)


write.csv2(LIST_KDK_20ANOS_1123_PARTICIPANTES,file = "C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/NOV/LIST_KDK_20ANOS_1123_PARTICIPANTES.csv" ,row.names = FALSE,na="")
