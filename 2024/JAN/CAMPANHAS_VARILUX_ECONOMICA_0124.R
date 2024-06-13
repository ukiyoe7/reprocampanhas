
## CAMPANHA TRANSITIONS ECONOMICA 112023


library(DBI)
library(readr)
library(dplyr)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_vlx_0124 <- dbGetQuery(con2, statement = read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\VARILUX_ECONOMICA_0124.sql'))

View(query_vlx_0124)


## CAMPANHA VARILUX ECONOMICA G148 ATE 31/12/2023 =====================================


CP_G148_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G148_VLX_ECONO_0124)

CP_G148_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_0124 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_0124 <- inner_join(CP_G148_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G148_VLX_ECONO_0124)


### pagamentos

PAG_G148_VLX_ECONO_0124 <- LIST_G148_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_0124)


View(PAG_G148_VLX_ECONO_0124)


## CAMPANHA VARILUX ECONOMICA G257 ATE 31/12/2023 =====================================


CP_G257_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G257_VLX_ECONO_0124)

CP_G257_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_0124 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_0124 <- inner_join(CP_G257_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G257_VLX_ECONO_0124)


### pagamentos

PAG_G257_VLX_ECONO_0124 <- LIST_G257_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_0124)


View(PAG_G257_VLX_ECONO_0124)


## CAMPANHA VARILUX ECONOMICA 986 ATE 31/12/2023 =====================================

CP_986_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_986_VLX_ECONO_0124)

CP_986_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_0124 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_0124 <- inner_join(CP_986_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_986_VLX_ECONO_0124)


### pagamentos

PAG_986_VLX_ECONO_0124 <- LIST_986_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0124)


View(PAG_986_VLX_ECONO_0124)



## CAMPANHA VARILUX ECONOMICA 1923 ATE 31.12.2023 =====================================


CP_1923_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_1923_VLX_ECONO_0124)

CP_1923_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_ECONO_0124 <-  paste0("RELOJOARIA ART JOIAS 1923 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_ECONO_0124 <- inner_join(CP_1923_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1923_VLX_ECONO_0124)


### pagamentos

PAG_1923_VLX_ECONO_0124 <- LIST_1923_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_ECONO_0124)


View(PAG_1923_VLX_ECONO_0124)


## CAMPANHA VARILUX ECONOMICA 1818 ATE 31.12.2023 =====================================


CP_1818_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(CLICODIGO==1818) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_1818_VLX_ECONO_0124)

CP_1818_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_1818_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_1818_VLX_ECONO_0124 <-  paste0("OTICA JOREL 1818 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1818_VLX_ECONO_0124 <- inner_join(CP_1818_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1818_VLX_ECONO_0124)


### pagamentos

PAG_1818_VLX_ECONO_0124 <- LIST_1818_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1818_VLX_ECONO_0124)


View(PAG_1818_VLX_ECONO_0124)

## CAMPANHA VARILUX ECONOMICA 291 ATE 31.12.2023 =====================================


CP_291_VLX_ECONO_0124 <- 
  query_vlx_0124 %>% 
  filter(CLICODIGO==291) %>% 
  mutate(PEDAUTORIZOU='') %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_291_VLX_ECONO_0124)

CP_291_VLX_ECONO_0124 %>% summarize(v=sum(VRVENDA))

CP_291_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))


OBS_291_VLX_ECONO_0124 <-  paste0("OTICA LUZ 291 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_291_VLX_ECONO_0124 <- inner_join(CP_291_VLX_ECONO_0124,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_291_VLX_ECONO_0124)


### pagamentos

PAG_291_VLX_ECONO_0124 <- LIST_291_VLX_ECONO_0124 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_291_VLX_ECONO_0124)


View(PAG_291_VLX_ECONO_0124)


## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_0124 <-  
  rbind(
    PAG_G148_VLX_ECONO_0124,
    PAG_G257_VLX_ECONO_0124,
    PAG_986_VLX_ECONO_0124,
    PAG_1923_VLX_ECONO_0124,
    PAG_1818_VLX_ECONO_0124,
    PAG_291_VLX_ECONO_0124
  ) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX ECONOMICA")


View(PAG_VLX_ECONO_0124)

PAG_VLX_ECONO_0124 %>% summarize(v=sum(BONUS))



## LISTAGEM FINAL   =============================================================================================================         


LIST_VLX_ECONO_0124 <-  rbind(
  LIST_G148_VLX_ECONO_0124,
  LIST_G257_VLX_ECONO_0124,
  LIST_986_VLX_ECONO_0124,
  LIST_1923_VLX_ECONO_0124,
  LIST_1818_VLX_ECONO_0124,
  LIST_291_VLX_ECONO_0124
) 

View(LIST_VLX_ECONO_0124)

LIST_VLX_ECONO_0124%>% summarize(v=sum(BONUS))

## CREDITO CARTOES ==============================================================================================================



CARTOES_260124 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\CARTOES_260124.RData"))



CREDITO_CARTOES_VLX_ECONO_0124 <- left_join(PAG_VLX_ECONO_0124 %>%
                                              mutate(CPF=as.character(CPF)),
                                            CARTOES_260124 %>% filter(STATUS!="Cancelado") %>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VLX_ECONO_0124)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_ECONO_0124_2 <- CREDITO_CARTOES_VLX_ECONO_0124 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_VLX_ECONO_0124_2)


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_ECONO_0124_3 <- CREDITO_CARTOES_VLX_ECONO_0124_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_VLX_ECONO_0124_3)  

CREDITO_CARTOES_VLX_ECONO_0124_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VLX_ECONO_0124_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_VLX_ECONO_0124_3,
           file = "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\JAN\\CREDITO_CARTOES_VLX_ECONO_0124.csv",
           row.names=FALSE,quote = FALSE)


