
## CAMPANHA TRANSITIONS ECONOMICA 102023


library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_vlx_1023 <- dbGetQuery(con2, statement = read_file('C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\VARILUX_ECONOMICA_1023.sql'))

View(query_vlx_1023)


## CAMPANHA VARILUX ECONOMICA G148 ATE 31/12/2023 =====================================


CP_G148_VLX_ECONO_1023 <- 
  query_vlx_1023 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G148_VLX_ECONO_1023)

CP_G148_VLX_ECONO_1023 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_1023 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_1023 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_1023 <- inner_join(CP_G148_VLX_ECONO_1023,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G148_VLX_ECONO_1023)


### pagamentos

PAG_G148_VLX_ECONO_1023 <- LIST_G148_VLX_ECONO_1023 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_1023)


View(PAG_G148_VLX_ECONO_1023)


## CAMPANHA VARILUX ECONOMICA G257 ATE 31/12/2023 =====================================


CP_G257_VLX_ECONO_1023 <- 
  query_vlx_1023 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G257_VLX_ECONO_1023)

CP_G257_VLX_ECONO_1023 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_1023 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_1023 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_1023 <- inner_join(CP_G257_VLX_ECONO_1023,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G257_VLX_ECONO_1023)


### pagamentos

PAG_G257_VLX_ECONO_1023 <- LIST_G257_VLX_ECONO_1023 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_1023)


View(PAG_G257_VLX_ECONO_1023)


## CAMPANHA VARILUX ECONOMICA 986 ATE 31/12/2023 =====================================

CP_986_VLX_ECONO_1023 <- 
  query_vlx_1023 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_986_VLX_ECONO_1023)

CP_986_VLX_ECONO_1023 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_1023 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_1023 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_1023 <- inner_join(CP_986_VLX_ECONO_1023,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_986_VLX_ECONO_1023)


### pagamentos

PAG_986_VLX_ECONO_1023 <- LIST_986_VLX_ECONO_1023 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_1023)


View(PAG_986_VLX_ECONO_1023)



## CAMPANHA VARILUX ECONOMICA 1923 ATÉ 31/12/2023 =====================================


CP_1923_VLX_ECONO_1023 <- 
  query_vlx_1023 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_1923_VLX_ECONO_1023)

CP_1923_VLX_ECONO_1023 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_ECONO_1023 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_ECONO_1023 <-  paste0("RELOJOARIA ART JOIAS 1923 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_ECONO_1023 <- inner_join(CP_1923_VLX_ECONO_1023,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1923_VLX_ECONO_1023)


### pagamentos

PAG_1923_VLX_ECONO_1023 <- LIST_1923_VLX_ECONO_1023 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_ECONO_1023)


View(PAG_1923_VLX_ECONO_1023)


## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_1023 <-  
  rbind(
    PAG_G148_VLX_ECONO_1023,
    PAG_G257_VLX_ECONO_1023,
    PAG_986_VLX_ECONO_1023,
    PAG_1923_VLX_ECONO_1023
  ) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX ECONOMICA")


View(PAG_VLX_ECONO_1023)

PAG_VARILUX_1023 %>% summarize(v=sum(BONUS))



## LISTAGEM FINAL   =============================================================================================================         


LIST_VLX_ECONO_1023 <-  rbind(
  LIST_G148_VLX_ECONO_1023,
  LIST_G257_VLX_ECONO_1023,
  LIST_986_VLX_ECONO_1023,
  LIST_1923_VLX_ECONO_1023
) 

View(LIST_VLX_ECONO_1023)

LIST_VLX_ECONO_1023%>% summarize(v=sum(BONUS))

## CREDITO CARTOES ==============================================================================================================



CARTOES_081123 <- get(load("C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\CARTOES_081123.RData"))



CREDITO_CARTOES_VLX_ECONO_1023 <- left_join(PAG_VLX_ECONO_1023 %>%
                                              mutate(CPF=as.character(CPF)),
                                              CARTOES_081123%>% filter(STATUS!="Cancelado") %>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VLX_ECONO_1023)

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



## DOUBLE PAY   =============================================================================================================         


# CAMPANHA PONTOS ESSILOR

campanha_pontos_essilor_q3 <-
  read_sheet("12pd6RnM_QvCQNaCA1EdtRC08fr0onh9oxbxC1FZpOXA",sheet = "PEDIDOS")

pagamentos_duplicados_1023 <- 
  inner_join(LIST_VLX_ECONO_1023,campanha_pontos_essilor_q3 %>% select(ID_PEDIDO),by="ID_PEDIDO") 

range_write(pagamentos_duplicados_1023,ss="1Fy7pehis2ZCDdy42lF3gVuKmDZhRVSSVF40W8GkKbsI",
            range = "A:P",sheet="PAGAMENTOS DUPLICADOS",reformat = FALSE)  


View(campanha_pontos_essilor_q3)





