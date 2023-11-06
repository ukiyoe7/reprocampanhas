## CAMPANHA TRANSITIONS ECONOMICA

library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_vlx_0923 <- dbGetQuery(con2, statement = read_file('C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\SET\\VARILUX_ECONOMICA_0923.sql'))

View(query_vlx_0923)

## CAMPANHA VARILUX ECONOMICA G175 =====================================

CP_G175_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(GCLCODIGO==175) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G175_VLX_ECONO_0923)

CP_G175_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_G175_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_G175_VLX_ECONO_0923 <-  paste0("CHILLI BEANS G175 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_G175_VLX_ECONO_0923 <- inner_join(CP_G175_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G175_VLX_ECONO_0923)


### pagamentos

PAG_G175_VLX_ECONO_0923 <- LIST_G175_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G175_VLX_ECONO_0923)


View(PAG_G175_VLX_ECONO_0923)


## CAMPANHA VARILUX ECONOMICA G148 =====================================


CP_G148_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G148_VLX_ECONO_0923)

CP_G148_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_0923 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_0923 <- inner_join(CP_G148_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G148_VLX_ECONO_0923)


### pagamentos

PAG_G148_VLX_ECONO_0923 <- LIST_G148_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_0923)


View(PAG_G148_VLX_ECONO_0923)


## CAMPANHA VARILUX ECONOMICA G257 =====================================


CP_G257_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G257_VLX_ECONO_0923)

CP_G257_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_0923 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_0923 <- inner_join(CP_G257_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G257_VLX_ECONO_0923)


### pagamentos

PAG_G257_VLX_ECONO_0923 <- LIST_G257_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_0923)


View(PAG_G257_VLX_ECONO_0923)


## CAMPANHA VARILUX ECONOMICA 986 =====================================

CP_986_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_986_VLX_ECONO_0923)

CP_986_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_0923 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_0923 <- inner_join(CP_986_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_986_VLX_ECONO_0923)


### pagamentos

PAG_986_VLX_ECONO_0923 <- LIST_986_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0923)


View(PAG_986_VLX_ECONO_0923)


## CAMPANHA VARILUX ECONOMICA 4469 =====================================

CP_4469_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==4469) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4469_VLX_ECONO_0923)

CP_4469_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_4469_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_4469_VLX_ECONO_0923 <-  paste0("OTICA MARILDS 4469 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_4469_VLX_ECONO_0923 <- inner_join(CP_4469_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4469_VLX_ECONO_0923)


### pagamentos

PAG_4469_VLX_ECONO_0923 <- LIST_4469_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4469_VLX_ECONO_0923)


View(PAG_4469_VLX_ECONO_0923)


## CAMPANHA VARILUX ECONOMICA 291 =====================================

CP_291_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==291) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_291_VLX_ECONO_0923)

CP_291_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_291_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_291_VLX_ECONO_0923 <-  paste0("OTICA LUZ 291 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_291_VLX_ECONO_0923 <- inner_join(CP_291_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_291_VLX_ECONO_0923)


### pagamentos

PAG_291_VLX_ECONO_0923 <- LIST_291_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_291_VLX_ECONO_0923)


View(PAG_291_VLX_ECONO_0923)

## CAMPANHA VARILUX ECONOMICA 4460 =====================================
## NAO ATINGIU O VALR MINIMO
## FALTAM DADOS PARA EMISSAO


CP_4460_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==4460) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4460_VLX_ECONO_0923)

CP_4460_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_4460_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_4460_VLX_ECONO_0923 <-  paste0("OTICA RIO BRANCO 4460 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_4460_VLX_ECONO_0923 <- inner_join(CP_4460_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4460_VLX_ECONO_0923)


### pagamentos

PAG_4460_VLX_ECONO_0923 <- LIST_4460_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4460_VLX_ECONO_0923)


View(PAG_4460_VLX_ECONO_0923)

## CAMPANHA VARILUX ECONOMICA 1923 =====================================


CP_1923_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_1923_VLX_ECONO_0923)

CP_1923_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_ECONO_0923 <-  paste0("RELOJOARIA ART JOIAS 1923 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_ECONO_0923 <- inner_join(CP_1923_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1923_VLX_ECONO_0923)


### pagamentos

PAG_1923_VLX_ECONO_0923 <- LIST_1923_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_ECONO_0923)


View(PAG_1923_VLX_ECONO_0923)

## CAMPANHA VARILUX ECONOMICA 2183 =====================================


CP_2183_VLX_ECONO_0923 <- 
  query_vlx_0923 %>% 
  filter(CLICODIGO==2183) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_2183_VLX_ECONO_0923)

CP_2183_VLX_ECONO_0923 %>% summarize(v=sum(VRVENDA))

CP_2183_VLX_ECONO_0923 %>% summarize(v=sum(BONUS))


OBS_2183_VLX_ECONO_0923 <-  paste0("OTICA MADRID 2183 VARILUX ECONOMICA ",format(floor_date(Sys.Date(),"month"),"%m%y")) 

## JOIN CPF

LIST_2183_VLX_ECONO_0923 <- inner_join(CP_2183_VLX_ECONO_0923,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_2183_VLX_ECONO_0923)


### pagamentos

PAG_2183_VLX_ECONO_0923 <- LIST_2183_VLX_ECONO_0923 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_2183_VLX_ECONO_0923)


View(PAG_2183_VLX_ECONO_0923)


## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_0923 <-  
  rbind(
    PAG_G175_VLX_ECONO_0923,
    PAG_G148_VLX_ECONO_0923,
    PAG_G257_VLX_ECONO_0923,
    PAG_986_VLX_ECONO_0923,
    PAG_4469_VLX_ECONO_0923,
    PAG_291_VLX_ECONO_0923,
    PAG_4460_VLX_ECONO_0923,
    PAG_1923_VLX_ECONO_0923,
    PAG_2183_VLX_ECONO_0923
    
  ) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX ECONOMICA")


View(PAG_VLX_ECONO_0923)

PAG_VARILUX_0923 %>% summarize(v=sum(BONUS))

range_write(PAG_VLX_ECONO_0923,ss="1vnn7_8_PwZwvG8AeTg6Oq1rJosDnl0zNM_yQ6oCweM4",range = "A48",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## LISTAGEM FINAL   =============================================================================================================         


LIST_VLX_ECONO_0923 <-  rbind(
  LIST_G175_VLX_ECONO_0923,
  LIST_G148_VLX_ECONO_0923,
  LIST_G257_VLX_ECONO_0923,
  LIST_986_VLX_ECONO_0923,
  LIST_4469_VLX_ECONO_0923,
  LIST_291_VLX_ECONO_0923,
  LIST_4460_VLX_ECONO_0923,
  LIST_1923_VLX_ECONO_0923,
  LIST_2183_VLX_ECONO_0923
) 

View(LIST_VLX_ECONO_0923)

LIST_VLX_ECONO_0923%>% summarize(v=sum(BONUS))

range_write(LIST_VLX_ECONO_0923,ss="1vnn7_8_PwZwvG8AeTg6Oq1rJosDnl0zNM_yQ6oCweM4",
            range = "A:P",sheet="VARILUX ECONOMICA",reformat = FALSE)  


## CREDITO CARTOES ==============================================================================================================

CARTOES_0923 <- get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\AGO\\CARTOES_0923.RData"))



CREDITO_CARTOES_VLX_ECONO_0923 <- left_join(PAG_VLX_ECONO_0923 %>%
                                              mutate(CPF=as.character(CPF)),
                                            CARTOES_0823 %>% filter(STATUS!="Cancelado") %>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VLX_ECONO_0923)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_ECONO_0923_2 <- CREDITO_CARTOES_VLX_ECONO_0923 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_VLX_ECONO_2)


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_ECONO_0923_3 <- CREDITO_CARTOES_VLX_ECONO_0923_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_VLX_ECONO_0923_3)  

CREDITO_CARTOES_VLX_ECONO_0923_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VLX_ECONO_0923_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_VLX_ECONO_0923_3,
           file = "C:\\Users\\Repro\\One Drive Comunicacao\\OneDrive - Luxottica Group S.p.A\\CAMPANHAS ALELO\\2023\\SET\\CREDITO_CARTOES_VLX_ECONO_0923.csv",
           row.names=FALSE,quote = FALSE)


left_join(CREDITO_CARTOES_0923_2,ALELO_0923 %>% rename(NSERIE=`Número de Série`),by="NSERIE") %>%.[,c(-4,-5,-6,-7,-8,-9)] %>% View()



## DOUBLE PAY   =============================================================================================================         


# CAMPANHA PONTOS ESSILOR

campanha_pontos_essilor_q3 <-
  read_sheet("12pd6RnM_QvCQNaCA1EdtRC08fr0onh9oxbxC1FZpOXA",sheet = "PEDIDOS")

pagamentos_duplicados_0923 <- 
  inner_join(LIST_VLX_ECONO_0923,campanha_pontos_essilor_q3 %>% select(ID_PEDIDO),by="ID_PEDIDO") 

range_write(pagamentos_duplicados_0923,ss="1Fy7pehis2ZCDdy42lF3gVuKmDZhRVSSVF40W8GkKbsI",
            range = "A:P",sheet="PAGAMENTOS DUPLICADOS",reformat = FALSE)  


View(campanha_pontos_essilor_q3)





