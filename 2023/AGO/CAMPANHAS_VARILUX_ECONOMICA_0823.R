## CAMPANHA TRANSITIONS ECONOMICA

library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_vlx_0823 <- dbGetQuery(con2, statement = read_file('C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/AGO/VARILUX_ECONOMICA_0823.sql'))

View(query_vlx_0823)

## CAMPANHA VARILUX ECONOMICA G175 =====================================

CP_G175_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(GCLCODIGO==175) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G175_VLX_ECONO_0823)

CP_G175_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_G175_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_G175_VLX_ECONO_0823 <-  paste0("CHILLI BEANS G175 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G175_VLX_ECONO_0823 <- inner_join(CP_G175_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G175_VLX_ECONO_0823)


### pagamentos

PAG_G175_VLX_ECONO_0823 <- LIST_G175_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G175_VLX_ECONO_0823)


View(PAG_G175_VLX_ECONO_0823)


## CAMPANHA VARILUX ECONOMICA G148 =====================================


CP_G148_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G148_VLX_ECONO_0823)

CP_G148_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_0823 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_0823 <- inner_join(CP_G148_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G148_VLX_ECONO_0823)


### pagamentos

PAG_G148_VLX_ECONO_0823 <- LIST_G148_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_0823)


View(PAG_G148_VLX_ECONO_0823)


## CAMPANHA VARILUX ECONOMICA G257 =====================================


CP_G257_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G257_VLX_ECONO_0823)

CP_G257_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_0823 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_0823 <- inner_join(CP_G257_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G257_VLX_ECONO_0823)


### pagamentos

PAG_G257_VLX_ECONO_0823 <- LIST_G257_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_0823)


View(PAG_G257_VLX_ECONO_0823)


## CAMPANHA VARILUX ECONOMICA 986 =====================================

CP_986_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_986_VLX_ECONO_0823)

CP_986_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_0823 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_0823 <- inner_join(CP_986_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_986_VLX_ECONO_0823)


### pagamentos

PAG_986_VLX_ECONO_0823 <- LIST_986_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0823)


View(PAG_986_VLX_ECONO_0823)

## CAMPANHA VARILUX ECONOMICA 3801 =====================================

CP_3801_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==3801) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_3801_VLX_ECONO_0823)

CP_3801_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_3801_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_3801_VLX_ECONO_0823 <-  paste0("VISIONE OPTICA 3801 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_3801_VLX_ECONO_0823 <- inner_join(CP_3801_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_3801_VLX_ECONO_0823)


### pagamentos

PAG_3801_VLX_ECONO_0823 <- LIST_3801_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_3801_VLX_ECONO_0823)


View(PAG_3801_VLX_ECONO_0823)


## CAMPANHA VARILUX ECONOMICA 4469 =====================================

CP_4469_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==4469) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4469_VLX_ECONO_0823)

CP_4469_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_4469_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_4469_VLX_ECONO_0823 <-  paste0("OTICA MARILDS 4469 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_4469_VLX_ECONO_0823 <- inner_join(CP_4469_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4469_VLX_ECONO_0823)


### pagamentos

PAG_4469_VLX_ECONO_0823 <- LIST_4469_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4469_VLX_ECONO_0823)


View(PAG_4469_VLX_ECONO_0823)


## CAMPANHA VARILUX ECONOMICA 291 =====================================

CP_291_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==291) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_291_VLX_ECONO_0823)

CP_291_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_291_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_291_VLX_ECONO_0823 <-  paste0("OTICA LUZ 291 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_291_VLX_ECONO_0823 <- inner_join(CP_291_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_291_VLX_ECONO_0823)


### pagamentos

PAG_291_VLX_ECONO_0823 <- LIST_291_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_291_VLX_ECONO_0823)


View(PAG_291_VLX_ECONO_0823)

## CAMPANHA VARILUX ECONOMICA 4460 =====================================
## NAO ATINGIU O VALR MINIMO
## FALTAM DADOS PARA EMISSAO


CP_4460_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==4460) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4460_VLX_ECONO_0823)

CP_4460_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_4460_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_4460_VLX_ECONO_0823 <-  paste0("OTICA RIO BRANCO 4460 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_4460_VLX_ECONO_0823 <- inner_join(CP_4460_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4460_VLX_ECONO_0823)


### pagamentos

PAG_4460_VLX_ECONO_0823 <- LIST_4460_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4460_VLX_ECONO_0823)


View(PAG_4460_VLX_ECONO_0823)

## CAMPANHA VARILUX ECONOMICA 1923 =====================================


CP_1923_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_1923_VLX_ECONO_0823)

CP_1923_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_ECONO_0823 <-  paste0("RELOJOARIA ART JOIAS 1923 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_ECONO_0823 <- inner_join(CP_1923_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1923_VLX_ECONO_0823)


### pagamentos

PAG_1923_VLX_ECONO_0823 <- LIST_1923_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_ECONO_0823)


View(PAG_1923_VLX_ECONO_0823)

## CAMPANHA VARILUX ECONOMICA 2183 =====================================


CP_2183_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==2183) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_2183_VLX_ECONO_0823)

CP_2183_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_2183_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_2183_VLX_ECONO_0823 <-  paste0("OTICA MADRID 2183 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_2183_VLX_ECONO_0823 <- inner_join(CP_2183_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_2183_VLX_ECONO_0823)


### pagamentos

PAG_2183_VLX_ECONO_0823 <- LIST_2183_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_2183_VLX_ECONO_0823)


View(PAG_2183_VLX_ECONO_0823)

## CAMPANHA VARILUX ECONOMICA 4244 =====================================


CP_4244_VLX_ECONO_0823 <- 
  query_vlx_0823 %>% 
  filter(CLICODIGO==4244) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4244_VLX_ECONO_0823)

CP_4244_VLX_ECONO_0823 %>% summarize(v=sum(VRVENDA))

CP_4244_VLX_ECONO_0823 %>% summarize(v=sum(BONUS))


OBS_4244_VLX_ECONO_0823 <-  paste0("OPTICA ZOLET 4244 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_4244_VLX_ECONO_0823 <- inner_join(CP_4244_VLX_ECONO_0823,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4244_VLX_ECONO_0823)


### pagamentos

PAG_4244_VLX_ECONO_0823 <- LIST_4244_VLX_ECONO_0823 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4244_VLX_ECONO_0823)


View(PAG_4244_VLX_ECONO_0823)


## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_0823 <-  
  rbind(
    PAG_G175_VLX_ECONO_0823,
    PAG_G148_VLX_ECONO_0823,
    PAG_G257_VLX_ECONO_0823,
    PAG_986_VLX_ECONO_0823,
    PAG_3801_VLX_ECONO_0823,
    PAG_4469_VLX_ECONO_0823,
    PAG_291_VLX_ECONO_0823,
    PAG_4460_VLX_ECONO_0823,
    PAG_1923_VLX_ECONO_0823,
    PAG_2183_VLX_ECONO_0823,
    PAG_4244_VLX_ECONO_0823
    
  ) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX ECONOMICA")


View(PAG_VLX_ECONO_0823)

PAG_VARILUX_0823 %>% summarize(v=sum(BONUS))

range_write(PAG_VLX_ECONO_0823,ss="1Fy7pehis2ZCDdy42lF3gVuKmDZhRVSSVF40W8GkKbsI",range = "A53",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## LISTAGEM FINAL   =============================================================================================================         


LIST_VLX_ECONO_0823 <-  rbind(
  LIST_G175_VLX_ECONO_0823,
  LIST_G148_VLX_ECONO_0823,
  LIST_G257_VLX_ECONO_0823,
  LIST_986_VLX_ECONO_0823,
  LIST_3801_VLX_ECONO_0823,
  LIST_4469_VLX_ECONO_0823,
  LIST_291_VLX_ECONO_0823,
  LIST_4460_VLX_ECONO_0823,
  LIST_1923_VLX_ECONO_0823,
  LIST_2183_VLX_ECONO_0823,
  LIST_4244_VLX_ECONO_0823
) 

View(LIST_VLX_ECONO_0823)

LIST_VLX_ECONO_0823%>% summarize(v=sum(BONUS))

range_write(LIST_VLX_ECONO_0823,ss="1Fy7pehis2ZCDdy42lF3gVuKmDZhRVSSVF40W8GkKbsI",
            range = "A:P",sheet="VARILUX ECONOMICA",reformat = FALSE)  


## CREDITO CARTOES ==============================================================================================================

CARTOES_0823 <- get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\AGO\\CARTOES_0823.RData"))



CREDITO_CARTOES_VLX_ECONO_0823 <- left_join(PAG_VLX_ECONO_0823 %>%
                                              mutate(CPF=as.character(CPF)),
                                            CARTOES_0823 %>% filter(STATUS!="Cancelado") %>% 
                                              mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                              mutate(CPF=sub("\\.", '',CPF)) %>% 
                                              mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_VLX_ECONO_0823)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_ECONO_0823_2 <- CREDITO_CARTOES_VLX_ECONO_0823 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_VLX_ECONO_2)


## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_ECONO_0823_3 <- CREDITO_CARTOES_VLX_ECONO_0823_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_VLX_ECONO_0823_3)  

CREDITO_CARTOES_VLX_ECONO_0823_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_VLX_ECONO_0823_3 %>% .[duplicated(.$CPF),]


write.csv2(CREDITO_CARTOES_VLX_ECONO_0823_3,
           file = "C:\\Users\\Repro\\Documents\\R\\ADM\\CAMPANHAS_REPRO\\2023\\AGO\\CREDITO_CARTOES_VLX_ECONO_0823.csv",
           row.names=FALSE,quote = FALSE)


left_join(CREDITO_CARTOES_0823_2,ALELO_0823 %>% rename(NSERIE=`Número de Série`),by="NSERIE") %>%.[,c(-4,-5,-6,-7,-8,-9)] %>% View()






## DOUBLE PAY   =============================================================================================================         


# CAMPANHA PONTOS ESSILOR

campanha_pontos_essilor_q3 <-
  read_sheet("12pd6RnM_QvCQNaCA1EdtRC08fr0onh9oxbxC1FZpOXA",sheet = "PEDIDOS")

pagamentos_duplicados_0823 <- 
  inner_join(LIST_VLX_ECONO_0823,campanha_pontos_essilor_q3 %>% select(ID_PEDIDO),by="ID_PEDIDO") 

range_write(pagamentos_duplicados_0823,ss="1Fy7pehis2ZCDdy42lF3gVuKmDZhRVSSVF40W8GkKbsI",
            range = "A:P",sheet="PAGAMENTOS DUPLICADOS",reformat = FALSE)  


View(campanha_pontos_essilor_q3)





