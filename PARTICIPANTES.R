## PARTICIPANTES CAMPANHAS


library(DBI)
library(dplyr)
library(googlesheets4)
library(googledrive)

con2 <- dbConnect(odbc::odbc(), "reproreplica")


## VERIFICAR REGSTRO PARTICIPANTE ================================================


drive_get(id="1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ")


PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                      sheet = 'DADOS') %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
                                       mutate(CLICODIGO=as.numeric(CLICODIGO))

View(PARTICIPANTES_CAMPANHA)


## VERIFICA SE ESTA NA BASE DE CLIENTES  ================================================


partic_dados <- left_join(cli %>% 
                             filter(GCLCODIGO==82),PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% 
                               mutate(CPF=as.character(CPF)) %>% .[c(1,2,6,7,11:13)]


View(partic_dados)


## VERIFICA SE ESTA BASE DE CLIENTES CAMPANHAS  ================================================

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') 


left_join(partic_dados %>%  mutate(CPF=as.character(CPF)),BASE_CPF %>% 
            mutate(CPF=as.character(CPF)),by="CLICODIGO") %>% View()



## VERIFICA SE HA CARTAO ================================================  

CARTOES <- CARTOES_1222 %>%   
            mutate(CPF=sub("\\D+", '',CPF)) %>% 
              mutate(CPF=sub("\\.", '',CPF)) %>% 
               mutate(CPF=sub("\\-", '',CPF)) %>% 
                mutate(CPF=sub("\\,", '',CPF)) 

CARTOES <- inner_join(partic_dados,CARTOES,by="CPF")

View(CARTOES)








