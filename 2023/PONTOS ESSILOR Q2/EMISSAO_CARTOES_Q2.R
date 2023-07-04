
## CAMPANHA TRANSITIONS PONTOS ESSILOR Q2 2023

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readr)


con2 <- dbConnect(odbc::odbc(), "reproreplica")

## CLIENTES CAMPANHA ============================================================================================


CLIENTES_TGEN8_MAI23 <- read_sheet(ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",sheet = "CLIENTES")

View(CLIENTES_TGEN8_MAI23)



CLIENTES_TGEN8_MAI23_DADOS_CARTAO_A <-
left_join(CLIENTES_TGEN8_MAI23,PARTICIPANTES_CAMPANHA %>% 
            rename(DATA=1) %>% 
             rename(CLICODIGO=2) %>%
            group_by(CLICODIGO) %>% 
            summarize(DATA = max(DATA)), by = "CLICODIGO") 


CLIENTES_TGEN8_MAI23_DADOS_CARTAO_B <-
left_join(CLIENTES_TGEN8_MAI23,PARTICIPANTES_CAMPANHA %>% 
            rename(DATA=1) %>% 
            rename(CLICODIGO=2) %>%
            rename(NOME=3) 
          ,by="CLICODIGO") 

CLIENTES_TGEN8_MAI23_DADOS_CARTAO_C <-
inner_join(CLIENTES_TGEN8_MAI23_DADOS_CARTAO_B,
           CLIENTES_TGEN8_MAI23_DADOS_CARTAO_A,by=c("CLICODIGO","DATA"))

CLIENTES_TGEN8_MAI23_DADOS_CARTAO_C <-
  inner_join(CLIENTES_TGEN8_MAI23_DADOS_CARTAO_B,
             CLIENTES_TGEN8_MAI23_DADOS_CARTAO_A,by=c("CLICODIGO","DATA")) %>% .[,c(1:5)] 

View(CLIENTES_TGEN8_MAI23_DADOS_CARTAO_C)

range_write(data =CLIENTES_TGEN8_MAI23_DADOS_CARTAO_C %>% filter(is.na(NOME)),ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ", sheet = "DADOS CARTOES 280623" ,range = "A1")



