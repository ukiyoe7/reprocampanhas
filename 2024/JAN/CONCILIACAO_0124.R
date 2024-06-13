
library(tidyverse)
library(readxl)
library(xlsx)


## CARTOES

CARTOES_260224 <- read_excel("~/CAMPANHAS/2024/FEV/CARTOES_260224.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 


cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_260224 <- CARTOES_260224 %>% `colnames<-`(cols2)


save(CARTOES_260224,file = "~/R/REPRO CAMPANHAS/2024/FEV/CARTOES_260224.RData")

View(CARTOES_260224)

## TRANSFERENCIAS

ALELO_280224 <- read_excel("~/CAMPANHAS/2024/FEV/ALELO_280224.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_280224 <- ALELO_280224 %>% `colnames<-`(cols3)

View(ALELO_280224)

ALELO_260124 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_280224_2 <-
  left_join(ALELO_280224,CARTOES_260224 %>% .[,c(1,2)],by="NSERIE") %>% 
  .[,c(1,4,9,3,8,2)] 

View(ALELO_280224_2)

write.csv2(ALELO_280224_2,file = "~/CAMPANHAS/2024/FEV/ALELO_280224_2.csv")

ALELO_280224_2 %>% group_by(DATA) %>% summarize(V=sum(VALOR)) %>% View()

ALELO_280224_2 %>% group_by(NOME,CPF) %>% summarize(V=sum(VALOR)) %>% 
  
  write.csv2(.,file = "~/CAMPANHAS/2024/FEV/NOME_280224.csv")




## APURAÇÃO

inner_join(
  FIND_CLI %>% mutate(CPF=as.character(CPF)),PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% View() 


