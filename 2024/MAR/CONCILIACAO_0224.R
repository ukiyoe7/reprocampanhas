
library(tidyverse)
library(readxl)
library(xlsx)


## CARTOES

CARTOES_010424 <- read_excel("~/CAMPANHAS/2024/MAR/CARTOES_010424.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 


cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_010424 <- CARTOES_010424 %>% `colnames<-`(cols2)


save(CARTOES_010424,file = "~/R/REPRO CAMPANHAS/2024/MAR/CARTOES_010424.RData")

View(CARTOES_010424)

## TRANSFERENCIAS

ALELO_010424 <- read_excel("~/CAMPANHAS/2024/MAR/ALELO_010424.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_010424 <- ALELO_010424 %>% `colnames<-`(cols3)

View(ALELO_010424)

ALELO_010424 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_010424_2 <-
  left_join(ALELO_010424,CARTOES_010424 %>% .[,c(1,2)],by="NSERIE") %>% 
  .[,c(1,4,9,3,8,2)] 

View(ALELO_280224_2)

write.csv2(ALELO_010424_2,file = "~/CAMPANHAS/2024/MAR/ALELO_010424_2.csv")

ALELO_010424_2 %>% group_by(DATA) %>% summarize(V=sum(VALOR)) %>% View()

ALELO_010424_2 %>% group_by(NOME,CPF) %>% summarize(V=sum(VALOR)) %>% 
  
write.csv2(.,file = "~/CAMPANHAS/2024/MAR/NOME_010424.csv")


## APURAÇÃO

inner_join(
  FIND_CLI %>% mutate(CPF=as.character(CPF)),PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% View() 


