library(tidyverse)
library(readxl)
library(xlsx)
library(googlesheets4)


## CARTOES

CARTOES_1023 <- read_excel("~/CAMPANHAS/2023/OUT/CARTOES_1023.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 




cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_1023 <- CARTOES_1023 %>% `colnames<-`(cols2)


save(CARTOES_1023,file = "~/R/REPRO CAMPANHAS/2023/OUT/CARTOES_1023.RData")

View(CARTOES_1023)

## TRANSFERENCIAS

ALELO_1023 <- read_excel("~/CAMPANHAS/2023/OUT/ALELO_1023.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_1023 <- ALELO_1023 %>% `colnames<-`(cols3)

View(ALELO_1023)

ALELO_1023 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_1023_2 <-
  left_join(ALELO_1023,CARTOES_1023 %>% .[,c(1,2)],by="NSERIE") %>% 
  .[,c(1,4,9,3,8,2)] 

View(ALELO_1023_2)

write.csv2(ALELO_1023_2,file = "~/CAMPANHAS/2023/OUT/ALELO_1023.csv")

ALELO_0823_2 %>% group_by(DATA) %>% summarize(V=sum(VALOR)) %>% View()

ALELO_1023_2 %>% group_by(NOME,CPF) %>% summarize(V=sum(VALOR)) %>% 

write.csv2(.,file = "~/CAMPANHAS/2023/OUT/NOME_1023.csv")


## APURAÇÃO

inner_join(
FIND_CLI %>% mutate(CPF=as.character(CPF)),PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% View() 


