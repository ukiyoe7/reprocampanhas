library(tidyverse)
library(readxl)
library(xlsx)
library(googlesheets4)


## CARTOES

CARTOES_0823 <- read_excel("~/CAMPANHAS/2023/AGO/CARTOES_0823.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 




cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_0823 <- CARTOES_0823 %>% `colnames<-`(cols2)


save(CARTOES_0823,file = "C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/AGO/CARTOES_0823.RData")

View(CARTOES_0823)

## TRANSFERENCIAS

ALELO_0823 <- read_excel("~/CAMPANHAS/2023/AGO/ALELO_0823.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_0823 <- ALELO_0823 %>% `colnames<-`(cols3)

View(ALELO_0823)

ALELO_0823 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_0823_2 <-
  left_join(ALELO_0823,CARTOES_0823 %>% .[,c(1,2)],by="NSERIE") %>% 
  .[,c(1,4,9,3,8,2)] 

View(ALELO_0823_2)

write.csv2(ALELO_0823_2,file = "~/CAMPANHAS/2023/JUL/ALELO_0823.csv")

ALELO_0823_2 %>% group_by(DATA) %>% summarize(V=sum(VALOR)) %>% View()

ALELO_0823_2 %>% group_by(NOME,CPF) %>% tally()

%>% write.csv2(.,file = "~/CAMPANHAS/2023/JUN/NOME_0823.csv")


## APURAÇÃO









