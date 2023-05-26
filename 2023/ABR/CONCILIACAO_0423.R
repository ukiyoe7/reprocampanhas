library(tidyverse)
library(readxl)
library(xlsx)
library(googlesheets4)


## CARTOES

CARTOES_0523 <- read_excel("~/CAMPANHAS/2023/ABR/CARTOES_0523.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_0523 <- CARTOES_0523 %>% `colnames<-`(cols2)

View(CARTOES_0523)

## TRANSFERENCIAS

ALELO_0423 <- read_excel("~/CAMPANHAS/2023/ABR/ALELO_0423.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_0423 <- ALELO_0423 %>% `colnames<-`(cols3)

View(ALELO_0423)

ALELO_0423 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_0423_2 <-
  left_join(ALELO_0423,CARTOES_0523 %>% .[,c(1,2)],by="NSERIE") %>% 
  .[,c(1,4,9,3,8,2)] 

View(ALELO_0423_2)

write.csv2(ALELO_0423_2,file = "~/CAMPANHAS/2023/ABR/ALELO_0423_2.csv")

ALELO_0423_2 %>% group_by(DATA) %>% summarize(V=sum(VALOR)) %>% View()


left_join(ALELO_0423,CARTOES_0423,by="NSERIE") %>% summarize(V=sum(VALOR))


## APURACAO

resumo_0423 <- 
  read_sheet(ss="1-WNd3PxF5xhxy1Z2xO0EnEP0ujyzIxYr0h6eO6DgU_g",sheet = "RESUMO")


View(resumo_0423)

LIST_INSIGNE_0423_ALL %>% group_by(CLICODIGO,GCLCODIGO,CPF3) %>% tally() %>% View()


RESUMO_CAMPANHAS_0423 <- 
  left_join(resumo_0423 %>% rename(CPF3=1),
            LIST_INSIGNE_0423_ALL %>% group_by(CLICODIGO,GCLCODIGO,CPF3) %>% tally() %>% mutate(CPF3=as.character(CPF3)),by="CPF3") %>% 
  as.data.frame() 

write.xlsx(RESUMO_CAMPANHAS_0423,file="C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/ABR/RESUMO_CAMPANHAS_0423.xlsx")  







