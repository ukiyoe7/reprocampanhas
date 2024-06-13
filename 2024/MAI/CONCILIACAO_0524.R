
library(tidyverse)
library(readxl)
library(xlsx)
library(clipr)


## CARTOES

CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)


save(CARTOES_ALELO,file = "~/R/REPRO CAMPANHAS/2024/MAI/CARTOES_ALELO.RData")

View(CARTOES_ALELO)

## TRANSFERENCIAS

ALELO_0524 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\MAI\\ALELO_0524.xlsx")

cols3 <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPERAÇÃO","VALOR") 

ALELO_0524 <- ALELO_0524 %>% `colnames<-`(cols3)

View(ALELO_0524)

ALELO_0524 %>% summarize(V=sum(VALOR))

## INTERSECT

ALELO_0524_2 <-
  left_join(ALELO_0524,CARTOES_ALELO %>% .[,c(1,2)],by="NSERIE") %>%  .[,c(1,4,9,3,8,2)] 

View(ALELO_0524_2)

write.csv2(ALELO_0524_2,file = "~/R/REPRO CAMPANHAS/2024/MAI/ALELO_0524_2.csv")


ALELO_0524_2 %>% group_by(NOME,CPF) %>% summarize(V=sum(VALOR)) %>% 
  
  write.csv2(.,file = "~/R/REPRO CAMPANHAS/2024/MAI//NOME_0524.csv")


## APURAÇÃO

inner_join(
  FIND_CLI %>% mutate(CPF=as.character(CPF)),PARTICIPANTES_CAMPANHA %>% mutate(CPF=as.character(CPF)),by="CPF") %>% View() 


# Read the data from the clipboard as a table
setA <- read.table(text = read_clip(), header = TRUE, sep = "\t", colClasses = "character")

View(setA)

setB <- read.table(text = read_clip(), header = TRUE, sep = "\t", colClasses = "character")


anti_join(setB,setA,by="CPF") %>% View()
