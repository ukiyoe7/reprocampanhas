
library(readxl)
library(googlesheets4)


## CARTOES

CARTOES_0423 <- read_excel("~/CAMPANHAS/2023/MAR/CARTOES_0423.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_0423 <- CARTOES_0423 %>% `colnames<-`(cols2)

View(CARTOES_0423)


left_join()