
library(readxl)
library(googlesheets4)



ALELO_0323 <- read_excel("~/CAMPANHAS/2023/FEV/ALELO_0323.xlsx")


ALELO_0323  <- ALELO_0323 %>% mutate(Valor = as.numeric(gsub(",", ".", gsub("\\.", "", gsub("R\\$ ", "", Valor))))) 

View(ALELO_0323)


ALELO_0323 %>% summarize(V=sum(Valor))

cols <- c("DATA","OBS","NSERIE","NOME","PRODUTO","CCUST","OPS","VALOR") 

ALELO_0323 <-
  ALELO_0323 %>% `colnames<-`(cols) 

View(ALELO_0323)

## CARTOES

CARTOES_0323 <- read_excel("~/CAMPANHAS/2023/FEV/CARTOES_0323.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_0323 <- CARTOES_0323 %>% `colnames<-`(cols2)

View(CARTOES_0323)


left_join(ALELO_0323,CARTOES_0323,by="NSERIE") %>% View()


RESUMO_INSIGNE_0123 <- read_sheet(ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",sheet = "INSIGNE") 

RESUMO_INSIGNE_0123 %>% 
  group_by(CPF3,CLICODIGO,GCLCODIGO) %>% 
  summarize(n=n_distinct(CPF3)) %>% 
  as.data.frame() %>% 
  write_sheet(.,ss="1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",sheet="CLI")


left_join(RESUMO_0123,PARTICIPANTES_CAMPANHA,by="CPF") %>% View()


