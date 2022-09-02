library(readr)
CARTOES_0822 <- read_delim("C:/Users/Repro/Downloads/ExportedReport (42).txt", 
                                 delim = ";", escape_double = FALSE, 
                                 col_types = cols(`Data de Cadastro` = col_date(format = "%d/%m/%Y")), 
                                 trim_ws = TRUE)  

nm <- c('COD','NUM_PED','CPF','TIPO','NSERIE','NOME','CCUST','DATACADASTRO')  

CARTOES_0822 <- CARTOES_0822 %>% `colnames<-`(nm)
  
View(CARTOES_0822)

