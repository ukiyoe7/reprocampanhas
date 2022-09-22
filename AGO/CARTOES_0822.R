library(readr)
CARTOES_0822 <- read_delim("C:/Users/Repro/Downloads/ExportedReport (42).txt", 
                                 delim = ";", escape_double = FALSE, 
                                 col_types = cols(`Data de Cadastro` = col_date(format = "%d/%m/%Y")), 
                                 trim_ws = TRUE)  

nm <- c('COD','NUM_PED','CPF','TIPO','NSERIE','NOME','CCUST','DATACADASTRO')  

CARTOES_0822 <- CARTOES_0822 %>% `colnames<-`(nm)
  
View(CARTOES_0822)


## CARTOES ==========================================================================


emissao_cartoes_0922_1 <- left_join(PAG_INSIGNE_0822_ALL %>% 
                                      mutate(CPF=as.character(CPF)),CARTOES_0822,by="CPF") %>% 
                                         left_join(.,PARTICIPANTES_CAMPANHA %>% 
                                          mutate(CPF=as.character(CPF)),by="CPF") %>% 
                                           filter(is.na(NSERIE)) 


emissao_cartoes_0922_2 <- left_join(PAG_ALL_0822 %>% 
                            mutate(CPF=as.character(CPF)),CARTOES_0822,by="CPF") %>% 
                             left_join(.,PARTICIPANTES_CAMPANHA %>% 
                              mutate(CPF=as.character(CPF)),by="CPF") %>% 
                                filter(is.na(NSERIE))

emissao_cartoes_0922_3 <- rbind(emissao_cartoes_0922_1,emissao_cartoes_0922_2) %>% 
                           .[,c(1,2,3,11:15)] %>% distinct(.,CLICODIGO,.keep_all=TRUE)
  
  
View(emissao_cartoes_0922_3)  


range_write(emissao_cartoes_0922_3 ,ss="10WezW63OwIO2-eKkYhhOQxbD9cu1ozLf8YF55gwCxdA",
            range = "A11",sheet="DADOS",reformat = FALSE)  

