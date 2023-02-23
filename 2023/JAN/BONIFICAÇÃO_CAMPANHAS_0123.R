## BONIFICAÇÃO CAMPANHAS



BONIF_0123_1 <- read_sheet("1TwpOhu9lr_Us8_hpFz-A7GzfIIzpL3XzIsKTfKHA_B4",sheet = 'RESUMO') 

View(BONIF_0123_1)


BONIF_0123_2 <- left_join(BONIF_0123_1 %>% 
                            mutate(CPF=as.character(CPF)) %>% 
                              filter(is.na(STATUS)),CARTOES_060223 %>% 
                            filter(Status=='Ativo')  %>%
                  mutate(CPF=sub("\\D+", '',CPF)) %>% 
                   mutate(CPF=sub("\\.", '',CPF)) %>% 
                    mutate(CPF=sub("\\-", '',CPF)) %>% 
                     mutate(CPF=sub("\\,", '',CPF)) ,by="CPF") 

View(BONIF_0123_2)
  
 
BONIF_0123_3 <- BONIF_0123_2 %>% .[,c(6,1,2,3)] 

View(BONIF_0123_3)

BONIF_0123_3 %>% .[duplicated(.$CPF),]

BONIF_0123_3 %>% summarize(BONUS=sum(BONUS))

write.csv2(BONIF_0123_3,file = "C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/JAN/BONIF_0123_4.csv",row.names=FALSE,quote = FALSE)