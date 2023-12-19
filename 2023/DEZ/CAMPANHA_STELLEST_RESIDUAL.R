

STELLEST_JUL_AGO_23 %>% filter(str_detect(CLIENTE,"G88")) %>% 
   filter(PAGAMENTO=='SIM')%>% View()

left_join(STELLEST_JUL_AGO_23 %>% filter(str_detect(CLIENTE,"G88")) %>% 
            filter(PAGAMENTO=='SIM') %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% .[,c(1,2,3,4)],by="CLICODIGO")  %>% View()


left_join(STELLEST_JUL_AGO_23 %>% filter(str_detect(CLIENTE,"G88")) %>% 
            filter(PAGAMENTO=='SIM') %>% 
            select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% .[,c(1,2,3,4)] %>% mutate(CPF=as.character(CPF)),by="CLICODIGO")  %>% 
  write.csv2(.,file="C:/Users/REPRO SANDRO/Documents/R/REPRO CAMPANHAS/2023/DEZ/g88_stellest.csv" ,row.names = FALSE,na="") 

inner_join(
CARTOES_1123,left_join(STELLEST_JUL_AGO_23 %>% filter(str_detect(CLIENTE,"G88")) %>% 
                         filter(PAGAMENTO=='SIM') %>% 
                         select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% .[,c(1,2,3,4)] %>% 
                         mutate(CPF=as.character(CPF)),by="CLICODIGO"),by="CPF") %>% View() 