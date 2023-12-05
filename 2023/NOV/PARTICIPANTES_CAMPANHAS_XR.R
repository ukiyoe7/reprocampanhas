
## LISTA DE PARTICIPANTES

View(PARTICIPANTES_CAMPANHA)

clientes_campanha_xr <- 
RESUMO_CAMPANHA_XR_SET_OUT_2023 %>% 
  filter(!is.na(BONIFICACAO)) %>% 
    filter(BONIFICACAO !=0) %>% 
   
   filter(CLICODIGO!='Total') %>%
  
  distinct(CLICODIGO) %>% mutate(CLICODIGO=as.numeric(CLICODIGO))


View(clientes_campanha_xr)

clientes_campanha_xr %>%  glimpse()


## PAGAMENTOS =====================================

CP_DADOSCARTAO_XR_A<-
  left_join(clientes_campanha_xr %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% 
              rename(DATA=1) %>% 
              rename(CLICODIGO=2) %>%
              group_by(CLICODIGO) %>% 
              summarize(DATA = max(DATA)), by = "CLICODIGO") 

view(CP_DADOSCARTAO_XR_A)


CP_DADOSCARTAO_XR_B <-
  left_join(clientes_campanha_xr %>% select(CLICODIGO),PARTICIPANTES_CAMPANHA %>% 
              rename(DATA=1) %>% 
              rename(CLICODIGO=2) %>%
              rename(NOME=3) 
            ,by="CLICODIGO") 

CP_DADOSCARTAO_XR_C <-
  inner_join(CP_DADOSCARTAO_XR_B ,
             CP_DADOSCARTAO_XR_A,by=c("CLICODIGO","DATA")) %>% .[,1:4] %>% as.data.frame() %>% 
  mutate(CPF=as.character(CPF))

View(CP_DADOSCARTAO_XR_C)


## CARTOES =====================================

dados_participantes_xr <-
left_join(CP_DADOSCARTAO_XR_C,CARTOES_081123 %>% 
              mutate(CPF=sub("\\D+", '', CPF))  %>% 
               mutate(CPF=sub("\\.", '',CPF)) %>% 
                mutate(CPF=sub("\\-", '',CPF)) %>% 
                 mutate(CPF=sub("\\.", '',CPF)) %>% 
                  filter(STATUS!="Cancelado"),by="CPF") %>% .[,1:6] 
  

write.csv2(dados_participantes_xr,file = "dados_participantes_xr.csv",row.names = FALSE,na="")






