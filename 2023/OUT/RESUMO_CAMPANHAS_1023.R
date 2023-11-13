
## RESUMO CAMPANHAS OUT 2023

## PAGAMENTOS

resumo_campanhas_out23 <-
rbind(
  REBATE_PAGAMENTO_1023,
  PAG_INSIGNE_1023_ALL,
  PAG_VARILUX_1023,
  PAG_VLX_ECONO_1023,
  PAG_VARILUX_G224_1023
) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_out23)

write.csv2(resumo_campanhas_out23,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\RESUMO_CAMPANHAS_OUT23.csv" ,row.names = FALSE,na="")


## LISTAGEM

# REBATES

write.csv2(REBATE_LISTAGEM_1023 %>% 
              as.data.frame() %>% 
                mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\REBATE_LISTAGEM_1023.csv" ,row.names = FALSE,na="")


# INSIGNE

write.csv2(LIST_INSIGNE_1023_ALL %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_INSIGNE_1023_ALL.csv" ,row.names = FALSE,na="")



# VARILUX

write.csv2(LIST_VARILUX_1023 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_VARILUX_1023.csv" ,row.names = FALSE,na="")


# VARILUX ECONOMICA

write.csv2(LIST_VLX_ECONO_1023 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_VLX_ECONO_1023.csv" ,row.names = FALSE,na="")

# OUTROS

write.csv2(LIST_VARILUX_G224_1023 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_VARILUX_G224_1023.csv" ,row.names = FALSE,na="")



