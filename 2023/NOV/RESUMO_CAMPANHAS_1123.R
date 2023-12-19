
## RESUMO CAMPANHAS NOV 2023

## PAGAMENTOS

resumo_campanhas_nov23 <-
  rbind(
    REBATE_PAGAMENTO_1123,
    PAG_INSIGNE_1123_ALL,
     PAG_VARILUX_1123,
      PAG_VLX_ECONO_1123
  ) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_nov23)

write.csv2(resumo_campanhas_nov23,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\RESUMO_CAMPANHAS_NOV23.csv" ,row.names = FALSE,na="")


left_join(resumo_campanhas_nov23,resumo_campanhas_out23,by="CPF") %>% View()

## LISTAGEM

# REBATES

write.csv2(REBATE_LISTAGEM_1123 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\REBATE_LISTAGEM_1123.csv" ,row.names = FALSE,na="")


# INSIGNE

write.csv2(LIST_INSIGNE_1123_ALL %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\LIST_INSIGNE_1123_ALL.csv" ,row.names = FALSE,na="")



# VARILUX

write.csv2(LIST_VARILUX_1123 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\LIST_VARILUX_1123.csv" ,row.names = FALSE,na="")


# VARILUX ECONOMICA

write.csv2(LIST_VLX_ECONO_1123 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\NOV\\LIST_VLX_ECONO_1123.csv" ,row.names = FALSE,na="")

# OUTROS

write.csv2(LIST_VARILUX_G224_1123 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\LIST_VARILUX_G224_1123.csv" ,row.names = FALSE,na="")



