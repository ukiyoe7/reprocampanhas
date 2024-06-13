
## RESUMO CAMPANHAS JAN 2024

## PAGAMENTOS

resumo_campanhas_0124 <-
  rbind(
    REBATE_PAGAMENTO_0124,
    PAG_INSIGNE_0124_ALL,
    PAG_VARILUX_0124,
    PAG_VLX_ECONO_0124
  ) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_0124)


write.csv2(resumo_campanhas_0124,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\RESUMO_CAMPANHAS_JAN24.csv" ,row.names = FALSE,na="")


left_join(resumo_campanhas_dez23,resumo_campanhas_0124,by=c("CPF","TIPO")) %>% mutate(DIF=BONUS.y-BONUS.x) %>% View()

## EMISSAO CARTOES ===============================================================


EMISSAO_CARTOES_INSIGNE_0124_1 %>% write.csv2(.,"C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\JAN\\EMISSAO_CARTOES_INSIGNE_0124_1.csv")


## LISTAGEM

## REBATES ===================================

write.csv2(REBATE_LISTAGEM_0124 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124.csv" ,row.names = FALSE,na="")



write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR3.csv" ,row.names = FALSE,na="")



write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR4.csv" ,row.names = FALSE,na="")



write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR5.csv" ,row.names = FALSE,na="")



write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(REBATE_LISTAGEM_0124 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\REBATE_LISTAGEM_0124_SETOR7.csv" ,row.names = FALSE,na="")


##  INSIGNE =======================================================

write.csv2(LIST_INSIGNE_0124_ALL %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR3.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR4.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0124_ALL %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_INSIGNE_0124_SETOR7.csv" ,row.names = FALSE,na="")



## VARILUX ===============================================================

write.csv2(LIST_VARILUX_0124 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0124 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VARILUX_0124_SETOR7.csv" ,row.names = FALSE,na="")


## VARILUX ECONOMICA ====================================================

write.csv2(LIST_VLX_ECONO_0124 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0124 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\JAN\\LIST_VLX_ECONO_0124_SETOR7.csv" ,row.names = FALSE,na="")

