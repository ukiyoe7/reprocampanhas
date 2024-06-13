## PERIODO DE REFERENCIA 02.2024


## RESUMO ================================================

resumo_campanhas_0224 <-
  rbind(
    PAG_REBATES_0224,
    PAG_INSIGNE_0224_ALL,
    PAG_VARILUX_0224,
    PAG_VLX_ECONO_0224,
    PAG_MARCAS_REPRO_0224,
    PAG_OUTRAS_0224
  ) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_0224)

resumo_campanhas_0224 %>% summarize(v=sum(BONUS))

write.csv2(resumo_campanhas_0224,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\RESUMO_CAMPANHAS_FEV24.csv" ,row.names = FALSE,na="")


## EMISSAO CARTOES ===============================================================


EMISSAO_CARTOES_0224 <-
  rbind(
    EMISSAO_CARTOES_INSIGNE_0224_1) 


View(EMISSAO_CARTOES_0224)

EMISSAO_CARTOES_INSIGNE_0224_1 %>% write.csv2(.,"C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\FEV\\EMISSAO_CARTOES_INSIGNE_0224_1.csv")


## LISTAGEM

## REBATES ===================================

write.csv2(LIST_REBATES_0224 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR3.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR4.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR5.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(LIST_REBATES_0224 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0224_SETOR7.csv" ,row.names = FALSE,na="")


##  INSIGNE =======================================================

write.csv2(LIST_INSIGNE_0224_ALL %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR3.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR4.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0224_ALL %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0224_SETOR7.csv" ,row.names = FALSE,na="")



## VARILUX ===============================================================


write.csv2(LIST_VARILUX_0224 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0224 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0224_SETOR7.csv" ,row.names = FALSE,na="")


## VARILUX ECONOMICA ====================================================

write.csv2(LIST_VLX_ECONO_0224 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0224 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0224_SETOR7.csv" ,row.names = FALSE,na="")


## MARCAS REPRO ====================================================

write.csv2(LIST_MREPRO_0224 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0224 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0224_SETOR7.csv" ,row.names = FALSE,na="")

## OUTRAS ====================================================

write.csv2(LIST_OUTRAS_0224 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0224 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0224_SETOR7.csv" ,row.names = FALSE,na="")

