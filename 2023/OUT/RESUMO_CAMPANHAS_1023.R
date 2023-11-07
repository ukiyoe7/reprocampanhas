## RESUMO CAMPANHAS OUT 2023


resumo_campanhas_out23 <-
union_all(
  REBATE_PAGAMENTO_1023,
  PAG_INSIGNE_1023_ALL
) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_out23)

write.csv2(resumo_campanhas_out23,file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2023\\OUT\\RESUMO_CAMPANHAS_OUT23.csv" ,row.names = FALSE)


