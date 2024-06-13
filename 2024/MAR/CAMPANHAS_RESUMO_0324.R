## PERIODO DE REFERENCIA 03.2024


library(tidyverse)
library(openxlsx)


## RESUMO ================================================

resumo_campanhas_0324 <-
  rbind(
    PAG_REBATES_0324,
    PAG_INSIGNE_0324,
    PAG_VARILUX_0324,
    PAG_VLX_ECONO_0324,
    PAG_MREPRO_0324,
    PAG_OUTRAS_0324
  ) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))

View(resumo_campanhas_0324)

resumo_campanhas_0324 %>% summarize(v=sum(BONUS))

# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\RESUMO_CAMPANHAS_0324.xlsx")

lista_campanhas_0324 <-
  rbind(
    LIST_REBATES_0324 %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS),
    LIST_INSIGNE_0324  %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS),
    LIST_VARILUX_0324 %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS),
    LIST_VLX_ECONO_0324 %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS),
    LIST_MREPRO_0324 %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS),
    LIST_OUTRAS_0324 %>% select(ID_PEDIDO,CLICODIGO,GCLCODIGO,SETOR,PDPDESCRICAO,PEDAUTORIZOU,QTD,VRVENDA,BONUS,CPF,OBS)
  ) %>% as.data.frame() %>% mutate(CPF=as.character(CPF))


## CREATE EXCEL ===============================

## GERAL

wbcp <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet resumo

addWorksheet(wbcp, "RESUMO")

writeDataTable(wbcp, "RESUMO", resumo_campanhas_0324, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp, sheet = "RESUMO", cols = 1, widths = 12)

setColWidths(wbcp, sheet = "RESUMO", cols = 2, widths = 12)

setColWidths(wbcp, sheet = "RESUMO", cols = 3, widths = 45)

setColWidths(wbcp, sheet = "RESUMO", cols = 4, widths = 10)

setColWidths(wbcp, sheet = "RESUMO", cols = 5, widths = 20)



## sheet lista pedidos

addWorksheet(wbcp, "PEDIDOS")

writeDataTable(wbcp, "PEDIDOS", lista_campanhas_0324, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp, file_path, overwrite = TRUE)


## CREATE EXCEL SETOR 1 ===============================

## GERAL

file_path_st1 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR1_0324.xlsx")

wbcp_st1 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st1, "PEDIDOS")

writeDataTable(wbcp_st1, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 1 - FLORIANOPOLIS REDES"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st1, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st1, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st1, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st1, file_path_st1, overwrite = TRUE)


## CREATE EXCEL SETOR 2 ===============================

## GERAL

file_path_st2 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR2_0324.xlsx")

wbcp_st2 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st2, "PEDIDOS")

writeDataTable(wbcp_st2, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 2 - CRICIUMA - SUL"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st2, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st2, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st2, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st2, file_path_st2, overwrite = TRUE)


## CREATE EXCEL SETOR 3 ===============================

## GERAL

file_path_st3 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR3_0324.xlsx")

wbcp_st3 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st3, "PEDIDOS")

writeDataTable(wbcp_st3, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 3 - CHAPECO - OESTE - RS"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st3, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st3, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st3, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st3, file_path_st3, overwrite = TRUE)



## CREATE EXCEL SETOR 4 ===============================

## GERAL

file_path_st4 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR4_0324.xlsx")

wbcp_st4 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st4, "PEDIDOS")

writeDataTable(wbcp_st4, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 4 - JOINVILLE - NORTE"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st4, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st4, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st4, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st4, file_path_st4, overwrite = TRUE)


## CREATE EXCEL SETOR 5 ===============================

## GERAL

file_path_st5 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR5_0324.xlsx")

wbcp_st5 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st5, "PEDIDOS")

writeDataTable(wbcp_st5, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 5 - BLUMENAU - VALE"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st5, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st5, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st5, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st5, file_path_st5, overwrite = TRUE)


## CREATE EXCEL SETOR 6 ===============================

## GERAL

file_path_st6 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR6_0324.xlsx")

wbcp_st6 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st6, "PEDIDOS")

writeDataTable(wbcp_st6, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 6 - B CAMBORIU - LITORAL"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st6, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st6, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st6, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st6, file_path_st6, overwrite = TRUE)


## CREATE EXCEL SETOR 7 ===============================

## GERAL

file_path_st7 <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_SETOR7_0324.xlsx")

wbcp_st7 <- createWorkbook()

estilonumerico <- createStyle(numFmt = "#,##0.00")

estilotexto <- createStyle(numFmt = "text")


## sheet lista pedidos

addWorksheet(wbcp_st7, "PEDIDOS")

writeDataTable(wbcp_st7, "PEDIDOS", lista_campanhas_0324 %>% filter(SETOR=="SETOR 7 - FLORIANOPOLIS  LOJAS"), startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 1, widths = 12)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 2, widths = 12)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 3, widths = 12)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 4, widths = 30)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 5, widths = 50)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 6, widths = 20)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 7, widths = 10)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 8, widths = 10)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 9, widths = 10)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 10, widths = 15)

setColWidths(wbcp_st7, sheet = "PEDIDOS", cols = 11, widths = 40)

addStyle(wbcp_st7, sheet = "PEDIDOS", style = estilonumerico, cols = c(7,8,9), rows = 1:3000, gridExpand = TRUE)

addStyle(wbcp_st7, sheet = "PEDIDOS", style = estilotexto, cols = 10, rows = 1:3000, gridExpand = TRUE)


saveWorkbook(wbcp_st7, file_path_st7, overwrite = TRUE)



## EMISSAO CARTOES ===============================================================


EMISSAO_CARTOES_0324 <-
  rbind(
    EMISSAO_CARTOES_INSIGNE_0324_1) 


View(EMISSAO_CARTOES_0324)

EMISSAO_CARTOES_INSIGNE_0324_1 %>% write.csv2(.,"C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\MAR\\EMISSAO_CARTOES_INSIGNE_0324_1.csv")



## LISTAGEM

## REBATES ===================================

write.csv2(LIST_REBATES_0324 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR3.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR4.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR5.csv" ,row.names = FALSE,na="")



write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(LIST_REBATES_0324 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\REBATE_LISTAGEM_0324_SETOR7.csv" ,row.names = FALSE,na="")


##  INSIGNE =======================================================

write.csv2(LIST_INSIGNE_0324_ALL %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR1.csv" ,row.names = FALSE,na="")



write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR2.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR3.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR4.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR6.csv" ,row.names = FALSE,na="")


write.csv2(LIST_INSIGNE_0324_ALL %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_INSIGNE_0324_SETOR7.csv" ,row.names = FALSE,na="")



## VARILUX ===============================================================


write.csv2(LIST_VARILUX_0324 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR5.csv" ,row.names = FALSE,na="")


write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VARILUX_0324 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VARILUX_0324_SETOR7.csv" ,row.names = FALSE,na="")


## VARILUX ECONOMICA ====================================================

write.csv2(LIST_VLX_ECONO_0324 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_VLX_ECONO_0324 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_VLX_ECONO_0324_SETOR7.csv" ,row.names = FALSE,na="")


## MARCAS REPRO ====================================================

write.csv2(LIST_MREPRO_0324 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_MREPRO_0324 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_MREPRO_0324_SETOR7.csv" ,row.names = FALSE,na="")

## OUTRAS ====================================================

write.csv2(LIST_OUTRAS_0324 %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR1.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR2.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR3.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR4.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>%
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR5.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR6.csv" ,row.names = FALSE,na="")

write.csv2(LIST_OUTRAS_0324 %>% 
             filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% 
             as.data.frame() %>% 
             mutate(CPF=as.character(CPF)),file ="C:\\Users\\REPRO SANDRO\\Documents\\R\\REPRO CAMPANHAS\\2024\\FEV\\LIST_OUTRAS_0324_SETOR7.csv" ,row.names = FALSE,na="")

