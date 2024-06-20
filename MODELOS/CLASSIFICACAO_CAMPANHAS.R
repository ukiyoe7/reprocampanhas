## MODELO CLASSFICACAO CAMPANHAS
## ATUALIZADO EM 19.06.2024
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(readr)
library(openxlsx)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## Queries

pedidos <- dbGetQuery(con2, statement = read_file('MODELOS/PEDIDOS.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))

modelos <- dbGetQuery(con2, statement = read_file('MODELOS/MODELOS_CAMPANHAS.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))

promo <- dbGetQuery(con2, statement = read_file('MODELOS/PROMO.sql')) %>% mutate(PROMO=as.integer(PROMO))

## Join campanhas and promo

pedidos_2 <-
  left_join(pedidos,promo,by="ID_PEDIDO") %>% mutate(PROMO=if_else(is.na(PROMO),0,PROMO))

## Extract CPF

pedidos_3 <- 
pedidos_2 %>%  
   mutate(CPF = str_extract(PEDAUTORIZOU, "\\b\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}\\b|\\b\\d{11}\\b")) %>% 
    mutate(CPF=str_replace_all(CPF, "[\\.\\-]", "")) 


## Using Joins to classify

classificacao_campanhas <-
  
left_join(
inner_join(pedidos_3,modelos %>% distinct(CLICODIGO,PROCODIGO),by=c("CLICODIGO","PROCODIGO"))
          
,modelos,by=c("CLICODIGO","PROCODIGO","PEDORIGEM","QTD","PROMO","VENDA")) %>% 
  select(-PRODESCRICAO.y) %>% rename(PRODESCRICAO=PRODESCRICAO.x) %>% 
   mutate(ID_PEDIDO=as.numeric(ID_PEDIDO))


View(classificacao_campanhas)

classificacao_campanhas %>% summarize(v=sum(BONUS,na.rm = TRUE))


## summarize

resumo_campanhas_m1 <-
classificacao_campanhas %>% 
  filter(!is.na(CAMPANHA)) %>% 
  group_by(CAMPANHA) %>% summarize(BONUS=sum(BONUS,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(summarize(., CAMPANHA = "TOTAL", BONUS = sum(BONUS)))


resumo_campanhas_setores_m1 <-
  classificacao_campanhas %>% 
  filter(!is.na(CAMPANHA)) %>% 
  group_by(SETOR) %>% summarize(BONUS=sum(BONUS,na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(summarize(., SETOR = "TOTAL", BONUS = sum(BONUS)))

## write excel

wb_campanhas_m1 <- createWorkbook()

## sheet resumo

addWorksheet(wb_campanhas_m1, "RESUMO")

setColWidths(wb_campanhas_m1, sheet = "RESUMO", cols = 1, widths = 2)

setColWidths(wb_campanhas_m1, sheet = "RESUMO", cols = 2, widths = 20)

estiloNumerico <- createStyle(numFmt = "#,##0")

writeDataTable(wb_campanhas_m1, "RESUMO", resumo_campanhas_m1, startCol = 2, startRow = 4, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

addStyle(wb_campanhas_m1, sheet = "RESUMO", style = estiloNumerico, cols = 3, rows = 1:nrow(resumo_campanhas_m1)+1, gridExpand = TRUE)



## sheet pedidos

addWorksheet(wb_campanhas_m1, "PEDIDOS")


setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 1:4, widths = 13)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 5, widths = 40)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 6, widths = 12)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 7, widths = 30)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 8:11, widths = 12)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 12, widths = 35)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 13:16, widths = 12)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 17, widths = 25)

setColWidths(wb_campanhas_m1, sheet = "PEDIDOS", cols = 18, widths = 10)


datesty <- createStyle(numFmt = "dd/MM/yyyy")


writeDataTable(wb_campanhas_m1, "PEDIDOS", classificacao_campanhas, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)

addStyle(wb_campanhas_m1, sheet = "PEDIDOS",style = datesty, cols = c(2:3), rows = 1:nrow(classificacao_campanhas)+1, gridExpand = TRUE)



# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))


# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CAMPANHAS_M1.xlsx")

dir_path <- dirname(file_path)
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}


saveWorkbook(wb_campanhas_m1, file = file_path, overwrite = TRUE)

