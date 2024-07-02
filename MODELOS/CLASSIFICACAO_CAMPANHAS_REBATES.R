## MODELO CLASSFICACAO CAMPANHAS REBATES
## ATUALIZADO EM 24.06.2024
## SANDRO JAKOSKA

## LOAD ====================

library(DBI)
library(tidyverse)
library(readr)
library(openxlsx)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## QUERIES =========

pedidos_rebates <- dbGetQuery(con2, statement = read_file('MODELOS/PEDIDOS_REBATES.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))

clientes_rebates <- dbGetQuery(con2, statement = read_file('MODELOS/MODELOS_CAMPANHAS_REBATES.sql')) 

promo <- dbGetQuery(con2, statement = read_file('MODELOS/PROMO.sql')) %>% mutate(PROMO=as.integer(PROMO))

## Join campanhas and promo ============

pedidos_rebates <-
  left_join(pedidos_rebates,promo,by="ID_PEDIDO") %>% mutate(PROMO=if_else(is.na(PROMO),0,PROMO))


## filter clientes rebates ===========

clientes_rebates_pedidos <-
    inner_join(pedidos_rebates,clientes_rebates %>% distinct(CLICODIGO),by=c("CLICODIGO")) %>% 
  mutate(ID_PEDIDO=as.numeric(ID_PEDIDO))


## EXTRACT CPF ===========

clientes_rebates_pedidos_cpf <- 
  clientes_rebates_pedidos %>%  
  mutate(CPF = str_extract(PEDAUTORIZOU, "\\b\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}\\b|\\b\\d{11}\\b")) %>% 
  mutate(CPF=str_replace_all(CPF, "[\\.\\-]", "")) 

View(clientes_rebates_pedidos_cpf)


base_cpf <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


# Function to perform the left join without duplicating rows
left_join_no_duplicates <- function(clientes_rebates_pedidos_cpf, base_cpf, by) {
  # Select the first occurrence of each id in the right_df
  base_cpf_unique <- base_cpf %>%
    group_by(across(all_of(by))) %>%
    slice(1) %>%
    ungroup()
  
  # Perform the left join
  result_df <- clientes_rebates_pedidos_cpf %>%
    left_join(base_cpf_unique, by = by)
  
  return(result_df)
}

# Perform the left join without duplicating rows
result_rebates_df <- left_join_no_duplicates(clientes_rebates_pedidos_cpf, base_cpf, by = "CLICODIGO")

result_rebates_df_2 <-
  result_rebates_df %>% rename("CPF_PEDIDO"="CPF.x") %>% 
  rename("CPF_BASE"="CPF.y") %>% mutate(DIF=if_else(CPF_PEDIDO!=CPF_BASE,1,0)) %>% 
  mutate(CPF=if_else(is.na(CPF_BASE),CPF_PEDIDO,CPF_BASE)) 

View(result_rebates_df_2)


## OBTEM METAS ===========================


generate_code <- function(df) {
  data_frames <- list()  # Initialize an empty list to store data frames
  
  for (cliente in df$CLIENTE) {
    line <- paste0(
      "REBATE_CONTRATO_", cliente, 
      " <- tryCatch({read_excel(\"C:\\\\Users\\\\REPRO SANDRO\\\\OneDrive - Luxottica Group S.p.A (1)\\\\CAMPANHAS ALELO\\\\2024\\\\CONTRATOS\\\\CONTRATO_CP_", cliente, ".xlsx\") %>% mutate(CLIENTE='", cliente, "')}, error = function(e) {NULL})"
    )
    cat(line, "\n")  # Print the generated line of code
    eval(parse(text = line))
    
    df_name <- paste0("REBATE_CONTRATO_", cliente)
    if (!is.null(get(df_name))) {
      data_frames[[df_name]] <- get(df_name)
    }
  }
  
  # Combine all data frames in the list into one data frame
  if (length(data_frames) > 0) {
    all_combined_dfs <- do.call(rbind, data_frames)
    return(all_combined_dfs)
  } else {
    return(NULL)
  }
}


df <- clientes_rebates_pedidos_cpf %>% filter(VENDA==1) %>% 
  mutate(CLIENTE=if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G',as.character(GCLCODIGO))))%>% 
  group_by(CLIENTE) %>%  
  summarize(RESULT=sum(VRVENDA)) %>% as.data.frame() %>% select(CLIENTE)


final_df <- generate_code(df) %>% select(CLIENTE,PARAM,VALOR)


## MATCH METAS RESULT ============================


# Define the function to match the result
find_match <- function(result, cliente, final_df) {
  df <- subset(final_df, CLIENTE == cliente)
  
  if (nrow(df) == 0) {
    return(NA) # Return NA or an appropriate value if the subset is empty
  }
  
  match_value <- 0
  for (i in seq_len(nrow(df))) {
    if (result < df$PARAM[i]) {
      if (i == 1) {
        match_value <- 0
      } else {
        match_value <- df$VALOR[i - 1]
      }
      break
    }
  }
  if (result >= tail(df$PARAM, 1)) {
    match_value <- tail(df$VALOR, 1)
  }
  return(match_value)
}

# Result data frame
result_df <- clientes_rebates_pedidos_cpf %>% filter(VENDA==1) %>% 
  mutate(CLIENTE=if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G',as.character(GCLCODIGO))))%>% 
  group_by(CLIENTE) %>%  
  summarize(RESULT=sum(VRVENDA)) %>% as.data.frame()


# Create an empty column to store the match results
result_df$Match <- NA

# Iterate over the result_df and apply find_match for each row
for (i in 1:nrow(result_df)) {
  result <- result_df$RESULT[i]
  cliente <- result_df$CLIENTE[i]
  result_df$Match[i] <- find_match(result, cliente, final_df)
}



## CALCULA REBATE =============================

result_df %>% mutate(REBATE=RESULT*Match) %>% View()


## OBS LOGICA

## QUANDO REBATES EM GRUPOS NAO APURA LOJA INDIVIDUAL DENTRO DO GRUPO



