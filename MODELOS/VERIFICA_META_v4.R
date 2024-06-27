## VERIFICA METAS REBATES

## LOAD ==================================

library(readxl)
library(dplyr)

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

# Print the final dataframe
print(result_df)


## CALCULA REBATE =============================

result_df %>% mutate(REBATE=RESULT*Match) %>% View()




