

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
final_df <- generate_code(df)

# View the combined data frame
View(final_df)


