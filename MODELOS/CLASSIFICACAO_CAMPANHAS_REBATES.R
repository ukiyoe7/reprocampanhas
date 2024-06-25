## MODELO CLASSFICACAO CAMPANHAS REBATES
## ATUALIZADO EM 24.06.2024
## SANDRO JAKOSKA

## load ====================

library(DBI)
library(tidyverse)
library(readr)
library(openxlsx)

con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## Queries =========

pedidos_rebates <- dbGetQuery(con2, statement = read_file('MODELOS/PEDIDOS.sql')) %>% mutate(PROCODIGO=trimws(PROCODIGO))

clientes_rebates <- dbGetQuery(con2, statement = read_file('MODELOS/MODELOS_CAMPANHAS_REBATES.sql')) 

promo <- dbGetQuery(con2, statement = read_file('MODELOS/PROMO.sql')) %>% mutate(PROMO=as.integer(PROMO))

## Join campanhas and promo ============

pedidos_rebates <-
  left_join(pedidos_rebates,promo,by="ID_PEDIDO") %>% mutate(PROMO=if_else(is.na(PROMO),0,PROMO))


## filter clientes rebates ===========

clientes_rebates_pedidos <-
    inner_join(pedidos_rebates,clientes_rebates %>% distinct(CLICODIGO),by=c("CLICODIGO")) %>% 
  mutate(ID_PEDIDO=as.numeric(ID_PEDIDO))


View(clientes_rebates_pedidos)



## Extract CPF ===========

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


## VERIFICA METAS

clientes_rebates %>% 
  mutate(CLIENTE=if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G',as.character(GCLCODIGO))))


clientes_rebates_pedidos_cpf %>% filter(VENDA==1) %>% 
  mutate(CLIENTE=if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G',as.character(GCLCODIGO))))%>% 
   group_by(CLIENTE) %>%  
  summarize(RESULT=sum(VRVENDA)) %>% View()



