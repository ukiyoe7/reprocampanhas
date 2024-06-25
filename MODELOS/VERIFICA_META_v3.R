
library(readxl)
library(dplyr)

# Define multiple data frames
REBATE_CONTRATO_849 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_849.xlsx") %>% mutate(CLIENTE='849')
REBATE_CONTRATO_157 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_157.xlsx") %>% mutate(CLIENTE='157')
REBATE_CONTRATO_198 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_198.xlsx") %>% mutate(CLIENTE='198')
REBATE_CONTRATO_G139 <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CONTRATOS\\CONTRATO_CP_G139.xlsx") %>% mutate(CLIENTE='G139')

# Combine the data frames into one
UNION_REBATES <- rbind(REBATE_CONTRATO_849, REBATE_CONTRATO_157, REBATE_CONTRATO_198, REBATE_CONTRATO_G139) %>%
  select(CLIENTE, PARAM, VALOR)

# Define the function to match the result
find_match <- function(result, cliente, UNION_REBATES) {
  df <- subset(UNION_REBATES, CLIENTE == cliente)
  
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
result_df <- data.frame(CLIENTE = c("849", "157", "198", "G139"), W = c(200000, 30000, 1000, 350))

# Create an empty column to store the match results
result_df$Match <- NA

# Iterate over the result_df and apply find_match for each row
for (i in 1:nrow(result_df)) {
  result <- result_df$W[i]
  cliente <- result_df$CLIENTE[i]
  result_df$Match[i] <- find_match(result, cliente, UNION_REBATES)
}

# Print the final dataframe
print(result_df)


