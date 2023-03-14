
## PERIODO DE REFERENCIA 0223
## PAGAMENTOS
## SANDRO JAKOSKA

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")

RESUMO_CAMPANHAS_0223 <- read_sheet("1wwR63v4p48kfzYkHWtn920lBs1yrCCxmN-JcpRH68SU",
                       sheet = 'RESUMO') 

View(RESUMO_CAMPANHAS_0223)