

## CAMPANHA TRANSITIONS PONTOS ESSILOR Q2 2023

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(glue)
library(readr)

## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")

### CLIENTES TRANSITIONS

CLIENTES_TGEN8_MAI23 <- read_sheet(ss="1eUdMe2la9Fkrs8tx0bWZneCJAUPoBwT3QURgni6b3DQ",sheet = "CLIENTES")

View(CLIENTES_TGEN8_MAI_23)


CP_CLIENTES_TGEN8_MAI23_sql <- glue_sql(read_file('C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/REPORTS/SQL/CAMPANHA_TRANSITIONS_Q2.sql'))
  
CP_TRANSITIONS_MAI23<-  dbGetQuery(con2,CP_CLIENTES_TGEN8_MAI23_sql) 