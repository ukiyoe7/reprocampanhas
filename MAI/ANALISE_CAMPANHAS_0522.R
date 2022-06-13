## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 0522
## SANDRO JAKOSKA

## GET LIBRARIES

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)

## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")


## GET CPF

RESULT_INSIGNE <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                       sheet = "INSIGNE") 


RESULT_INSIGNE %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#cce5e5",high = "#004c4c") +
  theme(axis.text.x = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
        )



RESULT_REBATES <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                             sheet = "REBATES") 


RESULT_REBATES %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#ffd9d4",high = "#b22e1d") +
  theme(axis.text.x = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Rebates", subtitle = "bonificações mensais")


OUTRAS_CAMPANHAS <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                             sheet = "OUTRAS CAMPANHAS") 


OUTRAS_CAMPANHAS %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#cdddd6",high = "#043b25") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  )  + labs(title = "Outras Campanhas")


RETROATIVOS <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                               sheet = "RETROATIVOS") 


RETROATIVOS %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#f0c1c1",high = "#7b1e1e") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Retroativos")
