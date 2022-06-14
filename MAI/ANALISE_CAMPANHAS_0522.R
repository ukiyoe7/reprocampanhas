## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 0522
## SANDRO JAKOSKA

## GET LIBRARIES

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(scales)


## ============================================================================================

con2 <- dbConnect(odbc::odbc(), "reproreplica")


## GERAL

RESULT_GERAL <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                             sheet = "RESUMODADOS") 


RESULT_GERAL %>% mutate(PROP=percent(BONUS/sum(BONUS)))%>% 
  ggplot(.,aes(x=reorder(CAMPANHAS,BONUS),BONUS,fill=CAMPANHAS)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(BONUS,big.mark=","),hjust=-0.1)) + 
  geom_text(aes(label=format(PROP,big.mark=","),hjust=1.5)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0,22000)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size=15)
  ) + scale_fill_manual(values=c("#275f96","#328c62","#db884c","#8d8d8d",))



## INSIGNE 

RESULT_INSIGNE <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                       sheet = "INSIGNE") 


RESULT_INSIGNE %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#b7c9dc",high = "#092e53") +
  theme(axis.text.x = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
        ) + labs(title = "Bonificações Insigne")


## REBATES

RESULT_REBATES <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                             sheet = "REBATES") 


RESULT_REBATES %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,22000)) + 
  scale_fill_datetime(low="#eabb99",high = "#a34400") +
  theme(axis.text.x = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Bonificações Rebates")


## OUTRAS CAMPANHAS

OUTRAS_CAMPANHAS <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                             sheet = "OUTRAS CAMPANHAS") 


OUTRAS_CAMPANHAS %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,10000)) + 
  scale_fill_datetime(low="#99c5b1",high = "#004e2a") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  )  + labs(title = "Bonificações em Outras Campanhas")


## RETROATIVOS


RETROATIVOS <- read_sheet("1TIlc2UCW9Cz1wVyXe9Fp6WgopDOROJfDkdJUPSWwxek",
                               sheet = "RETROATIVOS") 


RETROATIVOS %>% ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,15000)) + 
  scale_fill_datetime(low="#b3b3b3",high = "#1a1a1a") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Bonificações Retroativos")
