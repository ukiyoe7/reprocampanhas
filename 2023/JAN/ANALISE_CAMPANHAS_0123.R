## NOVO MODELO APURACAO CAMPANHAS
## PERIODO DE REFERENCIA 1022
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

RESULT_GERAL <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
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
  ) + scale_fill_manual(values=c("#275f96","#328c62","#db884c","#8d8d8d","#f0dc82","#4d9385"))



## INSIGNE 

RESULT_INSIGNE <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
                             sheet = "INSIGNE") 

RESULT_INSIGNE %>% 
  filter(floor_date(DATE,"day")>=floor_date(Sys.Date() %m-% months(13), 'month')) %>% 
  
  ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
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



tb_insigne <- data.frame(TEMCAMPANHA=c('S','N'),PERCENTUAL=c(0.65,0.35))

tb_insigne

tb_insigne %>% ggplot(.,aes(TEMCAMPANHA,PERCENTUAL,fill=TEMCAMPANHA)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=percent(PERCENTUAL),size=12,vjust=-0.1)) + 
  scale_fill_manual(values = c("#8b324d","#147388")) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1)) +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "TEM CAMPANHA ? ")




RESULT_INSIGNE2 <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
                              sheet = "INSIGNE2") 


RESULT_INSIGNE2  %>% 
  filter(floor_date(DATE,"day")>=floor_date(Sys.Date() %m-% months(13), 'month')) %>%
  
  ggplot(.,aes(DATE,VALOR,fill=TIPO)) + geom_bar(stat = "identity",position = "dodge2") + 
  geom_text(aes(label=format(VALOR,big.mark=",")),position = position_dodge2(width =0.7),vjust=-0.1,size=5) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,250000)) +
  scale_fill_manual(values = c("#6b8197","#092e53"))+
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Bonificações Insigne")


## REBATES

RESULT_REBATES <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
                             sheet = "REBATES") 


RESULT_REBATES %>% 
  filter(floor_date(DATE,"day")>=floor_date(Sys.Date() %m-% months(13), 'month')) %>%
  ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
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

OUTRAS_CAMPANHAS <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
                               sheet = "OUTRAS CAMPANHAS") 


OUTRAS_CAMPANHAS %>% 
  filter(floor_date(DATE,"day")>=floor_date(Sys.Date() %m-% months(13), 'month')) %>%
  ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1500)) + 
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


RETROATIVOS <- read_sheet("1bs5FyPoMjs6X8MtdvWDazGxCm6HKDPhwGJp-MrQTgzM",
                          sheet = "RETROATIVOS") 


RETROATIVOS %>% 
  filter(floor_date(DATE,"day")>=floor_date(Sys.Date() %m-% months(13), 'month')) %>%
  ggplot(.,aes(DATE,VALOR,fill=DATE)) + geom_bar(stat = "identity") + 
  geom_text(aes(label=format(VALOR,big.mark=","),vjust=-0.1)) + 
  scale_x_datetime(date_breaks = "month",date_labels = "%b/%Y",expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,10000)) + 
  scale_fill_datetime(low="#b3b3b3",high = "#1a1a1a") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none"
  ) + labs(title = "Bonificações Retroativos")


## CHECK INSIGNE



PAG_INSIGNE_1022_ALL

left_join(ALELO1122,CARTOES1122, by="NSERIE") %>% 
  mutate(CPF2=str_replace(.$CPF,"[.]","")) %>% 
  mutate(CPF2=str_replace(.$CPF2,"[.]","")) %>% 
  mutate(CPF2=str_replace(.$CPF2,"[-]","")) %>%  View()

anti_join(PAG_INSIGNE_1022_ALL %>% mutate(CPF=as.character(CPF)),.,by=c("CPF"="CPF2")) %>% view()


