
## PARTICIPANTES


library(DBI)
library(dplyr)
library(googlesheets4)
con2 <- dbConnect(odbc::odbc(), "reproreplica")
## CAMPANHAS INSIGNE 



PARTICIPANTES_CAMPANHA <- read_sheet("1jUVGD4qsU0ZI7Z9Tgo8in_82KD_GA4xlseQs1gQPdCQ",
                                      sheet = 'DADOS') %>% rename(CLICODIGO=`CÓDIGO DA ÓTICA`) %>% 
                                       mutate(CLICODIGO=as.numeric(CLICODIGO))

View(PARTICIPANTES_CAMPANHA)

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 

View(BASE_CPF)



CAMPANHAS <- read_sheet("1Tt7VLY1oHoirHduSaJoj62SlkyZCMVIZ9eHzLAhpqIY",
                       sheet = 'CAMPANHAS ATIVAS') %>% .[,c(2,3,4,5,6,7,8,13,14,15)] 

View(CAMPANHAS)


## CAMPANHAS LOJAS

CAMPANHAS_LOJAS <- read_sheet("1Tt7VLY1oHoirHduSaJoj62SlkyZCMVIZ9eHzLAhpqIY",
                               sheet = 'CAMPANHAS ATIVAS') %>% .[,c(2,4,6,7,8,12,13,14,15)] %>% 
  filter(!is.na(CLICODIGO))


cli <- dbGetQuery(con2,"SELECT DISTINCT C.CLICODIGO,
                          CLINOMEFANT,
                           ENDCODIGO,
                            C.GCLCODIGO,
                             GCLNOME,
                              SETOR
                               FROM CLIEN C
                                LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                 LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                  LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                   WHERE CLICLIENTE='S'")


left_join(CAMPANHAS_LOJAS ,cli,by="CLICODIGO") %>% left_join(.,PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% View()





## CAMPANHAS GRUPOS

CAMPANHAS_GRUPOS <- read_sheet("1Tt7VLY1oHoirHduSaJoj62SlkyZCMVIZ9eHzLAhpqIY",
                        sheet = 'CAMPANHAS ATIVAS') %>% .[,c(3,4,5,6,7,8,12,13,14,15)] %>% 
                         filter(!is.na(GCLCODIGO))


cli <- dbGetQuery(con2,"SELECT DISTINCT C.CLICODIGO,
                          CLINOMEFANT,
                           ENDCODIGO,
                            C.GCLCODIGO,
                             GCLNOME,
                              SETOR
                               FROM CLIEN C
                                LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                 LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                  LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                   WHERE CLICLIENTE='S'")


left_join(CAMPANHAS_GRUPOS ,cli,by="GCLCODIGO") %>% left_join(.,PARTICIPANTES_CAMPANHA,by="CLICODIGO") %>% View()




