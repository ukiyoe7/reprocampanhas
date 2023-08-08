## CAMPANHA TRANSITIONS ECONOMICA

library(DBI)
library(readr)
library(dplyr)
library(googlesheets4)
library(lubridate)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

BASE_CPF <- read_sheet("1ShpVwae6DVAqYW3afQli7XggL_7cJrDiWeKw2luKpC0",
                       sheet = 'DADOS') %>% select(CLICODIGO,CPF) 


query_vlx_0723 <- dbGetQuery(con2, statement = read_file('C:/Users/Repro/Documents/R/ADM/CAMPANHAS_REPRO/2023/JUL/VARILUX_ECONOMICA_0723.sql'))

View(query_vlx_0723)

## CAMPANHA VARILUX ECONOMICA G175 =====================================

CP_G175_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(GCLCODIGO==175) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G175_VLX_ECONO_0723)

CP_G175_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_G175_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_G175_VLX_ECONO_0723 <-  paste0("CHILLI BEANS G175 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G175_VLX_ECONO_0723 <- inner_join(CP_G175_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G175_VLX_ECONO_0723)


### pagamentos

PAG_G175_VLX_ECONO_0723 <- LIST_G175_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G175_VLX_ECONO_0723)


View(PAG_G175_VLX_ECONO_0723)


## CAMPANHA VARILUX ECONOMICA G148 =====================================


CP_G148_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(GCLCODIGO==148) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G148_VLX_ECONO_0723)

CP_G148_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_G148_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_G148_VLX_ECONO_0723 <-  paste0("HEUSI G148 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G148_VLX_ECONO_0723 <- inner_join(CP_G148_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G148_VLX_ECONO_0723)


### pagamentos

PAG_G148_VLX_ECONO_0723 <- LIST_G148_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G148_VLX_ECONO_0723)


View(PAG_G148_VLX_ECONO_0723)


## CAMPANHA VARILUX ECONOMICA G257 =====================================


CP_G257_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(GCLCODIGO==257) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_G257_VLX_ECONO_0723)

CP_G257_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_G257_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_G257_VLX_ECONO_0723 <-  paste0("ZOLET G257 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_G257_VLX_ECONO_0723 <- inner_join(CP_G257_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_G257_VLX_ECONO_0723)


### pagamentos

PAG_G257_VLX_ECONO_0723 <- LIST_G257_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G257_VLX_ECONO_0723)


View(PAG_G257_VLX_ECONO_0723)


## CAMPANHA VARILUX ECONOMICA 986 =====================================

CP_986_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(CLICODIGO==986) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_986_VLX_ECONO_0723)

CP_986_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_986_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_986_VLX_ECONO_0723 <-  paste0("OTICA LUZ 986 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_986_VLX_ECONO_0723 <- inner_join(CP_986_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_986_VLX_ECONO_0723)


### pagamentos

PAG_986_VLX_ECONO_0723 <- LIST_986_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0723)


View(PAG_986_VLX_ECONO_0723)

## CAMPANHA VARILUX ECONOMICA 3801 =====================================

CP_3801_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(CLICODIGO==3801) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_3801_VLX_ECONO_0723)

CP_3801_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_3801_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_3801_VLX_ECONO_0723 <-  paste0("OTICA LUZ 3801 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_3801_VLX_ECONO_0723 <- inner_join(CP_3801_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_3801_VLX_ECONO_0723)


### pagamentos

PAG_3801_VLX_ECONO_0723 <- LIST_3801_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0723)


View(PAG_3801_VLX_ECONO_0723)

## CAMPANHA VARILUX ECONOMICA 291 =====================================

CP_291_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(CLICODIGO==291) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_291_VLX_ECONO_0723)

CP_291_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_291_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_291_VLX_ECONO_0723 <-  paste0("OTICA LUZ 291 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_291_VLX_ECONO_0723 <- inner_join(CP_291_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_291_VLX_ECONO_0723)


### pagamentos

PAG_291_VLX_ECONO_0723 <- LIST_291_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0723)


View(PAG_291_VLX_ECONO_0723)

## CAMPANHA VARILUX ECONOMICA 4469 =====================================

CP_4469_VLX_ECONO_0723 <- 
  query_vlx_0723 %>% 
  filter(CLICODIGO==4469) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

View(CP_4469_VLX_ECONO_0723)

CP_4469_VLX_ECONO_0723 %>% summarize(v=sum(VRVENDA))

CP_4469_VLX_ECONO_0723 %>% summarize(v=sum(BONUS))


OBS_4469_VLX_ECONO_0723 <-  paste0("OTICA MARILDS 4469 VARILUX ECONOMICA ",format(floor_date(Sys.Date()-months(1),"month"),"%m%y")) 

## JOIN CPF

LIST_4469_VLX_ECONO_0723 <- inner_join(CP_4469_VLX_ECONO_0723,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_4469_VLX_ECONO_0723)


### pagamentos

PAG_4469_VLX_ECONO_0723 <- LIST_4469_VLX_ECONO_0723 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_986_VLX_ECONO_0723)


View(PAG_4469_VLX_ECONO_0723)


## PAGAMENTOS  =============================================================================================================       


PAG_VLX_ECONO_0723 <-  
  rbind(
    PAG_G175_VLX_ECONO_0723,
    PAG_G148_VLX_ECONO_0723,
    PAG_G257_VLX_ECONO_0723
  ) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX")


View(PAG_VLX_ECONO_0723)

PAG_VARILUX_0723 %>% summarize(v=sum(BONUS))

range_write(PAG_VLX_ECONO_0723,ss="19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",range = "A60",
            col_names = FALSE,sheet="RESUMO",reformat = FALSE)  


## LISTAGEM FINAL   =============================================================================================================         


LIST_VLX_ECONO_0723 <-  rbind(
  LIST_G175_VLX_ECONO_0723,
  LIST_G148_VLX_ECONO_0723,
  LIST_G257_VLX_ECONO_0723
) 


View(LIST_VLX_ECONO_0723)

LIST_VLX_ECONO_0723%>% summarize(v=sum(BONUS))

range_write(LIST_VLX_ECONO_0723,ss="19SdfYG-JiApWajfM7e5whV7VMvZpvji19A_DMm71Gls",
            range = "A:P",sheet="VARILUX ECONOMICA",reformat = FALSE)  



## CAMPANHA VARILUX SQL PRODUTOS =====================================================


dbGetQuery(con2,"
SELECT PROCODIGO FROM PRODU WHERE (MARCODIGO=57 OR PRODESCRICAO LIKE '%EYEZEN%')") %>% View() 

# EYEZEN ===========================================

# 1 - ORMA

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO, 20 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%ORMA%')) AND 
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL") %>% View() 


# 2 -  1.67 

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO, 40 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
                   PRODESCRICAO NOT LIKE '%TGEN8%' AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL") %>%View()

# 3 -  AIR 

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO, 50 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%AIR%')
             OR
             (PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%AIR%'))
             AND
                   PRODESCRICAO NOT LIKE '%TGEN8%' AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL") %>%View()

# 4 -  ORMA TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO, 60 BONUS FROM PRODU WHERE  
                  (PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%ORMA%') AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL") %>% View() 



# 5 - 1.67  TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,100 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%1.67%')) AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL") %>% View() 

# 6 - AIR  TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%AIR%')) AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL") %>% View() 



## VARILUX LIBERTY  ================================================

# 7 - ORMA

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,20 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 8 - AIR
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 9 - ORMA TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 10 - AIR TRANS
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 



## VARILUX COMFORT ================================================

# 11 - ORMA

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,40 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 12 -  1.67 1.74

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%' AND PRODESCRICAO NOT LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 13 - AIR

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 14 - ORMA TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,100 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 15 - 1.67 1.74 TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,140 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 16- AIR TRANS

dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 

## VARILUX E ==================================================================


## 17 - ORMA
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%ORMA%')
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 18 - 1.67 1.74
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,80 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 

## 19 -  AIR
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%AIR%')
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


## 20 - ORMA TRANS
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,140 TRANS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%ORMA%')
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


# 21 - 1.67 1.74 TRANS
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,180 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND 
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 


## 22 - AIR TRANS
dbGetQuery(con2,"
SELECT PROCODIGO,PRODESCRICAO,50 TRANS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%AIR%')
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15") %>%View() 



