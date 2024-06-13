
## MARCAS REPRO 

## PERIODO DE REFERENCIA 05.2024

## LOAD ============================================================================================

library(DBI)
library(tidyverse)
library(lubridate)
library(googlesheets4)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 



## CLIENTES ======================================================


clientes_mpr_0524 <- dbGetQuery(con2,"SELECT CP.CLICODIGO,CLINOMEFANT,GCLCODIGO FROM CLIPROMO CP
                                                LEFT JOIN CLIEN C ON C.CLICODIGO=CP.CLICODIGO
                                                WHERE ID_PROMO=44")

View(clientes_mpr_0524)


## SQL ============================================================

SQL_MREPRO_0524 <- dbGetQuery(con2,"WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=44)CLP ON C.CLICODIGO=CLP.CLICODIGO
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO),
       
PED AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
AND PEDSITPED <>'C'),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE PROSITUACAO='A'),

-- IMAGEM

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, 10 BONUS FROM PRODU WHERE  
                  (PRODESCRICAO LIKE '%IMAGEM%' AND 
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                  (PRODESCRICAO NOT LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL)),
                  
PROD2 AS (SELECT PROCODIGO,PRODESCRICAO, 40 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%IMAGEM%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),                  

-- UZ+

PROD3 AS (SELECT PROCODIGO, PRODESCRICAO, 15 AS BONUS 
FROM PRODU 
WHERE 
    (PRODESCRICAO LIKE '%UZ+%' AND 
    PRODESCRICAO NOT LIKE '%TGEN8%' AND
    PRODESCRICAO NOT LIKE '%TRANS%' AND
    PRODESCRICAO NOT LIKE '%FOTO%') AND
    GR2CODIGO = 1 AND 
    PROSITUACAO = 'A' AND 
    PROCODIGO2 IS NULL),
                  
PROD4 AS (SELECT PROCODIGO,PRODESCRICAO, 50 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%UZ+%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),                    

-- ACTUALITE


PROD5 AS (SELECT PROCODIGO, PRODESCRICAO, 25 AS BONUS 
FROM PRODU 
WHERE 
    (PRODESCRICAO LIKE '%ACTUALITE%' AND 
    PRODESCRICAO NOT LIKE '%TGEN8%' AND
    PRODESCRICAO NOT LIKE '%TRANS%' AND
    PRODESCRICAO NOT LIKE '%FOTO%') AND
    GR2CODIGO = 1 AND 
    PROSITUACAO = 'A' AND 
    PROCODIGO2 IS NULL),
                  
PROD6 AS (SELECT PROCODIGO,PRODESCRICAO, 70 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%ACTUALITE%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),                  
                  
-- AVANCE

PROD7 AS (SELECT PROCODIGO, PRODESCRICAO, 35 AS BONUS 
FROM PRODU 
WHERE 
    (PRODESCRICAO LIKE '%AVANCE%' AND 
    PRODESCRICAO NOT LIKE '%TGEN8%' AND
    PRODESCRICAO NOT LIKE '%TRANS%' AND
    PRODESCRICAO NOT LIKE '%FOTO%') AND
    GR2CODIGO = 1 AND 
    PROSITUACAO = 'A' AND 
    PROCODIGO2 IS NULL),
                  
PROD8 AS (SELECT PROCODIGO,PRODESCRICAO, 90 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%AVANCE%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),  
                 
RESULT AS (                 
SELECT 
PD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
      CASE 
      WHEN  PROD1.PROCODIGO IS NOT NULL THEN PROD1.BONUS
      WHEN  PROD2.PROCODIGO IS NOT NULL THEN PROD2.BONUS
      WHEN  PROD3.PROCODIGO IS NOT NULL THEN PROD3.BONUS
      WHEN  PROD4.PROCODIGO IS NOT NULL THEN PROD4.BONUS
      WHEN  PROD5.PROCODIGO IS NOT NULL THEN PROD5.BONUS
      WHEN  PROD6.PROCODIGO IS NOT NULL THEN PROD6.BONUS
      WHEN  PROD7.PROCODIGO IS NOT NULL THEN PROD7.BONUS
      WHEN  PROD8.PROCODIGO IS NOT NULL THEN PROD8.BONUS


      ELSE 0 END BONUS, 
       
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED p ON PD.ID_PEDIDO=P.ID_PEDIDO
INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
INNER JOIN PROD  ON PD.PROCODIGO=PROD.PROCODIGO
LEFT JOIN PROD1 ON PD.PROCODIGO=PROD1.PROCODIGO
LEFT JOIN PROD2 ON PD.PROCODIGO=PROD2.PROCODIGO
LEFT JOIN PROD3 ON PD.PROCODIGO=PROD3.PROCODIGO
LEFT JOIN PROD4 ON PD.PROCODIGO=PROD4.PROCODIGO
LEFT JOIN PROD5 ON PD.PROCODIGO=PROD5.PROCODIGO
LEFT JOIN PROD6 ON PD.PROCODIGO=PROD6.PROCODIGO
LEFT JOIN PROD7 ON PD.PROCODIGO=PROD7.PROCODIGO
LEFT JOIN PROD8 ON PD.PROCODIGO=PROD8.PROCODIGO


GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2)

SELECT * FROM RESULT WHERE BONUS<>0") 


## 1464 OTICA ESPECIALISTA =====================================


CP_1464_MREPRO_0524 <- 
  SQL_MREPRO_0524 %>% 
  filter(CLICODIGO==1464) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_1464_MREPRO_0524 %>% summarize(v=sum(VRVENDA))

CP_1464_MREPRO_0524%>% summarize(v=sum(BONUS))


OBS_1464_MREPRO_0524 <-  paste0("OTICA ESPECIALISTA 1464 MARCA REPRO ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1464_MREPRO_0524 <- inner_join(CP_1464_MREPRO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1464_MREPRO_0524)


### pagamentos

PAG_1464_MREPRO_0524 <- LIST_1464_MREPRO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1464_MREPRO_0524)




## 360 OTICA UNIVERSAL =====================================


CP_360_MREPRO_0524 <- 
  SQL_MREPRO_0524 %>% 
  filter(CLICODIGO==360) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_360_MREPRO_0524 %>% summarize(v=sum(VRVENDA))

CP_360_MREPRO_0524%>% summarize(v=sum(BONUS))


OBS_360_MREPRO_0524 <-  paste0("OTICA UNIVERSAL 360 MARCA REPRO ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_360_MREPRO_0524 <- inner_join(CP_360_MREPRO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_360_MREPRO_0524)


### pagamentos

PAG_360_MREPRO_0524 <- LIST_360_MREPRO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_360_MREPRO_0524)




## 1940 OTICA VARELA =====================================


CP_1940_MREPRO_0524 <- 
  SQL_MREPRO_0524 %>% 
  filter(CLICODIGO==1940) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_1940_MREPRO_0524 %>% summarize(v=sum(VRVENDA))

CP_1940_MREPRO_0524%>% summarize(v=sum(BONUS))


OBS_1940_MREPRO_0524 <-  paste0("OTICA VARELLA 1940 MARCA REPRO ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1940_MREPRO_0524 <- inner_join(CP_1940_MREPRO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_1940_MREPRO_0524)


### pagamentos

PAG_1940_MREPRO_0524 <- LIST_1940_MREPRO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1940_MREPRO_0524)




## 231 OTICA VEJA =====================================


CP_231_MREPRO_0524 <- 
  SQL_MREPRO_0524 %>% 
  filter(CLICODIGO==231) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_231_MREPRO_0524 %>% summarize(v=sum(VRVENDA))

CP_231_MREPRO_0524%>% summarize(v=sum(BONUS))


OBS_231_MREPRO_0524 <-  paste0("OTICA VEJA 231 MARCA REPRO ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_231_MREPRO_0524 <- inner_join(CP_231_MREPRO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_231_MREPRO_0524)


### pagamentos

PAG_231_MREPRO_0524 <- LIST_231_MREPRO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_231_MREPRO_0524)




## 509 OTICA CAPRI =====================================


CP_509_MREPRO_0524 <- 
  SQL_MREPRO_0524 %>% 
  filter(CLICODIGO==509) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 

CP_509_MREPRO_0524 %>% summarize(v=sum(VRVENDA))

CP_509_MREPRO_0524%>% summarize(v=sum(BONUS))


OBS_509_MREPRO_0524 <-  paste0("OTICA CAPRI 509 MARCA REPRO ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_509_MREPRO_0524 <- inner_join(CP_509_MREPRO_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-11] %>% mutate(OBS=OBS_509_MREPRO_0524)


### pagamentos

PAG_509_MREPRO_0524 <- LIST_509_MREPRO_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_509_MREPRO_0524)



## PAGAMENTOS  =============================================================================================================       


PAG_MREPRO_0524 <-  
  rbind(
    PAG_1464_MREPRO_0524,
    PAG_360_MREPRO_0524,
    PAG_35_MREPRO_0524,
    PAG_1940_MREPRO_0524,
    PAG_231_MREPRO_0524,
    PAG_509_MREPRO_0524
  ) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="MARCAS REPRO")


View(PAG_MREPRO_0524)

PAG_MREPRO_0524 %>% summarize(v=sum(BONUS))



## LISTAGEM  =============================================================================================================         


LIST_MREPRO_0524 <-  rbind(
  LIST_1464_MREPRO_0524,
  LIST_360_MREPRO_0524,
  LIST_35_MREPRO_0524,
  LIST_1940_MREPRO_0524,
  LIST_231_MREPRO_0524,
  LIST_509_MREPRO_0524
) 

View(LIST_MREPRO_0524)



## CREDITO CARTOES ==============================================================================================================


CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)

CREDITO_CARTOES_MREPRO_0524 <- left_join(PAG_MREPRO_0524 %>%
                                           mutate(CPF=as.character(CPF)),
                                         CARTOES_ALELO %>% filter(STATUS!="Cancelado") %>% 
                                           mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                           mutate(CPF=sub("\\.", '',CPF)) %>% 
                                           mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


View(CREDITO_CARTOES_MREPRO_0524)

## EXCLUI SEM CARTAO

CREDITO_CARTOES_MREPRO_0524_2 <- CREDITO_CARTOES_MREPRO_0524 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')



## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_MREPRO_0524_3 <- CREDITO_CARTOES_MREPRO_0524_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))



CREDITO_CARTOES_MREPRO_0524_3 %>% summarize(v=sum(`Valor da Carga`))

CREDITO_CARTOES_MREPRO_0524_3 %>% .[duplicated(.$CPF),]


# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_MREPRO_0524.csv")


# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_MREPRO_0524_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)


## EMISSAO CARTAO ======================================================================================================= 


EMISSAO_CARTOES_MREPRO_0524_1 <- CREDITO_CARTOES_MREPRO_0524 %>% 
  filter(is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S') %>%  
  left_join(.,PARTICIPANTES_CAMPANHA %>% 
              mutate(CPF=as.character(CPF)),by="CPF") %>% 
  .[,c(13,1:3,14:16)] %>% rename(CLICODIGO=1) %>% .[1,]




