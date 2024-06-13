
## CAMPANHAS VARILUX PREMIUM
## PERIODO REFERENCIA 05.2024

## LOAD ======================================================

library(DBI)
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")


BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


## CLIENTES ======================================================


clientes_vlx_premium_0524 <- dbGetQuery(con2,"SELECT CP.CLICODIGO,CLINOMEFANT,GCLCODIGO FROM CLIPROMO CP
                                                LEFT JOIN CLIEN C ON C.CLICODIGO=CP.CLICODIGO
                                                WHERE ID_PROMO=45
                                            ")


## SQL ======================================================

query_vlx_premium_0524 <- dbGetQuery(con2,"WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=45)CLP ON C.CLICODIGO=CLP.CLICODIGO
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO),

PED AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
AND PEDSITPED <>'C'),

PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PED PE ON  P.ID_PEDIDO=PE.ID_PEDIDO
   WHERE PROCODIGO='PAP'),                 
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PED PE ON P.ID_PEDIDPROMOCAO=PE.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE (MARCODIGO=57)),

-- XR DESIGN

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, 80 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR DESIGN%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO, 180 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR DESIGN%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                 
                  
-- XR TRACK

PROD3 AS (SELECT PROCODIGO,PRODESCRICAO, 100 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR TRACK%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO, 220 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR TRACK%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

-- XR PRO                

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO, 120 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR PRO%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD6 AS (SELECT PROCODIGO,PRODESCRICAO, 260 BONUS FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR PRO%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL)              

-- FINAL SELECT

                
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


      ELSE 0 END BONUS, 
        PD.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED p ON PD.ID_PEDIDO=P.ID_PEDIDO
INNER JOIN PROD  ON PD.PROCODIGO=PROD.PROCODIGO
LEFT JOIN PROD1 ON PD.PROCODIGO=PROD1.PROCODIGO
LEFT JOIN PROD2 ON PD.PROCODIGO=PROD2.PROCODIGO
LEFT JOIN PROD3 ON PD.PROCODIGO=PROD3.PROCODIGO
LEFT JOIN PROD4 ON PD.PROCODIGO=PROD4.PROCODIGO
LEFT JOIN PROD5 ON PD.PROCODIGO=PROD5.PROCODIGO
LEFT JOIN PROD6 ON PD.PROCODIGO=PROD6.PROCODIGO

LEFT OUTER JOIN PED_PROMO_UNION ON PD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2")



## CAMPANHA VARILUX PREMIUM 1923 ART JOIAS   =====================================


CP_1923_VLX_PREMIUM_0524 <- 
  query_vlx_premium_0524 %>% 
  filter(CLICODIGO==1923) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_1923_VLX_PREMIUM_0524 %>% summarize(v=sum(VRVENDA))

CP_1923_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))


OBS_1923_VLX_PREMIUM_0524 <-  paste0("ART JOIAS 1923 VARILUX PREMIUM ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1923_VLX_PREMIUM_0524 <- inner_join(CP_1923_VLX_PREMIUM_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_1923_VLX_PREMIUM_0524)


### pagamentos

PAG_1923_VLX_PREMIUM_0524 <- LIST_1923_VLX_PREMIUM_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1923_VLX_PREMIUM_0524)

## CAMPANHA VARILUX PREMIUM 1190 KOHLER JOIAS   =====================================


CP_1190_VLX_PREMIUM_0524 <- 
  query_vlx_premium_0524 %>% 
  filter(CLICODIGO==1190) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_1190_VLX_PREMIUM_0524 %>% summarize(v=sum(VRVENDA))

CP_1190_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))


OBS_1190_VLX_PREMIUM_0524 <-  paste0("KOHLER 1190 VARILUX PREMIUM ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_1190_VLX_PREMIUM_0524 <- inner_join(CP_1190_VLX_PREMIUM_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_1190_VLX_PREMIUM_0524)


### pagamentos

PAG_1190_VLX_PREMIUM_0524 <- LIST_1190_VLX_PREMIUM_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_1190_VLX_PREMIUM_0524)




## CAMPANHA VARILUX PREMIUM 4796 OJO   =====================================


CP_4796_VLX_PREMIUM_0524 <- 
  query_vlx_premium_0524 %>% 
  filter(CLICODIGO==4796) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_4796_VLX_PREMIUM_0524 %>% summarize(v=sum(VRVENDA))

CP_4796_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))


OBS_4796_VLX_PREMIUM_0524 <-  paste0("OJO 4796 VARILUX PREMIUM ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_4796_VLX_PREMIUM_0524 <- inner_join(CP_4796_VLX_PREMIUM_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_4796_VLX_PREMIUM_0524)


### pagamentos

PAG_4796_VLX_PREMIUM_0524 <- LIST_4796_VLX_PREMIUM_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_4796_VLX_PREMIUM_0524)


## CAMPANHA VARILUX PREMIUM 180 HELENA   =====================================


CP_G180_VLX_PREMIUM_0524 <- 
  query_vlx_premium_0524 %>% 
  filter(GCLCODIGO==180) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_G180_VLX_PREMIUM_0524 %>% summarize(v=sum(VRVENDA))

CP_G180_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))


OBS_G180_VLX_PREMIUM_0524 <-  paste0("OTICAS HELENA G180 VARILUX PREMIUM ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_G180_VLX_PREMIUM_0524 <- inner_join(CP_G180_VLX_PREMIUM_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_G180_VLX_PREMIUM_0524)


### pagamentos

PAG_G180_VLX_PREMIUM_0524 <- LIST_G180_VLX_PREMIUM_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_G180_VLX_PREMIUM_0524)


## CAMPANHA VARILUX PREMIUM 932 CRAFTS   =====================================


CP_932_VLX_PREMIUM_0524 <- 
  query_vlx_premium_0524 %>% 
  filter(CLICODIGO==932) %>% 
  mutate(CPF=sub("\\D+", '',PEDAUTORIZOU)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\,", '',CPF)) 



CP_932_VLX_PREMIUM_0524 %>% summarize(v=sum(VRVENDA))

CP_932_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))


OBS_932_VLX_PREMIUM_0524 <-  paste0("CRAFTS 932 VARILUX PREMIUM ",format(floor_date(Sys.Date(),"month") - months(1),"%m%y")) 

## JOIN CPF

LIST_932_VLX_PREMIUM_0524 <- inner_join(CP_932_VLX_PREMIUM_0524,BASE_CPF,by="CLICODIGO") %>% 
  rename(CPF=CPF.y) %>% .[,-12] %>% mutate(OBS=OBS_932_VLX_PREMIUM_0524)


### pagamentos

PAG_932_VLX_PREMIUM_0524 <- LIST_932_VLX_PREMIUM_0524 %>% group_by(CPF) %>% 
  summarize(BONUS=round(sum(BONUS),2)) %>% 
  mutate(OBS=OBS_932_VLX_PREMIUM_0524)




## PAGAMENTOS  =============================================================================================================       


PAG_VLX_PREMIUM_0524 <-  
  rbind(
    PAG_1923_VLX_PREMIUM_0524,
    PAG_1190_VLX_PREMIUM_0524,
    PAG_4796_VLX_PREMIUM_0524,
    PAG_G180_VLX_PREMIUM_0524,
    PAG_932_VLX_PREMIUM_0524
    
  ) %>% 
  mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="VARILUX PREMIUM")


View(PAG_VLX_PREMIUM_0524)

PAG_VLX_PREMIUM_0524 %>% summarize(v=sum(BONUS))




## LISTAGEM PEDIDOS   =============================================================================================================         


LIST_VLX_PREMIUM_0524 <-  rbind(
  LIST_1923_VLX_PREMIUM_0524,
  LIST_1190_VLX_PREMIUM_0524,
  LIST_4796_VLX_PREMIUM_0524,
  LIST_G180_VLX_PREMIUM_0524,
  LIST_932_VLX_PREMIUM_0524
) 

View(LIST_VLX_PREMIUM_0524)

LIST_VLX_ECONO_0524%>% summarize(v=sum(BONUS))



## CREDITO CARTOES ==============================================================================================================


CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)



CREDITO_CARTOES_VLX_PREMIUM_0524 <- left_join(PAG_VLX_PREMIUM_0524 %>%
                                                mutate(CPF=as.character(CPF)),
                                              CARTOES_ALELO %>% filter(STATUS!="Cancelado") %>% 
                                                mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                                mutate(CPF=sub("\\.", '',CPF)) %>% 
                                                mutate(CPF=sub("\\-", '',CPF)),by="CPF") 


## EXCLUI SEM CARTAO

CREDITO_CARTOES_VLX_PREMIUM_0524_2 <- CREDITO_CARTOES_VLX_PREMIUM_0524 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')



## CRIA BASE DE PAGAMENTO

CREDITO_CARTOES_VLX_PREMIUM_0524_3 <- CREDITO_CARTOES_VLX_PREMIUM_0524_2 %>% 
  .[,c(6,1,2,3)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))



# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_VLX_PREMIUM_0524.csv")


# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_VLX_PREMIUM_0524_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)


