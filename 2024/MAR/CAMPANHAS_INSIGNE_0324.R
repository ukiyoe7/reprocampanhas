
## PERIODO DE REFERENCIA 0324
## SANDRO JAKOSKA

## LOAD =======================================================================================================  


library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")


BASE_CPF <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\BASE_CLIENTES_CAMPANHAS_2024.xlsx") %>% select(CLICODIGO,CPF) 


VALORES_INSIGNE <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\VALORES_INSIGNE.xlsx")


## SQL =======================================================================================================  


CP_INSIGNE_0324 <-dbGetQuery(con2,"
  WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
    FROM CLIEN C
     INNER JOIN (SELECT CLICODIGO FROM CLIPROMO WHERE ID_PROMO=18)CLP ON C.CLICODIGO=CLP.CLICODIGO
      LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
        WHERE CLICLIENTE='S'),
    
  FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),
    
  PED AS (SELECT ID_PEDIDO,
                  PEDCODIGO,
                   P.CLICODIGO,
                    CLINOMEFANT,
                     GCLCODIGO,
                      PEDDTEMIS,
                       SETOR,
                        PEDDTBAIXA,
                         PEDORIGEM,
                          TPCODIGO,
                           PEDAUTORIZOU,
                            PEDORIGEM ORIGEM FROM PEDID P
     INNER JOIN FIS F ON P.FISCODIGO1=F.FISCODIGO
      INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
       WHERE 
        PEDDTBAIXA 
         BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1) AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) AND
          PEDSITPED<>'C' AND PEDORIGEM='W'),
  
  AUX AS (SELECT PROCODIGO,PROCODIGO2,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2)CHAVE FROM PRODU
  WHERE MARCODIGO=189 AND PROTIPO<>'T')
  
    
  SELECT 
  PD.ID_PEDIDO,
    PEDDTBAIXA,
     CLICODIGO,
       GCLCODIGO,
        SETOR,
          PD.PROCODIGO,
           PDPDESCRICAO,
            PEDAUTORIZOU,
             ORIGEM,
               SUM(PDPQTDADE) QTD,
                SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
                
    FROM PDPRD PD
     INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
      INNER JOIN AUX A ON PD.PROCODIGO=A.PROCODIGO
        GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2
  ")  %>%
 ## mutate(PEDAUTORIZOU=as.character(PEDAUTORIZOU))  %>%
  mutate(CPF=sub("\\D+", '', PEDAUTORIZOU))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF))  %>% 
  mutate(OBS=paste0("INSIGNE"," ",if_else(is.na(GCLCODIGO),as.character(CLICODIGO),paste0('G', GCLCODIGO))," ",format(floor_date(Sys.Date(),"month")-month(1),"%m%y")))


## BONUS ===============================================================

RESULT_INSIGNE_0324 <-
  CP_INSIGNE_0324 %>% 
  mutate(LINHA = str_extract(PDPDESCRICAO, "(?<=\\bINSIGNE\\b\\s)\\w+")) %>% 
  left_join(.,VALORES_INSIGNE,by=c("LINHA"="NOME"))

##REMOVE DUPLICATES =====================================================


RESULT_INSIGNE_0324_2 <- RESULT_INSIGNE_0324 %>% filter(CLICODIGO!=151)


## INTERSECTION ========================================================

LIST_INSIGNE_0324_ALL <- left_join(RESULT_INSIGNE_0324_2,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>% 
  mutate(CPF3=ifelse(is.na(CPF2),CPF,CPF2)) 

## PAY ==================================================================

PAG_INSIGNE_0324 <-  LIST_INSIGNE_0324_ALL %>% filter(nchar(CPF3)==11) %>% group_by(CPF3,OBS) %>% 
  summarize(BONUS=sum(BONUS)) %>%  rename(CPF=CPF3)


## 151 ==================================================================================

RESULT_INSIGNE_0324_151 <- RESULT_INSIGNE_0324 %>% filter(CLICODIGO==151)


LIST_INSIGNE_0324_151 <- left_join(RESULT_INSIGNE_0324_151,BASE_CPF,by="CLICODIGO") %>%
  rename(CPF=CPF.x) %>% rename(CPF2=CPF.y) %>%
  mutate(CPF3=ifelse(CPF2=='NULL',CPF,CPF2)) %>%  
  mutate(CPF3=ifelse(nchar(gsub("[^[:alnum:]]", "", PEDAUTORIZOU)) < 6,'00773625941',CPF2))


PAG_INSIGNE_0324_151 <-  LIST_INSIGNE_0324_151 %>% group_by(CPF3,OBS) %>% 
  summarize(BONUS=sum(BONUS)) %>% rename(CPF=CPF3)


## PAGAMENTOS ======================================================================================================= 


PAG_INSIGNE_0324 <-  rbind( 
  PAG_INSIGNE_0324,
  PAG_INSIGNE_0324_151
) %>% mutate(PGTO_MINIMO=if_else(BONUS>=100,"S","N")) %>% 
  mutate(TIPO="INSIGNE") %>% rename(OBS=OBS)



PAG_INSIGNE_0324 %>% summarize(v=sum(BONUS))



## LISTAGEM FINAL  =============================================================================================================         

LIST_INSIGNE_0324 <-  rbind( 
  LIST_INSIGNE_0324_ALL,
  LIST_INSIGNE_0324_151
) %>%  .[,c(-12,-16)] %>% rename(CPF=CPF3)


## CREDITO CARTOES ======================================================================================================= 

CARTOES_ALELO <- 
  read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\2024\\CARTOES_ALELO.xlsx") %>% 
  mutate(CPF=sub("\\D+", '', CPF))  %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) 

cols2 <- c("NSERIE","CPF","NOME","CCUST","STATUS","PRODUTO","CADASTRO") 

CARTOES_ALELO <- CARTOES_ALELO %>% `colnames<-`(cols2)



CREDITO_CARTOES_INSIGNE_0324 <- left_join(PAG_INSIGNE_0324 %>%
                                            mutate(CPF=as.character(CPF)),CARTOES_ALELO %>%  
                                            filter(STATUS!="Cancelado") %>% 
                                            mutate(CPF=sub("\\D+", '',CPF)) %>% 
                                            mutate(CPF=sub("\\.", '',CPF)) %>% 
                                            mutate(CPF=sub("\\-", '',CPF)),by="CPF") 

View(CREDITO_CARTOES_INSIGNE_0324)


## EXCLUI SEM CARTAO E PGTO MINIMO

CREDITO_CARTOES_INSIGNE_0324_2 <- CREDITO_CARTOES_INSIGNE_0324 %>% 
  filter(!is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S')

View(CREDITO_CARTOES_INSIGNE_0324_2)



## CRIA BASE DE PAGAMENTO ============================================================

CREDITO_CARTOES_INSIGNE_0324_3 <- CREDITO_CARTOES_INSIGNE_0324_2  %>% 
  .[,c(6,1,3,2)] %>% 
  rename_at(1:4, ~ c("Número de Série","CPF","Valor da Carga","Observacao"))


View(CREDITO_CARTOES_INSIGNE_0324_3)  

# Get current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- toupper(format(Sys.Date()-months(1), "%b"))

# Construct file path with current year and month
file_path <- paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\CAMPANHAS ALELO\\", current_year, "\\", current_month, "\\CREDITO_CARTOES_INSIGNE_0324.csv")


# Write CSV with updated file path
write.csv2(CREDITO_CARTOES_INSIGNE_0324_3,
           file = file_path,
           row.names = FALSE,
           quote = FALSE)




## EMISSAO CARTAO ======================================================================================================= 


EMISSAO_CARTOES_INSIGNE_0324_1 <-
inner_join(
  
  CREDITO_CARTOES_INSIGNE_0324 %>% 
    filter(is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S') %>%  
    left_join(.,PARTICIPANTES_CAMPANHA %>% 
                mutate(CPF=as.character(CPF)),by="CPF")
  
  ,
  
  CREDITO_CARTOES_INSIGNE_0324 %>% 
             filter(is.na(NSERIE)) %>% filter(PGTO_MINIMO=='S') %>%  
             left_join(.,PARTICIPANTES_CAMPANHA %>% 
                         mutate(CPF=as.character(CPF)),by="CPF") %>% 
             group_by(CPF) %>% 
             summarise(`Carimbo de data/hora` = max(`Carimbo de data/hora`), 
                       CPF = CPF[which.max(`Carimbo de data/hora`)] , 
                       `NOME COMPLETO DO PARTICIPANTE`= `NOME COMPLETO DO PARTICIPANTE`[which.max(`Carimbo de data/hora`)])
  ,
  
  by=c("CPF","Carimbo de data/hora")) %>% as.data.frame() 


View(EMISSAO_CARTOES_INSIGNE_0324_1)

