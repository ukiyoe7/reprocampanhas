## G139 SCHROEDER
# Rebate 7% para vendedoras e 3% para montador

CP_G139_0722 <-dbGetQuery(con2,"
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,GCLCODIGO,CLINOMEFANT,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
       WHERE GCLCODIGO=139),

PD AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,CLINOMEFANT,SETOR,GCLCODIGO,PEDAUTORIZOU 
       FROM PEDID 
       INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
       INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
       WHERE
       PEDDTBAIXA
       BETWEEN DATEADD(MONTH, -1, CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE) + 1)
       AND CURRENT_DATE - EXTRACT(DAY FROM CURRENT_DATE)
       AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N'))


SELECT 
 PDPRD.ID_PEDIDO,
  PEDDTBAIXA,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
     PROCODIGO,
      PDPDESCRICAO,
       PEDAUTORIZOU,
       SUM(PDPQTDADE) QTD,
       SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA,
       SUM(PDPUNITLIQUIDO*PDPQTDADE)*0.07 BONUS
       FROM
       PDPRD
       INNER JOIN PD ON PDPRD.ID_PEDIDO=PD.ID_PEDIDO
       GROUP BY 1,2,3,4,5,6,7,8") 

View(CP_G139_0722)
CP_G139_0722 %>%  summarize(v=sum(VRVENDA)) # total sales
CP_G139_0722 %>%  summarize(v=sum(VRVENDA)*0.07) # vendedoras 
CP_G139_0722 %>%  summarize(v=sum(VRVENDA)*0.03) # montador
sum(CP_G139_0722 %>%  summarize(v=sum(VRVENDA)*0.07),CP_G139_0722 %>% summarize(v=sum(VRVENDA)*0.03)) #total

## OBS

OBS_G139 <- paste0("SCHROEDER ","G",CP_G139_0722 %>% 
                     distinct(GCLCODIGO)," ",format(floor_date(Sys.Date()-months(1),"month"),"%m/%y"))


### Cria Planilha identificação CPFs

CP_G139_0722_IPEDIDOS <- CP_G139_0722 %>% group_by(ID_PEDIDO,PEDDTBAIXA) %>% 
  summarize(VRVENDA=sum(VRVENDA)) %>% 
  as.data.frame() %>% `colnames<-`(c("ID_PEDIDO","DATA","VALOR.VENDA")) %>% 
  mutate(DATA=format(DATA,"%d/%m/%y")) 

range_write("1AJrGLWrYvg1-_daZ8VptWApdA8ln0LWdcpF3-6uI8IM",data=CP_G139_0722_IPEDIDOS,sheet = "JUN22",
            range = "A1") 


View(CP_G139_0722_IPEDIDOS)

## obtem dados planilha identificação CPFs


CPF_G139_CPF_0722 <- read_sheet("1AJrGLWrYvg1-_daZ8VptWApdA8ln0LWdcpF3-6uI8IM",sheet = "JUL22") %>% 
  as.data.frame() %>% 
  mutate(CPF=sub("\\D+", '',CPF)) %>% 
  mutate(CPF=sub("\\.", '',CPF)) %>% 
  mutate(CPF=sub("\\-", '',CPF))

View(CPF_G139_CPF_0722)


#lista pedidos

LIST_G139_0722 <- inner_join(CP_G139_0722,CPF_G139_CPF_0722,by="ID_PEDIDO") %>% 
  mutate(OBS=OBS_G139) %>% 
  .[,c(-12,-13,-14)]

View(LIST_G139_0722)

LIST_G139_0722 %>% summarize(BONUS=sum(BONUS)) 

### pagamentos


# Calculo vendedoras

PAG_G139_0722_VENDEDORAS <- LIST_G139_0722 %>% group_by(CPF) %>% summarize(BONUS=sum(BONUS)) 

#calculo com montador

PAG_G139_0722_MONTADOR <- data.frame(CPF=c("88717020930"),BONUS=(CP_G139_0722 %>% 
                                                                   summarize(BONUS=sum(VRVENDA)*0.03))) 


PAG_G139_0722 <- rbind(PAG_G139_0722_VENDEDORAS,PAG_G139_0722_MONTADOR) %>%  mutate(OBS=OBS_G139)

View(PAG_G139_0722)

PAG_G139_0722 %>% summarize(BONUS=sum(BONUS)) 