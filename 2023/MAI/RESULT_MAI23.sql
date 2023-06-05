
WITH CLI AS (SELECT DISTINCT C.CLICODIGO,
                              CLINOMEFANT,
                               ENDCODIGO,
                                GCLCODIGO,
                                 SETOR
                                  FROM CLIEN C
                                   LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                    LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA 
                                     WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                      E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                       WHERE CLICLIENTE='S'),
                               
                               
   FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
   
    
    PED AS (SELECT ID_PEDIDO,
                    EMPCODIGO,
                    TPCODIGO,
                     PEDDTBAIXA,
                      PEDAUTORIZOU,
                       P.CLICODIGO,
                        GCLCODIGO,
                         SETOR,
                          IIF(FISCODIGO IS NULL,0,1) VENDA,
                           CLINOMEFANT,
                            PEDORIGEM
                            FROM PEDID P
                             INNER JOIN FIS ON P.FISCODIGO1=FIS.FISCODIGO
                              INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO AND P.ENDCODIGO=C.ENDCODIGO
                               WHERE PEDDTBAIXA BETWEEN '01.05.2023' AND '31.05.2023' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N') ),
                               
      PROD AS (SELECT PROCODIGO,IIF(PROCODIGO2 IS NULL,PROCODIGO,PROCODIGO2)CHAVE,PROTIPO FROM PRODU)

    
      SELECT PD.ID_PEDIDO,
               PEDDTBAIXA,
                CLICODIGO,
                 GCLCODIGO,
                   SETOR,
                    PD.PROCODIGO,
                     PDPDESCRICAO,
                      PEDAUTORIZOU,
                             SUM(PDPQTDADE)QTD,
                              SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
                                FROM PDPRD PD
                                 INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                                  LEFT JOIN PROD PR ON PD.PROCODIGO=PR.PROCODIGO
                                   GROUP BY 1,2,3,4,5,6,7,8 ORDER BY ID_PEDIDO DESC
                                   
                                   
                                   