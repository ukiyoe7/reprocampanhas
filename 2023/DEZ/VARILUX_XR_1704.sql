
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R') OR FISCODIGO IN ('5.91V','6.91V')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE C.CLICODIGO=1704),

PED AS (SELECT ID_PEDIDO,PEDDTEMIS,P.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID P
INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
WHERE 
PEDDTEMIS BETWEEN
       '01.09.2023'
       AND '31.10.2023'
AND PEDSITPED <>'C'),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%XR%'),

TGEN8 AS (SELECT PROCODIGO FROM PRODU WHERE MARCODIGO=57 AND PRODESCRICAO LIKE '%TGEN8%'),

                 
RESULT AS (                 
SELECT 
PD.ID_PEDIDO,
  PEDDTEMIS,
   CLICODIGO,
    GCLCODIGO,
     SETOR,
        PD.PROCODIGO,
         IIF(T.PROCODIGO IS NOT NULL,1,0) TRANSITIONS,
         PDPDESCRICAO,
          PEDAUTORIZOU,
           PD.FISCODIGO,
            SUM(PDPQTDADE)QTD,
             SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED p ON PD.ID_PEDIDO=P.ID_PEDIDO
INNER JOIN FIS F ON PD.FISCODIGO=F.FISCODIGO
INNER JOIN PROD  ON PD.PROCODIGO=PROD.PROCODIGO
LEFT JOIN TGEN8 T ON T.PROCODIGO=PD.PROCODIGO
GROUP BY 1,2,3,4,5,6,7,8,9,10 HAVING SUM(PDPQTDADE)>=2)

SELECT * FROM RESULT



