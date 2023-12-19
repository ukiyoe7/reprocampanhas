

WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE C.CLICODIGO IN (2241,1784,4442,1238,436,4418,2046,3820,162,886,4579,4500,3305,2242,2243,2246,4492,4498,4626,4585,4208)),

PED AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA BETWEEN
       '25.09.2023'
       AND '31.10.2023'
AND PEDSITPED <>'C'),

PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PED PE ON  P.ID_PEDIDO=PE.ID_PEDIDO
   WHERE PROCODIGO='PAP'),                 
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PED PE ON P.ID_PEDIDPROMOCAO=PE.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE MARCODIGO=24),

-- PRECISE

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, 15 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%PRECISE UHD%' AND 
                  PRODESCRICAO LIKE '%1.50%')) AND 
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO, 35 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%PRECISE UHD%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%PRECISE UHD%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
                   PRODESCRICAO NOT LIKE '%TGEN8%' AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL),
                 
                  
PROD3 AS (SELECT PROCODIGO,PRODESCRICAO, 35 BONUS FROM PRODU WHERE  
                  (PRODESCRICAO LIKE '%PRECISE UHD%' AND 
                  PRODESCRICAO LIKE '%1.50%') AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),                  

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO,55 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%PRECISE UHD%' AND 
                  PRODESCRICAO LIKE '%1.67%')) AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                  
-- NETWORK

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO,20 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%NETWORK%' AND PRODESCRICAO LIKE '%1.50%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                  
                  
PROD6 AS (SELECT PROCODIGO,PRODESCRICAO,40 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%NETWORK%' AND PRODESCRICAO LIKE '%1.50%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),                  
                  


PROD7 AS (SELECT PROCODIGO,PRODESCRICAO,40 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%KODAK NETWORK%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%KODAK NETWORK%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%' AND PRODESCRICAO NOT LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 

PROD8 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%KODAK NETWORK%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%KODAK NETWORK%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),                 

-- UNIQUE ================================================                 

PROD9 AS (SELECT PROCODIGO,PRODESCRICAO,25 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%KODAK UNIQUE%' AND PRODESCRICAO LIKE '%1.50%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 

PROD10 AS (SELECT PROCODIGO,PRODESCRICAO,45 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%KODAK UNIQUE%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%KODAK UNIQUE%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%' AND PRODESCRICAO NOT LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                                  
                 
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
      WHEN  PROD9.PROCODIGO IS NOT NULL THEN PROD9.BONUS
      WHEN  PROD10.PROCODIGO IS NOT NULL THEN PROD10.BONUS

      ELSE 0 END BONUS, 
        PD.PROCODIGO,
        PDPDESCRICAO,
        PEDAUTORIZOU,
          SUM(PDPQTDADE)QTD,
           SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
FROM
PDPRD PD
INNER JOIN PED p ON PD.ID_PEDIDO=P.ID_PEDIDO
INNER JOIN FIS F ON F.FISCODIGO=PD.FISCODIGO
INNER JOIN PROD  ON PD.PROCODIGO=PROD.PROCODIGO
LEFT JOIN PROD1 ON PD.PROCODIGO=PROD1.PROCODIGO
LEFT JOIN PROD2 ON PD.PROCODIGO=PROD2.PROCODIGO
LEFT JOIN PROD3 ON PD.PROCODIGO=PROD3.PROCODIGO
LEFT JOIN PROD4 ON PD.PROCODIGO=PROD4.PROCODIGO
LEFT JOIN PROD5 ON PD.PROCODIGO=PROD5.PROCODIGO
LEFT JOIN PROD6 ON PD.PROCODIGO=PROD6.PROCODIGO
LEFT JOIN PROD7 ON PD.PROCODIGO=PROD7.PROCODIGO
LEFT JOIN PROD8 ON PD.PROCODIGO=PROD8.PROCODIGO
LEFT JOIN PROD9 ON PD.PROCODIGO=PROD9.PROCODIGO
LEFT JOIN PROD10 ON PD.PROCODIGO=PROD10.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9 HAVING SUM(PDPQTDADE)>=2)

SELECT * FROM RESULT WHERE BONUS<>0



