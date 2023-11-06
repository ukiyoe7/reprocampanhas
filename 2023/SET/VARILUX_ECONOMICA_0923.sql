
WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','SR','R')),

CLI AS(SELECT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR FROM CLIEN C
       LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
       INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON 
       C.CLICODIGO=A.CLICODIGO WHERE (GCLCODIGO IN (148,175,257) OR C.CLICODIGO IN (986,3801,4469,291,4460,1923,2183,4244))),

PED AS (SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,GCLCODIGO,SETOR,CLINOMEFANT,PEDAUTORIZOU  
FROM PEDID 
INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
INNER JOIN CLI ON PEDID.CLICODIGO=CLI.CLICODIGO
WHERE 
PEDDTBAIXA BETWEEN
       '01.09.2023'
       AND '30.09.2023'
AND PEDSITPED <>'C'),

PED_PROMO_PAP AS (SELECT P.ID_PEDIDO ID_PEDIDO_PROMO FROM PDPRD P
 INNER JOIN PED PE ON  P.ID_PEDIDO=PE.ID_PEDIDO
   WHERE PROCODIGO='PAP'),                 
   
PED_PROMO_PLUGIN AS (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO FROM PEDIDPROMO P
   INNER JOIN PED PE ON P.ID_PEDIDPROMOCAO=PE.ID_PEDIDO),

PED_PROMO_UNION AS (SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PAP UNION
SELECT ID_PEDIDO_PROMO FROM PED_PROMO_PLUGIN),

PROD AS (SELECT PROCODIGO FROM PRODU WHERE (MARCODIGO=57 OR PRODESCRICAO LIKE '%EYEZEN%')),

-- EYEZEN

PROD1 AS (SELECT PROCODIGO,PRODESCRICAO, 20 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%ORMA%')) AND 
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),

PROD2 AS (SELECT PROCODIGO,PRODESCRICAO, 40 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
                   PRODESCRICAO NOT LIKE '%TGEN8%' AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL),
                 
                  
PROD3 AS (SELECT PROCODIGO,PRODESCRICAO, 50 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%AIR%')
             OR
             (PRODESCRICAO LIKE '%EYEZEN%' AND PRODESCRICAO LIKE '%AIR%'))
             AND
                   PRODESCRICAO NOT LIKE '%TGEN8%' AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL),                  

PROD4 AS (SELECT PROCODIGO,PRODESCRICAO, 60 BONUS FROM PRODU WHERE  
                  (PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%ORMA%') AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                  

PROD5 AS (SELECT PROCODIGO,PRODESCRICAO,100 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%1.67%')) AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),
                  
                  
PROD6 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
                  ((PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO LIKE '%AIR%')) AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL),                  
                  
-- LIBERTY

PROD7 AS (SELECT PROCODIGO,PRODESCRICAO,20 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 

PROD8 AS (SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),                 
                 

PROD9 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 

PROD10 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%LIBERTY%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                                  

-- COMFORT                 

PROD11 AS (SELECT PROCODIGO,PRODESCRICAO,40 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 
PROD12 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%' AND PRODESCRICAO NOT LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 
PROD13 AS (SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 
PROD14 AS (SELECT PROCODIGO,PRODESCRICAO,100 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%ORM%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 

PROD15 AS (SELECT PROCODIGO,PRODESCRICAO,140 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%1.74%'))
              AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
             
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 
                 
                 
                 
PROD16 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%COMFORT%' AND PRODESCRICAO LIKE '%AIR%')
             AND
                  (PRODESCRICAO LIKE '%TGEN8%' OR PRODESCRICAO LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 
                 

-- VARILUX E

PROD17 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%ORMA%')
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 

PROD18 AS (SELECT PROCODIGO,PRODESCRICAO,80 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 

PROD19 AS (SELECT PROCODIGO,PRODESCRICAO,50 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%AIR%')
             AND
             (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 

PROD20 AS (SELECT PROCODIGO,PRODESCRICAO,140 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%ORMA%')
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15), 

PROD21 AS (SELECT PROCODIGO,PRODESCRICAO,180 BONUS FROM PRODU WHERE  
             ((PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.67%')
             OR
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%1.74%'))
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND 
                 GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL AND GR1CODIGO=15),
                 
                 
PROD22 AS (SELECT PROCODIGO,PRODESCRICAO,60 BONUS FROM PRODU WHERE  
             (PRODESCRICAO LIKE '%VARILUX E%' AND PRODESCRICAO LIKE '%AIR%')
             AND
             (PRODESCRICAO LIKE '%TGEN8%') AND
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
      WHEN  PROD11.PROCODIGO IS NOT NULL THEN PROD11.BONUS
      WHEN  PROD12.PROCODIGO IS NOT NULL THEN PROD12.BONUS
      WHEN  PROD13.PROCODIGO IS NOT NULL THEN PROD13.BONUS 
      WHEN  PROD14.PROCODIGO IS NOT NULL THEN PROD14.BONUS
      WHEN  PROD15.PROCODIGO IS NOT NULL THEN PROD15.BONUS
      WHEN  PROD16.PROCODIGO IS NOT NULL THEN PROD16.BONUS
      WHEN  PROD17.PROCODIGO IS NOT NULL THEN PROD17.BONUS
      WHEN  PROD18.PROCODIGO IS NOT NULL THEN PROD18.BONUS
      WHEN  PROD19.PROCODIGO IS NOT NULL THEN PROD19.BONUS
      WHEN  PROD20.PROCODIGO IS NOT NULL THEN PROD20.BONUS
      WHEN  PROD21.PROCODIGO IS NOT NULL THEN PROD21.BONUS
      WHEN  PROD22.PROCODIGO IS NOT NULL THEN PROD22.BONUS

      ELSE 0 END BONUS, 
       
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
LEFT JOIN PROD7 ON PD.PROCODIGO=PROD7.PROCODIGO
LEFT JOIN PROD8 ON PD.PROCODIGO=PROD8.PROCODIGO
LEFT JOIN PROD9 ON PD.PROCODIGO=PROD9.PROCODIGO
LEFT JOIN PROD10 ON PD.PROCODIGO=PROD10.PROCODIGO
LEFT JOIN PROD11 ON PD.PROCODIGO=PROD11.PROCODIGO
LEFT JOIN PROD12 ON PD.PROCODIGO=PROD12.PROCODIGO
LEFT JOIN PROD13 ON PD.PROCODIGO=PROD13.PROCODIGO
LEFT JOIN PROD14 ON PD.PROCODIGO=PROD14.PROCODIGO
LEFT JOIN PROD15 ON PD.PROCODIGO=PROD15.PROCODIGO
LEFT JOIN PROD16 ON PD.PROCODIGO=PROD16.PROCODIGO
LEFT JOIN PROD17 ON PD.PROCODIGO=PROD17.PROCODIGO
LEFT JOIN PROD18 ON PD.PROCODIGO=PROD18.PROCODIGO
LEFT JOIN PROD19 ON PD.PROCODIGO=PROD19.PROCODIGO
LEFT JOIN PROD20 ON PD.PROCODIGO=PROD20.PROCODIGO
LEFT JOIN PROD21 ON PD.PROCODIGO=PROD21.PROCODIGO
LEFT JOIN PROD22 ON PD.PROCODIGO=PROD22.PROCODIGO
LEFT OUTER JOIN PED_PROMO_UNION ON PD.ID_PEDIDO=PED_PROMO_UNION.ID_PEDIDO_PROMO
WHERE PED_PROMO_UNION.ID_PEDIDO_PROMO IS NULL
GROUP BY 1,2,3,4,5,6,7,8 HAVING SUM(PDPQTDADE)>=2)

SELECT * FROM RESULT WHERE BONUS<>0



