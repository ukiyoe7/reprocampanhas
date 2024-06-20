-- SQL MODELOS DE CAMPANHAS
-- ULTIMA ATUALIZAÇÃO 13.06.2024


-- CTE CAMPANHAS
WITH CAMPANHAS AS (
    SELECT CLICODIGO, P.ID_PROMO, DESCRICAO AS CAMPANHA
    FROM CLIPROMO C
    LEFT JOIN PROMO P ON C.ID_PROMO = P.ID_PROMO
    WHERE DESCRICAO LIKE '%ALELO%' OR DESCRICAO LIKE '%INSIGNE%'),

-- CTE PRODUTOS   

    PRODUTOS AS (

------------------ VARILUX ECONOMICA

                  -- EYEZEN
                  
                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%EYEZEN%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 60 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%EYEZEN%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  --LIBERTY
                  
                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%LIBERTY%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 60 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE
                  PRODESCRICAO LIKE '%LIBERTY%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  -- COMFORT                 

                  SELECT PROCODIGO,PRODESCRICAO, 40 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%COMFORT%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  SELECT PROCODIGO,PRODESCRICAO, 100 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%COMFORT%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  -- PHYSIO

                  SELECT PROCODIGO,PRODESCRICAO, 50 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PHYSIO%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  SELECT PROCODIGO,PRODESCRICAO, 120 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PHYSIO%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  -- VARILUX E
                  
                  SELECT PROCODIGO,PRODESCRICAO, 60 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%VARILUX E%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  SELECT PROCODIGO,PRODESCRICAO, 140 BONUS,'ALELO VARILUX ECONOMICA' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%VARILUX E%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

----------------- VARILUX PREMIUM

                  -- XR DESIGN
                  
                  SELECT PROCODIGO,PRODESCRICAO, 80 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR DESIGN%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 180 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE  
                   PRODESCRICAO LIKE '%XR DESIGN%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  -- XR TRACK

                  SELECT PROCODIGO,PRODESCRICAO, 100 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%XR TRACK%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  SELECT PROCODIGO,PRODESCRICAO, 220 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%XR TRACK%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  -- XR PRO                

                  SELECT PROCODIGO,PRODESCRICAO, 120 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%XR PRO%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION

                  SELECT PROCODIGO,PRODESCRICAO, 260 BONUS,'ALELO VARILUX PREMIUM' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%XR PRO%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%') AND
                  LEFT(PROCODIGO,2)='LD' AND
                  GR1CODIGO<>17 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                      
                  
                  
------------------ INSIGNE

                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%ONE%' AND 
                  MARCODIGO=189
             
                  UNION
                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%MONO%' AND 
                  MARCODIGO=189
                  
                  UNION
                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%RELAX%' AND 
                  MARCODIGO=189
                  
                  UNION
                  SELECT PROCODIGO,PRODESCRICAO, 20 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%OFFICE%' AND 
                  MARCODIGO=189
                  
                  UNION
                  SELECT PROCODIGO,PRODESCRICAO, 40 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%MORE%' AND 
                  MARCODIGO=189 UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 60 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PERFECT%' AND 
                  MARCODIGO=189 UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 100 BONUS,'PROMO INSIGNE' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%VISION%' AND 
                  MARCODIGO=189 UNION
                  
------------------ MARCAS REPRO
                  -- IMAGEM

                  SELECT PROCODIGO,PRODESCRICAO, 10 BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE  
                  (PRODESCRICAO LIKE '%IMAGEM%' AND 
                  (PRODESCRICAO NOT LIKE '%TGEN8%') AND
                  (PRODESCRICAO NOT LIKE '%TRANS%') AND
                  (PRODESCRICAO NOT LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL) UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 40 BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%IMAGEM%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                   

                  -- UZ+

                  SELECT PROCODIGO, PRODESCRICAO, 15 AS BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE 
                  (PRODESCRICAO LIKE '%UZ+%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  PRODESCRICAO NOT LIKE '%FOTO%') AND
                  GR2CODIGO = 1 AND 
                  PROSITUACAO = 'A' AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 50 BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%UZ+%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                   

                  -- ACTUALITE

                  SELECT PROCODIGO, PRODESCRICAO, 25 AS BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE 
                  (PRODESCRICAO LIKE '%ACTUALITE%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  PRODESCRICAO NOT LIKE '%FOTO%') AND
                  GR2CODIGO = 1 AND 
                  PROSITUACAO = 'A' AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 70 BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%ACTUALITE%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                  
                  
                  -- AVANCE

                  SELECT PROCODIGO, PRODESCRICAO, 35 AS BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE 
                  (PRODESCRICAO LIKE '%AVANCE%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  PRODESCRICAO NOT LIKE '%TRANS%' AND
                  PRODESCRICAO NOT LIKE '%FOTO%') AND
                  GR2CODIGO = 1 AND 
                  PROSITUACAO = 'A' AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 90 BONUS,'ALELO MARCAS REPRO' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%AVANCE%' AND 
                  (PRODESCRICAO LIKE '%TGEN8%' OR
                  PRODESCRICAO LIKE '%TRANS%' OR
                  PRODESCRICAO LIKE '%FOTO%') AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION 
                  
------------------ KODAK
                  -- PRECISE

                  SELECT PROCODIGO,PRODESCRICAO, 10 BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PRECISE%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 40 BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%PRECISE%' AND 
                  PRODESCRICAO LIKE '%TGEN8%' AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                   

                  -- NETWORK

                  SELECT PROCODIGO, PRODESCRICAO, 15 AS BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%NETWORK%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  GR2CODIGO = 1 AND 
                  PROSITUACAO = 'A' AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 50 BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%NETWORK%' AND 
                  PRODESCRICAO LIKE '%TGEN8%' AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL UNION                   

                  -- UNIQUE

                  SELECT PROCODIGO, PRODESCRICAO, 25 AS BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE 
                  PRODESCRICAO LIKE '%UNIQUE%' AND 
                  PRODESCRICAO NOT LIKE '%TGEN8%' AND
                  GR2CODIGO = 1 AND 
                  PROSITUACAO = 'A' AND 
                  PROCODIGO2 IS NULL UNION
                  
                  SELECT PROCODIGO,PRODESCRICAO, 70 BONUS,'ALELO KODAK' CAMPANHA FROM PRODU WHERE  
                  PRODESCRICAO LIKE '%UNIQUE%' AND 
                  PRODESCRICAO LIKE '%TGEN8%' AND
                  GR2CODIGO=1 AND 
                  PROSITUACAO='A'AND 
                  PROCODIGO2 IS NULL
                  )
                  
                 
-- FINAL SELECT   
                  
SELECT C.CLICODIGO, P.PROCODIGO,P.PRODESCRICAO,'W' PEDORIGEM, 2 QTD, 0 PROMO, 1 VENDA,P.CAMPANHA, P.BONUS
FROM CAMPANHAS C
JOIN PRODUTOS P ON C.CAMPANHA = P.CAMPANHA;                  

