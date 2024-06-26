
WITH PED AS 
  (SELECT ID_PEDIDO
    FROM PEDID P
    WHERE 
    PEDDTEMIS BETWEEN DATEADD(-365 DAY TO CURRENT_DATE) AND 'TODAY'),

PED_PROMO_PAP AS
(
SELECT P1.ID_PEDIDO ID_PEDIDO_PROMO 
    FROM PDPRD P1
    INNER JOIN PED ON P1.ID_PEDIDO=PED.ID_PEDIDO
    WHERE PROCODIGO='PAP'),
  
  PED_PROMO_PLUGIN AS 
  (SELECT ID_PEDIDPROMOCAO ID_PEDIDO_PROMO 
    FROM PEDIDPROMO P2
    INNER JOIN PED ON P2.ID_PEDIDPROMOCAO=PED.ID_PEDIDO),
    
    PED_PROMO_CONECTA AS (SELECT P3.ID_PEDIDO ID_PEDIDO_PROMO 
                               FROM PDINFOPROMO P3
                                INNER JOIN PED ON P3.ID_PEDIDO=PED.ID_PEDIDO
                                 WHERE PIPPAR=2)
                                 
SELECT ID_PEDIDO_PROMO ID_PEDIDO, '1' PROMO
                          FROM PED_PROMO_PAP UNION
                           SELECT ID_PEDIDO_PROMO ID_PEDIDO, '1' PROMO 
                            FROM PED_PROMO_PLUGIN UNION
                             SELECT ID_PEDIDO_PROMO ID_PEDIDO, '1' PROMO
                              FROM PED_PROMO_CONECTA