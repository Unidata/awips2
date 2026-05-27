BEGIN;
-- first remove any duplicate records
DROP TABLE IF EXISTS t_deleteIds;
CREATE TEMP TABLE t_deleteIds (id int);

INSERT INTO t_deleteIds(id) (
SELECT id FROM (
  SELECT id, 
  ROW_NUMBER() OVER(PARTITION BY officeid, phen, sig, etn, ugczone ORDER BY issuetime DESC) AS Row
  FROM activetable
) dups
WHERE dups.Row > 1);

DELETE FROM activetable a using t_deleteIds t WHERE a.id = t.id;
COMMIT;

BEGIN;
-- drop the old id column as primary key
ALTER TABLE activetable DROP CONSTRAINT IF EXISTS activetable_pkey;
ALTER TABLE activetable DROP COLUMN IF EXISTS id;
DROP SEQUENCE IF EXISTS activetableseq;

-- set proper length on several columns
ALTER TABLE activetable ALTER COLUMN act TYPE character varying(3);
ALTER TABLE activetable ALTER COLUMN wmoid TYPE character varying(22);
ALTER TABLE activetable ALTER COLUMN vtecstr TYPE character varying(48);
ALTER TABLE activetable ALTER COLUMN productclass TYPE character varying(1);
ALTER TABLE activetable ALTER COLUMN locationid TYPE character varying(5);
ALTER TABLE activetable ALTER COLUMN floodseverity TYPE character varying(1);
ALTER TABLE activetable ALTER COLUMN immediatecause TYPE character varying(2);
ALTER TABLE activetable ALTER COLUMN officeid TYPE character varying(4);
ALTER TABLE activetable ALTER COLUMN phen TYPE character varying(2);
ALTER TABLE activetable ALTER COLUMN sig TYPE character varying(1);
ALTER TABLE activetable ALTER COLUMN ugczone TYPE character varying(6);

-- add new primary key
ALTER TABLE activetable ALTER COLUMN officeid SET NOT NULL;
ALTER TABLE activetable ALTER COLUMN phen SET NOT NULL;
ALTER TABLE activetable ALTER COLUMN sig SET NOT NULL;
ALTER TABLE activetable ALTER COLUMN etn SET NOT NULL;
ALTER TABLE activetable ALTER COLUMN ugczone SET NOT NULL;
ALTER TABLE activetable ADD CONSTRAINT activetable_pkey PRIMARY KEY (etn, officeid, phen, sig, ugczone);
COMMIT;
VACUUM FULL ANALYZE activetable;


-- now do the same for the practice_activetable
BEGIN;
-- first remove any duplicate records
DROP TABLE IF EXISTS t_deleteIds;
CREATE TEMP TABLE t_deleteIds (id int);

INSERT INTO t_deleteIds(id) (
SELECT id FROM (
  SELECT id, 
  ROW_NUMBER() OVER(PARTITION BY officeid, phen, sig, etn, ugczone ORDER BY issuetime DESC) AS Row
  FROM practice_activetable
) dups
WHERE dups.Row > 1);

DELETE FROM practice_activetable a using t_deleteIds t WHERE a.id = t.id;
COMMIT;

BEGIN;
-- drop the old id column as primary key
ALTER TABLE practice_activetable DROP CONSTRAINT IF EXISTS practice_activetable_pkey;
ALTER TABLE practice_activetable DROP COLUMN IF EXISTS id;
DROP SEQUENCE IF EXISTS practice_activetableseq;

-- set proper length on several columns
ALTER TABLE practice_activetable ALTER COLUMN act TYPE character varying(3);
ALTER TABLE practice_activetable ALTER COLUMN wmoid TYPE character varying(22);
ALTER TABLE practice_activetable ALTER COLUMN vtecstr TYPE character varying(48);
ALTER TABLE practice_activetable ALTER COLUMN productclass TYPE character varying(1);
ALTER TABLE practice_activetable ALTER COLUMN locationid TYPE character varying(5);
ALTER TABLE practice_activetable ALTER COLUMN floodseverity TYPE character varying(1);
ALTER TABLE practice_activetable ALTER COLUMN immediatecause TYPE character varying(2);
ALTER TABLE practice_activetable ALTER COLUMN officeid TYPE character varying(4);
ALTER TABLE practice_activetable ALTER COLUMN phen TYPE character varying(2);
ALTER TABLE practice_activetable ALTER COLUMN sig TYPE character varying(1);
ALTER TABLE practice_activetable ALTER COLUMN ugczone TYPE character varying(6);

-- add new primary key
ALTER TABLE practice_activetable ALTER COLUMN officeid SET NOT NULL;
ALTER TABLE practice_activetable ALTER COLUMN phen SET NOT NULL;
ALTER TABLE practice_activetable ALTER COLUMN sig SET NOT NULL;
ALTER TABLE practice_activetable ALTER COLUMN etn SET NOT NULL;
ALTER TABLE practice_activetable ALTER COLUMN ugczone SET NOT NULL;
ALTER TABLE practice_activetable ADD CONSTRAINT practice_activetable_pkey PRIMARY KEY (etn, officeid, phen, sig, ugczone);
COMMIT;
VACUUM FULL ANALYZE practice_activetable;

DROP TABLE IF EXISTS t_deleteIds;

