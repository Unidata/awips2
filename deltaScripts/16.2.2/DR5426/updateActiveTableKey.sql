-- This script contains merge from DR 19213

BEGIN;
-- drop the old id column as primary key
ALTER TABLE activetable DROP CONSTRAINT IF EXISTS activetable_pkey;

-- create new issueyear column
ALTER TABLE activetable ADD COLUMN issueyear integer;

-- populate issueyear column
UPDATE activetable SET issueyear = EXTRACT(YEAR FROM issuetime);

-- add new primary key
ALTER TABLE activetable ALTER COLUMN issueyear SET NOT NULL;
ALTER TABLE activetable ALTER COLUMN pil SET NOT NULL;
ALTER TABLE activetable ADD CONSTRAINT activetable_pkey PRIMARY KEY (etn, issueyear, officeid, pil, phen, sig, ugczone);

COMMIT;
VACUUM FULL ANALYZE activetable;


-- now do the same for the practice_activetable
BEGIN;
-- drop the old id column as primary key
ALTER TABLE practice_activetable DROP CONSTRAINT IF EXISTS practice_activetable_pkey;

-- create new issueyear column
ALTER TABLE practice_activetable ADD COLUMN issueyear integer;

-- populate issueyear column
UPDATE practice_activetable SET issueyear = EXTRACT(YEAR FROM issuetime);

-- add new primary key
ALTER TABLE practice_activetable ALTER COLUMN issueyear SET NOT NULL;
ALTER TABLE practice_activetable ALTER COLUMN pil SET NOT NULL;
ALTER TABLE practice_activetable ADD CONSTRAINT practice_activetable_pkey PRIMARY KEY (etn, issueyear, officeid, pil, phen, sig, ugczone);

COMMIT;
VACUUM FULL ANALYZE practice_activetable;

