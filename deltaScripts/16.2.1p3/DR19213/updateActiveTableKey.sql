BEGIN;
-- drop the old id column as primary key
ALTER TABLE activetable DROP CONSTRAINT IF EXISTS activetable_pkey;

-- add new primary key
ALTER TABLE activetable ALTER COLUMN pil SET NOT NULL;
ALTER TABLE activetable ADD CONSTRAINT activetable_pkey PRIMARY KEY (etn, officeid, pil, phen, sig, ugczone);

COMMIT;


-- now do the same for the practice_activetable
BEGIN;
-- drop the old id column as primary key
ALTER TABLE practice_activetable DROP CONSTRAINT IF EXISTS practice_activetable_pkey;

-- add new primary key
ALTER TABLE activetable ALTER COLUMN pil SET NOT NULL;
ALTER TABLE practice_activetable ADD CONSTRAINT practice_activetable_pkey PRIMARY KEY (etn, officeid, pil, phen, sig, ugczone);

COMMIT;

