-- called by updateWarningTables.sh to alter the warning and practicewarning tables
-- and to create indexes and sequences for the activetable tables
DROP SEQUENCE IF EXISTS practice_activetableseq;
DROP SEQUENCE IF EXISTS activetableseq;
DROP INDEX IF EXISTS activetable_officeid_phensig_idx;
DROP INDEX IF EXISTS practice_activetable_officeid_phensig_idx
DROP INDEX IF EXISTS practicewarning_office_phensig_index
DROP INDEX IF EXISTS warning_office_phensig_index
ALTER TABLE warning DROP COLUMN IF EXISTS ugczones;
ALTER TABLE practicewarning DROP COLUMN IF EXISTS ugczones;

CREATE INDEX activetable_officeid_phensig_idx
  ON activetable
  USING btree
  (officeid COLLATE pg_catalog."default", phensig COLLATE pg_catalog."default");

CREATE INDEX practice_activetable_officeid_phensig_idx
  ON practice_activetable
  USING btree
  (officeid COLLATE pg_catalog."default", phensig COLLATE pg_catalog."default");

CREATE SEQUENCE activetableseq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE activetableseq
  OWNER TO awips;

CREATE SEQUENCE practice_activetableseq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
ALTER TABLE practice_activetableseq
  OWNER TO awips;

CREATE INDEX practicewarning_office_phensig_index
  ON practicewarning
  USING btree
  (officeid COLLATE pg_catalog."default", phensig COLLATE pg_catalog."default");

CREATE INDEX warning_office_phensig_index
  ON warning
  USING btree
  (officeid COLLATE pg_catalog."default", phensig COLLATE pg_catalog."default");


ALTER TABLE warning ADD COLUMN ugczones text;
ALTER TABLE practicewarning ADD COLUMN ugczones text;

