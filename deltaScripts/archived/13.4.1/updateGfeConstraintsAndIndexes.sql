-- Called by normalizeGfe.sh to dop and add constraints

ALTER TABLE gfe DROP CONSTRAINT gfe_datauri_key;

DROP INDEX IF EXISTS gfeParmTime_idx;

DROP INDEX IF EXISTS gfedatauri_idx;

DROP INDEX IF EXISTS gfefcsttimeindex;

ALTER TABLE gfe DROP COLUMN IF EXISTS parmname;

ALTER TABLE gfe DROP COLUMN IF EXISTS parmlevel;

ALTER TABLE gfe DROP COLUMN IF EXISTS dbid;

ALTER TABLE gfe DROP COLUMN IF EXISTS parmid;

ALTER TABLE gfe ADD CONSTRAINT fk18f667bab05cc FOREIGN KEY (parmid_id)
      REFERENCES gfe_parmid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE;

ALTER TABLE gfe ADD CONSTRAINT gfe_parmid_id_rangestart_rangeend_reftime_forecasttime_key
      UNIQUE (parmid_id, rangestart, rangeend, reftime, forecasttime);

ALTER TABLE gfe_gridhistory DROP CONSTRAINT fk66434335e416514f;

ALTER TABLE gfe_gridhistory RENAME COLUMN key TO id;

ALTER TABLE gfe_gridhistory RENAME COLUMN parent to parent_id;

ALTER TABLE gfe_gridhistory ADD CONSTRAINT fk664343359ad1f975 FOREIGN KEY (parent_id)
      REFERENCES gfe (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE;

DROP TABLE IF EXISTS gfelocktable;
