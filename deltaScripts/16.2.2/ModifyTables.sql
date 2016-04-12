--Double the remarks sections in the following tables: 
--  location, riverstat, benchmark, gage, crest(cremark), lowwater(lwrem)

ALTER TABLE lowwater ALTER COLUMN lwrem TYPE character varying(160);
ALTER TABLE crest ALTER COLUMN cremark TYPE character varying(160);
ALTER TABLE gage ALTER COLUMN remark TYPE character varying(510);
ALTER TABLE benchmark ALTER COLUMN remark TYPE character varying(510);
ALTER TABLE riverstat ALTER COLUMN remark TYPE character varying(510);
ALTER TABLE location ALTER COLUMN lremark TYPE character varying(510);
