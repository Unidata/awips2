ALTER TABLE binlightning ADD COLUMN lightsource character varying(5);
ALTER TABLE binlightning ALTER COLUMN lightsource SET STORAGE EXTENDED;
ALTER TABLE binlightning ALTER COLUMN lightsource SET DEFAULT 'NLDN'::character varying;
