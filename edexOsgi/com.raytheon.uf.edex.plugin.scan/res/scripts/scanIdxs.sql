CREATE INDEX scan_icao_type_idx
  ON scan
  USING btree
  (icao COLLATE pg_catalog."default", type COLLATE pg_catalog."default");
