-- Index: convsigmet_section_parentid_idex

-- DROP INDEX convsigmet_section_parentid_idex;

CREATE INDEX convsigmet_section_parentid_idex
  ON convsigmet_section
  USING btree
  (parentid);

-- Index: convsigmet_location_parentid_idex

-- DROP INDEX convsigmet_location_parentid_idex;

CREATE INDEX convsigmet_location_parentid_idex
  ON convsigmet_location
  USING btree
  (parentid);

-- Index: airmet_report_parentid_idex

-- DROP INDEX airmet_report_parentid_idex;

CREATE INDEX airmet_report_parentid_idex
  ON airmet_report
  USING btree
  (parentid);

-- Index: airmet_location_parentid_idex

-- DROP INDEX airmet_location_parentid_idex;

CREATE INDEX airmet_location_parentid_idex
  ON airmet_location
  USING btree
  (parentid);
