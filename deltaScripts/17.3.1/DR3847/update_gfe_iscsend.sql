DROP TABLE IF EXISTS gfe_iscsend;

CREATE TABLE gfe_iscsend
(
  key integer NOT NULL,
  inserttime timestamp without time zone,
  state character varying(10) NOT NULL,
  rangeend timestamp without time zone,
  rangestart timestamp without time zone,
  xmldest text,
  parmid_id integer NOT NULL,
  CONSTRAINT gfe_iscsend_pkey PRIMARY KEY (key),
  CONSTRAINT fk_gfe_iscsend_to_gfe_parmid FOREIGN KEY (parmid_id)
      REFERENCES gfe_parmid (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT uk_gfe_iscsend UNIQUE (parmid_id, rangestart, rangeend, state, xmldest)
)
WITH (
  OIDS=FALSE
);

CREATE INDEX iscsendinserttimeindex
  ON gfe_iscsend
  USING btree
  (inserttime);

