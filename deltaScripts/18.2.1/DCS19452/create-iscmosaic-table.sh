#!/usr/bin/sh

# DCS 19452: Add new iscmosaicjob and iscmosaicparm tables

psql -U awipsadmin -d metadata -c '
CREATE TABLE iscmosaicjob (
    id integer NOT NULL,
    args text,
    inuse boolean NOT NULL,
    lastuse timestamp without time zone NOT NULL,
    leader integer NOT NULL,
    prepared boolean NOT NULL,
    site character varying(4) NOT NULL
);

ALTER TABLE ONLY iscmosaicjob
    ADD CONSTRAINT iscmosaicjob_pkey PRIMARY KEY (id);

CREATE INDEX iscmosaicjob_leader ON iscmosaicjob USING btree (leader);

CREATE INDEX iscmosaicjob_prepared ON iscmosaicjob USING btree (prepared);

CREATE SEQUENCE iscmosaicjobseq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE iscmosaicparm (
    job_id integer NOT NULL,
    lockname character varying(255) NOT NULL
);

ALTER TABLE ONLY iscmosaicparm
    ADD CONSTRAINT iscmosaicparm_pkey PRIMARY KEY (job_id, lockname);

ALTER TABLE ONLY iscmosaicparm
    ADD CONSTRAINT fk_iscmosaicparm_to_iscmosaicjob FOREIGN KEY (job_id) REFERENCES iscmosaicjob(id);

GRANT SELECT,INSERT,DELETE,TRIGGER,TRUNCATE,UPDATE ON TABLE iscmosaicjob TO awips;
GRANT ALL ON SEQUENCE iscmosaicjobseq TO awips;
GRANT SELECT,INSERT,DELETE,TRIGGER,TRUNCATE,UPDATE ON TABLE iscmosaicparm TO awips;
' \
    && echo 'Database update for DCS 19452 has completed successfully!' \
    || echo 'Database update for DCS 19452 has failed!'
