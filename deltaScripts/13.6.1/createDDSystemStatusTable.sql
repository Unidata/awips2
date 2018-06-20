CREATE TABLE IF NOT EXISTS ddsystemstatus
(
  name character varying(255) NOT NULL,
  systemtype character varying(255) NOT NULL,
  status character varying(255),
  CONSTRAINT ddsystemstatus_pkey PRIMARY KEY (name, systemtype)
)