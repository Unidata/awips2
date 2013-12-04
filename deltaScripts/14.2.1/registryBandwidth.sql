CREATE TABLE datadeliveryregistrybandwidth
(
  timeperiod integer NOT NULL,
  bytes integer NOT NULL,
  CONSTRAINT datadeliveryregistrybandwidth_pkey PRIMARY KEY (timeperiod)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE datadeliveryregistrybandwidth
  OWNER TO awips;
