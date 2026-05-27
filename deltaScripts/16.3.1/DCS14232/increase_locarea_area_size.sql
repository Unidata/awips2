-- Update area column in locarea table of IHFS to hold up to 500 characters

ALTER TABLE locarea ALTER COLUMN area TYPE character varying(500);