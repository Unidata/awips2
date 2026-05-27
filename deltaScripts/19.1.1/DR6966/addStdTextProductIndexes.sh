#! /bin/bash
# Adds indexes to stdTextProducts and practiceStdTextProducts to 
# improve purge performance
/awips2/psql/bin/psql -U awipsadmin -d fxatext -c "
DROP INDEX IF EXISTS public.textafosidreftime_idx;
CREATE INDEX textafosidreftime_idx
  ON public.stdtextproducts
  USING btree
  (cccid COLLATE pg_catalog."default", nnnid COLLATE pg_catalog."default", xxxid COLLATE pg_catalog."default", reftime);

DROP INDEX IF EXISTS public.practicetextafosidreftime_idx;
CREATE INDEX practicetextafosidreftime_idx
  ON public.practicestdtextproducts
  USING btree
  (cccid COLLATE pg_catalog."default", nnnid COLLATE pg_catalog."default", xxxid COLLATE pg_catalog."default", reftime);
"