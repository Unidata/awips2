#! /bin/bash
#
# This script will right pad any xxxid entries with spaces 
# if they are less than 3 characters long, if a padded entry
# does not already exist
#

# right pad all short xxxid entries if one doesn't already exist
/awips2/psql/bin/psql -U awips -d fxatext -c "
INSERT INTO public.textproductinfo (cccid, nnnid, xxxid, versionstokeep)
    (SELECT cccid, nnnid, rpad(xxxid,3), versionstokeep 
         FROM public.textproductinfo WHERE length(xxxid) < 3)
    ON CONFLICT DO NOTHING;
"

# delete all records with short xxxid entries
/awips2/psql/bin/psql -U awips -d fxatext -c "
DELETE FROM public.textproductinfo WHERE length(xxxid) < 3;
"