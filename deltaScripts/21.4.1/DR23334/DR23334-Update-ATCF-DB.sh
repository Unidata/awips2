#!/bin/sh
# DCS 21882/DR 23334 - Add checks for region and cylone number in atcf_procedures.sql function "update_storm"
#             "update_storm" so storma could be properly renamed or corrected. The script should be run "dv1"
#             on which postgres is run.
# Author: jwu
# Oct. 27th, 2022
. /awips2/etc/environment || exit 1
set -e

echo "Update ATCF DB function update_storm ..." >&2
a2dbauth psql -U awipsadmin -d metadata <<'EOF'

CREATE OR REPLACE FUNCTION
  atcf.update_storm
  (
     _old_storm_id VARCHAR,
     _new_region VARCHAR,
     _new_cyclonenum INTEGER,
     _new_storm_name VARCHAR,
     _new_storm_state VARCHAR,
     _new_sub_region VARCHAR,
     _new_mover VARCHAR,
     _new_wt_num INTEGER
   )
RETURNS VARCHAR AS
$BODY$
DECLARE
    _old_region VARCHAR;--
    _old_year Integer;--
    _old_cyclonenum Integer;--
    _old_storm_name VARCHAR;--
    _old_storm_state VARCHAR;--
    _old_sub_region VARCHAR;--
    _old_mover VARCHAR;--
    _old_wt_num INTEGER;--
    _update_deck_flag Integer;--
    _new_storm_id VARCHAR;--
BEGIN
    -- Retrieve storm key columns
    SELECT region, cyclonenum, year, stormname, stormstate, subregion, mover, wtnum
    INTO _old_region, _old_cyclonenum, _old_year, _old_storm_name, _old_storm_state, _old_sub_region, _old_mover, _old_wt_num
    FROM atcf.storm WHERE stormid=_old_storm_id; --

    --Check if given sandbox exists
    IF(_old_region is NULL) THEN
      RAISE EXCEPTION 'Given old Storm is not existing'; --
    END IF; --

    -- initialize variables
    SELECT -1 INTO _update_deck_flag;--
    SELECT 'NaN' INTO _new_storm_id;--

    -- check if deck records need to be updated
    IF (_new_storm_name != 'NaN' OR _new_region != 'NaN' OR _new_cyclonenum > 0) THEN
      -- Any change on these three fields required deck level updates
      SELECT 1 INTO _update_deck_flag;--

      -- either region or cyclonenum change will change the stormid
      IF (_new_region != 'NaN' OR _new_cyclonenum > 0) THEN
        IF (_new_region = 'NaN') THEN
          SELECT _old_region INTO _new_region;--
        END IF;--
        IF (_new_cyclonenum <= 0) THEN
          SELECT _old_cyclonenum INTO _new_cyclonenum;--
        END IF;--

        -- create new storm ID
        SELECT trim(_new_region)||trim(to_char(_new_cyclonenum, '09'))||_old_year
        INTO _new_storm_id;--
      END IF;--
    END IF;--

    -- check if new storm id is already existing
    IF (_new_storm_id != 'NaN') THEN
      IF EXISTS (SELECT 1 FROM atcf.storm WHERE stormid=_new_storm_id) THEN
        RAISE EXCEPTION 'Updating region or cyclonenum are conflicted with a existing storm!'; --
      END IF; --
    END IF;--

    -- if any field is NaN or -1, set to old value
    IF (_new_region = 'NaN') THEN
      SELECT _old_region INTO _new_region;--
    END IF;--

    IF (_new_cyclonenum <= 0) THEN
      SELECT _old_cyclonenum INTO _new_cyclonenum;--
    END IF;--

    IF (_new_storm_name = 'NaN') THEN
      SELECT _old_storm_name INTO _new_storm_name;--
    END IF;--

    IF (_new_storm_state = 'NaN') THEN
      SELECT _old_storm_state INTO _new_storm_state;--
    END IF;--

    IF (_new_sub_region = 'NaN') THEN
      SELECT _old_sub_region INTO _new_sub_region;--
    END IF;--

    IF (_new_mover = 'NaN') THEN
      SELECT _old_mover INTO _new_mover;--
    END IF;--

    IF (_new_wt_num < 0) THEN
      SELECT _old_wt_num INTO _new_wt_num;--
    END IF;--

    IF (_update_deck_flag > 0) THEN

      --Update storm
      IF (_new_storm_id != 'NaN') THEN
        UPDATE atcf.storm
        SET stormid=_new_storm_id, region=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name, stormstate=_new_storm_state, subregion=_new_sub_region, mover=_new_mover, wtnum=_new_wt_num
        WHERE region=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--
      ELSE
        UPDATE atcf.storm
        SET stormname=_new_storm_name, stormstate=_new_storm_state, subregion=_new_sub_region, mover=_new_mover, wtnum=_new_wt_num
        WHERE region=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--
      END IF;--

      -- Update decks
      UPDATE atcf.adeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.bdeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.edeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.fdeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.fst
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox_adeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox_bdeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox_edeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox_fdeck
      SET basin=_new_region, cyclonenum=_new_cyclonenum
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox_fst
      SET basin=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE basin=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--

      UPDATE atcf.sandbox
      SET region=_new_region, cyclonenum=_new_cyclonenum, stormname=_new_storm_name
      WHERE region=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--
    ELSE
      -- none of stormname, region or cyclonenum being changed, only storm table need to be updated
      UPDATE atcf.storm
      SET stormname=_new_storm_name, stormstate=_new_storm_state, subregion=_new_sub_region, mover=_new_mover, wtnum=_new_wt_num
      WHERE region=_old_region AND cyclonenum=_old_cyclonenum AND year=_old_year;--
    END IF;--

    -- caller should check the return then notify accordingly
    IF (_new_storm_id = 'NaN') THEN
      RETURN _old_storm_id;--
    ELSE
      RETURN _new_storm_id;--
    END IF;--

END;--
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION atcf.update_storm(varchar, varchar, integer, varchar, varchar, varchar, varchar, integer)
  OWNER TO awipsadmin;
GRANT EXECUTE ON FUNCTION atcf.update_storm(varchar, varchar, integer, varchar, varchar, varchar, varchar, integer) TO awips;
EOF

echo >&2
echo "Successfully updated ATCF DB function update_storm ..." >&2