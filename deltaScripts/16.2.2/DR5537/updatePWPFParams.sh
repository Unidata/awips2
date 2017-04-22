#!/bin/bash
# DR 5537
# Update 12/24/48/72 hr TOTSN/FRZR inch and percent parameter names to start with the time period.
# Parameters whose name already start with the time period will not be updated.

echo "Updating 12hr parameter names."
psql -U awips -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'TOTSN%pct12hr' AND name NOT LIKE '12hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'TOTSN%in12hr' AND name NOT LIKE '12hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'FRZR%pct12hr' AND name NOT LIKE '12hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '12hr ' || name WHERE abbreviation like 'FRZR%in12hr' AND name NOT LIKE '12hr%'";

echo "Updating 24hr parameter names."
psql -U awips -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'TOTSN%pct24hr' AND name NOT LIKE '24hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'TOTSN%in24hr' AND name NOT LIKE '24hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'FRZR%pct24hr' AND name NOT LIKE '24hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '24hr ' || name WHERE abbreviation like 'FRZR%in24hr' AND name NOT LIKE '24hr%'";

echo "Updating 48hr parameter names."
psql -U awips -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'TOTSN%pct48hr' AND name NOT LIKE '48hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'TOTSN%in48hr' AND name NOT LIKE '48hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'FRZR%pct48hr' AND name NOT LIKE '48hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '48hr ' || name WHERE abbreviation like 'FRZR%in48hr' AND name NOT LIKE '48hr%'";

echo "Updating 72hr parameter names."
psql -U awips -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'TOTSN%pct72hr' AND name NOT LIKE '72hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'TOTSN%in72hr' AND name NOT LIKE '72hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'FRZR%pct72hr' AND name NOT LIKE '72hr%'";
psql -U awips -d metadata -c "UPDATE parameter SET name = '72hr ' || name WHERE abbreviation like 'FRZR%in72hr' AND name NOT LIKE '72hr%'";

echo "Done."
