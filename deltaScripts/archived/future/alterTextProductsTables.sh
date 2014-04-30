#!/bin/sh
# This script should be run on dx1 as part of next delivery to update the definitions for
# the stdtextproducts and practicestdtextproducts tables.

psql -U awips -d fxa -c "ALTER TABLE stdtextproducts RENAME COLUMN createtime to reftime;"
psql -U awips -d fxa -c "ALTER TABLE practicestdtextproducts RENAME COLUMN createtime to reftime;"

psql -U awips -d fxa -c "ALTER TABLE stdtextproducts ADD COLUMN inserttime timestamp without time zone;"
psql -U awips -d fxa -c "ALTER TABLE practicestdtextproducts ADD COLUMN inserttime timestamp without time zone;"

psql -U awips -d fxa -c "CREATE INDEX stdtextproductsinserttimeindex ON stdtextproducts USING btree (inserttime);"
psql -U awips -d fxa -c "CREATE INDEX practicestdtextproductsinserttimeindex ON practicestdtextproducts USING btree (inserttime);"

psql -U awips -d fxa -c "UPDATE stdtextproducts SET inserttime = CURRENT_TIMESTAMP AT TIME ZONE 'GMT' WHERE inserttime IS NULL;"
psql -U awips -d fxa -c "UPDATE practicestdtextproducts SET inserttime = CURRENT_TIMESTAMP AT TIME ZONE 'GMT' WHERE inserttime IS NULL;"

psql -U awips -d fxa -c "ALTER TABLE stdtextproducts ALTER COLUMN inserttime SET NOT NULL;"
psql -U awips -d fxa -c "ALTER TABLE practicestdtextproducts ALTER COLUMN inserttime SET NOT NULL;"
