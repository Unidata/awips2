BEGIN;
CREATE TEMPORARY TABLE gridcoverage_id_tmp AS (SELECT id FROM gridcoverage WHERE majoraxis > 7000000 OR minoraxis > 7000000);
DELETE FROM grid WHERE info_id IN (SELECT id FROM grid_info WHERE location_id IN (SELECT * FROM gridcoverage_id_tmp));
DELETE FROM grid_info WHERE location_id IN (SELECT * FROM gridcoverage_id_tmp);
DELETE FROM gridcoverage WHERE id IN (SELECT * FROM gridcoverage_id_tmp);
COMMIT;
