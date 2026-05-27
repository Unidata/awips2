CREATE OR REPLACE VIEW gfe_view AS 
 SELECT db.siteid, db.modelname, db.dbtype, db.modeltime, parm.parmname, 
    parm.parmlevel, rec.rangestart, rec.rangeend, rec.inserttime
   FROM gfe rec, gfe_parmid parm, gfe_dbid db
  WHERE rec.parmid_id = parm.id AND parm.dbid_id = db.id;

ALTER TABLE gfe_view
  OWNER TO awips;
GRANT ALL ON TABLE gfe_view TO awips;
GRANT SELECT ON TABLE gfe_view TO public;

CREATE OR REPLACE VIEW gfe_locks_view AS 
 SELECT db.siteid, db.modelname, db.dbtype, db.modeltime, parm.parmname, 
    parm.parmlevel, lk.starttime, lk.endtime, lk.wsid
   FROM gfe_locks lk, gfe_parmid parm, gfe_dbid db
  WHERE lk.parmid_id = parm.id AND parm.dbid_id = db.id;

ALTER TABLE gfe_locks_view
  OWNER TO awips;
GRANT ALL ON TABLE gfe_locks_view TO awips;
GRANT SELECT ON TABLE gfe_locks_view TO public;
