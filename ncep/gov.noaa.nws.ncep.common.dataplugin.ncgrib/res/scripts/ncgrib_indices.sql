/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
CREATE INDEX "ncgribEventName_idx"
  ON ncgrib
  USING btree
  (eventname);

CREATE INDEX "ncgribModelName_idx"
  ON ncgrib
  USING btree
  (modelname);

CREATE INDEX "ncgribParm_idx"
  ON ncgrib
  USING btree
  (parm);

CREATE INDEX "ncgribRecordQuery_idx"
  ON ncgrib
  USING btree
  (modelName, parm, vcord, glevel1, glevel2, refTime, forecastTime);

CREATE INDEX "ncgribRecordEventRangeQuery_idx"
  ON ncgrib
  USING btree
  (refTime, forecastTime, utilityFlags, rangeStart, rangeEnd, eventName);
  
CREATE INDEX "ncgribModelsModelName_idx"
  ON ncgrib_models
  USING btree
  (modelname);

CREATE INDEX "ncgribModelsLevelId_idx"
  ON ncgrib_models
  USING btree
  (level_id);

CREATE INDEX "ncgribCoverageType_idx"
  ON ncgridcoverage
  USING btree
  (dtype);

CREATE INDEX "ncgribCoverageLookup_idx"
  ON ncgridcoverage
  USING btree
  (dx, dy, la1, lo1, nx, ny);

