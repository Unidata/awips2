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

CREATE INDEX "gridDatasetReftime_idx"
  ON grid
  USING btree
  (info_id, reftime, forecasttime);

CREATE INDEX "gridinfoNameParamLevel_idx"
  ON grid_info
  USING btree
  (datasetid, parameter_abbreviation, level_id);
  
CREATE INDEX "gridinfoSecondryId_idx"
  ON grid_info
  USING btree
  (secondaryid)
  WHERE secondaryid IS NOT NULL;
  
CREATE INDEX "gridinfoEnsembleId_idx"
  ON grid_info
  USING btree
  (ensembleid)
  WHERE ensembleid IS NOT NULL;
