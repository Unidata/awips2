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
CREATE INDEX nctaf_change_groups_parent_idx
  ON nctaf_change_groups
  USING btree
  (parentid);
  
CREATE INDEX nctaf_icing_layers_idx
  ON nctaf_icing_layers
  USING btree
  (parentid);
  
CREATE INDEX nctaf_sky_cover_idx
  ON nctaf_sky_cover
  USING btree
  (parentid);
  
CREATE INDEX nctaf_temperature_fcst_idx
  ON nctaf_temperature_forecasts
  USING btree
  (parentid);
  
CREATE INDEX nctaf_turbulence_layers_idx
  ON nctaf_turbulence_layers
  USING btree
  (parentid);
  
CREATE INDEX nctaf_weather_cond_idx
  ON nctaf_weather_conditions
  USING btree
  (parentid);