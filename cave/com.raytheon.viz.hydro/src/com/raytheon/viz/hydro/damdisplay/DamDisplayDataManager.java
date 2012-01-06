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
package com.raytheon.viz.hydro.damdisplay;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.DamMaster;

/**
 * Class for managing database query calls. LowWaterStatementDataManager.java
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class DamDisplayDataManager {
    private static final String DAM = "dam";
   
   /**
    * Get tabularDisplayData from the DB
    * @return String[]
    */
   public Object[] getLowWaterStatementData() {
      ArrayList<Object[]> data;

      try {
          data = (ArrayList<Object[]>)DirectDbQuery.executeQuery("select lid,obstime,lid from latestobsvalue", HydroConstants.IHFS, QueryLanguage.SQL);
          if (data != null) {
              return data.get(0);
          }
      } catch (VizException e) {

      }
      return null;
   }   
   
   public List<DamMaster> getDamMaster(String where) throws VizException {
       List<DamMaster> masterList = new ArrayList<DamMaster>();
       StringBuilder sql = new StringBuilder("select ");
       sql.append("nidid, dam_name, county, river, downstream_hazard, ");
       sql.append("max_storage, hsa, rfc, latitude_dam, longitude_dam ");
       sql.append("from damMaster ");

       if ((where != null) && (where.length() > 0)) {
           sql.append(where);
       }
       
       List<Object[]> results = DirectDbQuery.executeQuery(sql.toString(), DAM, QueryLanguage.SQL);
       
       for (Object[] oa:  results) {
           int i = 0;
           DamMaster dm = new DamMaster();
           dm.setNidid((String) oa[i]);
           if (oa[++i] != null) {
               dm.setDamName((String) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setCounty((String) oa[i]);
           }
           if (oa[i++] != null) {
               dm.setRiver((String) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setDownStreamHazard((String) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setMax_storage((Double) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setHsa((String) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setRfc((String) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setLatitudeDam((Double) oa[i]);
           }
           if (oa[++i] != null) {
               dm.setLongitudeDam((Double) oa[i]);
           }
           
           masterList.add(dm);
       }
       
       return masterList;
   }
   
}
