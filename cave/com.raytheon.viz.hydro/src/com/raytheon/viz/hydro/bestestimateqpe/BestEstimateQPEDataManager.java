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
package com.raytheon.viz.hydro.bestestimateqpe;

import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Class for managing database query calls. BestEstimateQPEDataManager.java
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

public class BestEstimateQPEDataManager {

   
    private static final String tabDispDataQuery = "select lid,obstime,lid from latestobsvalue";
    
   /**
    * Get tabularDisplayData from the DB
    * @return String[]
    */
   public Object[] getBestEstimateQPEData() {
      
       List<Object[]> data;

       try {
           data = DirectDbQuery.executeQuery(tabDispDataQuery,
                   HydroConstants.IHFS, QueryLanguage.SQL);
           if (data != null) {
               return data.get(0);
           }
       } catch (VizException e) {

       }
       return null;
   }   
   
  
}
