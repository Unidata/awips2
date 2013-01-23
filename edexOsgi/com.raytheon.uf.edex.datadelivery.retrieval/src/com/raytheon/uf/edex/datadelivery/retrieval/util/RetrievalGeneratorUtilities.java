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
package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * 
 * Retrieval generation related utilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2012            bsteffen     Initial javadoc
 * Dec 10, 2012   1259     bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class RetrievalGeneratorUtilities {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalGeneratorUtilities.class);

    /**
     * Find duplicate URI's
     * 
     * @param dataURI
     * @return
     */
    public static boolean findDuplicateUri(String dataUri, String plugin) {

        boolean isDuplicate = false;
        String sql = "select id from " + plugin + " where datauri = '"
                + dataUri + "'";

        CoreDao dao = new CoreDao(DaoConfig.forDatabase("metadata"));
        Object[] results = dao.executeSQLQuery(sql);
        if (results.length > 0) {
            isDuplicate = true;
        }

        return isDuplicate;
    }

    /**
     * Find the duplicate URI's for grid
     * 
     * @param times
     * @param levels
     * @param parm
     * @param cov
     * @return
     */
    public static HashMap<DataTime, ArrayList<Level>> findGridDuplicates(
            String name,
            ArrayList<DataTime> times, ArrayList<Level> levels, Parameter parm,
            GridCoverage cov) {

        HashMap<DataTime, ArrayList<Level>> dups = new HashMap<DataTime, ArrayList<Level>>();

        for (DataTime time : times) {

            ArrayList<Level> levDups = dups.get(time);

            if (levDups == null) {
                levDups = new ArrayList<Level>();
            }

            for (Level level : levels) {
                try {

                    GridRecord rec = ResponseProcessingUtilities.getGridRecord(
                            name, parm, level, cov);
                    rec.setDataTime(time);
                    rec.constructDataURI();
                    boolean isDup = findDuplicateUri(rec.getDataURI(), "grid");
                    if (isDup) {
                        levDups.add(level);
                    }
                } catch (PluginException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            if (!levDups.isEmpty()) {
                dups.put(time, levDups);
            }
        }

        return dups;
    }
}
