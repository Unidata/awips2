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
package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IClimoManager;

/**
 * GFEClimoMgr manages the set of climate data sets available from the
 * ifpServer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class GFEClimoManager implements IClimoManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEClimoManager.class);

    private DataManager dataManager;

    private Map<List<String>, List<IGridSlice>> compositeClimoMap;

    public GFEClimoManager(DataManager dataManager) {
        this.dataManager = dataManager;
        compositeClimoMap = new HashMap<List<String>, List<IGridSlice>>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IClimoManager#getCompositeClimo(java.lang.Object
     * [])
     */
    @Override
    public synchronized IGridSlice[] getCompositeClimo(String source,
            String shortParmName) {
        List<String> parmKey = new ArrayList<String>(2);
        parmKey.add(source);
        parmKey.add(shortParmName);
        List<IGridSlice> slicesL = compositeClimoMap.get(parmKey);
        IGridSlice[] slices = null;

        if (slicesL == null) {
            ParmID aClimoParmID = null;
            aClimoParmID = getCompositeParmID(source, shortParmName);
            try {
                IFPClient client = dataManager.getClient();
                List<TimeRange> trs = client.getGridInventory(aClimoParmID);

                slicesL = client.getGridData(aClimoParmID, trs);
                GridParmInfo gpi = client.getGridParmInfo(aClimoParmID);
                gpi.setParmID(aClimoParmID);

                // Save the result in the map
                if (slicesL != null) {
                    compositeClimoMap.put(parmKey, slicesL);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to load "
                        + source + " climatology data for " + shortParmName, e);
            }
        }

        if (slicesL != null) {
            // make copies?? - too slow...
            slices = slicesL.toArray(new IGridSlice[0]);
        }
        return slices;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IClimoManager#getCompositeParmID(java.lang.
     * String, java.lang.String)
     */
    @Override
    public ParmID getCompositeParmID(String source, String shortParmName) {
        ParmID rtnVal = null;
        String siteID = dataManager.getSiteID();
        String modelID = siteID + "_GRID_Climo_" + source + "_00000000_0000";
        rtnVal = new ParmID(shortParmName, modelID, "SFC");
        return rtnVal;
    }
}
