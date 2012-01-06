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
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IHlsTopoManager;

/**
 * GFEClimoMgr manages the set of climate data sets available from the
 * ifpServer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2011            wldougher   Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class GFEHlsTopoManager implements IHlsTopoManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GFEHlsTopoManager.class);

    private DataManager dataManager;

    private Map<List<String>, List<IGridSlice>> compositeHlsTopoMap;

    public GFEHlsTopoManager(DataManager dataManager) {
        this.dataManager = dataManager;
        compositeHlsTopoMap = new HashMap<List<String>, List<IGridSlice>>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IClimoManager#getCompositeClimo(java.lang.Object
     * [])
     */
    @Override
    public synchronized IGridSlice[] getCompositeTopo(String shortParmName,
            String simpleModel) {
        List<String> parmKey = new ArrayList<String>(2);
        parmKey.add(simpleModel);
        parmKey.add(shortParmName);
        List<IGridSlice> slicesL = compositeHlsTopoMap.get(parmKey);
        IGridSlice[] slices = null;

        if (slicesL == null) {
            ParmID aParmID = null;
            aParmID = getCompositeParmID(shortParmName, simpleModel);
            try {
                IFPClient client = dataManager.getClient();
                List<TimeRange> trs = client.getGridInventory(aParmID);

                slicesL = client.getGridData(aParmID, trs);
                GridParmInfo gpi = client.getGridParmInfo(aParmID);
                gpi.setParmID(aParmID);

                // Save the result in the map
                if (slicesL != null) {
                    compositeHlsTopoMap.put(parmKey, slicesL);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to load "
                                + simpleModel + " HLS topology data for "
                                + shortParmName, e);
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
    public ParmID getCompositeParmID(String shortParmName, String model) {
        ParmID rtnVal = null;
        String siteID = dataManager.getSiteID();
        String modelID = siteID + "_GRID_hlsTopo_" + model + "_00000000_0000";
        rtnVal = new ParmID(shortParmName, modelID, "SFC");
        return rtnVal;
    }
}
