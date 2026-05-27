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
package com.raytheon.viz.hydro.staffgage;

import java.util.LinkedHashMap;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;

/**
 * Class for managing database query calls. RiverDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1507      dhladky     Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class StaffGageDataManager {

    private LinkedHashMap<String, RiverDataPoint> riverPoints = null;

    /** Singleton instance of this class */
    private static StaffGageDataManager staffManager = null;

    /* Private Constructor */
    private StaffGageDataManager() {
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized StaffGageDataManager getInstance() {
        if (staffManager == null) {
            staffManager = new StaffGageDataManager();
        }
        return staffManager;
    }

    /**
     * Get Staff Gage display data
     * 
     * @return RiverDataPoint
     */
    public RiverDataPoint getStaffGageData() {

        RiverDataManager rdm = RiverDataManager.getInstance();
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        RiverDataPoint rdp = null;

        if (manager.getCurrentData() != null) {
            // sets the River ID
            manager.setRiverID(rdm.getRiverID(manager.getCurrentLid()));
            riverPoints = rdm.populateRiverData(manager.getCurrentData()
                    .getRiverID(), rdm.getRiverSummaryData().get(
                    manager.getCurrentData().getRiverID()));
            if (riverPoints != null) {
                rdp = riverPoints.get(manager.getCurrentLid());
                // get crest data
                rdp = rdm.getRiverCrest(rdp);
            }
        }
        return rdp;
    }

}
