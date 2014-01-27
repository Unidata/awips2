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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ITopoManager;

/**
 * GFETopoMgr manages the set of topography data sets available from the
 * ifpServer.
 * 
 * All parms now use the same topo data, since the assumption is that all parms
 * in a database have the same exact GridLocation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul  2, 2008		#1160	randerso	Initial creation
 * Nov 20, 2013     #2331   randerso    Re-implemented to better match A1 design
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFETopoManager implements ITopoManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFETopoManager.class);

    private DataManager dataManager;

    private ScalarGridSlice compositeTopo;

    private ParmID compositeParmID;

    /**
     * Constructor taking a pointer to the DataManager
     * 
     * @param dataManager
     */
    public GFETopoManager(DataManager dataManager) {
        this.dataManager = dataManager;

        // hard coding this here to match what is done in TopoDatabaseManager
        // this avoids retrieving the topo data just to get the parm ID.
        DatabaseID did = new DatabaseID("Topo", DataType.GRID, "Topo", "Topo");
        this.compositeParmID = new ParmID("Topo", did);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ITopoManager#getCompositeTopo()
     */
    @Override
    public synchronized ScalarGridSlice getCompositeTopo() {
        if (compositeTopo == null) {
            GridLocation gloc = dataManager.getParmManager()
                    .compositeGridLocation();
            try {
                ServerResponse<ScalarGridSlice> sr = this.dataManager
                        .getClient().getTopoData(gloc);
                if (!sr.isOkay()) {
                    statusHandler
                            .error("Error getting topography data from IFPServer: "
                                    + sr.message());
                }
                compositeTopo = sr.getPayload();
            } catch (GFEServerException e) {
                statusHandler.error(
                        "Error getting topography data from IFPServer: ", e);
            }
        }
        return compositeTopo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.ITopoManager#getCompositeParmID()
     */
    @Override
    public ParmID getCompositeParmID() {
        return this.compositeParmID;
    }
}
