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
package com.raytheon.viz.gfe.core;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.gfe.core.internal.NotificationRouter;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.gfe.gridmanager.IGridManager;
import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.gfe.smarttool.GridCycler;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2008            wdougherty     Initial creation
 * </pre>
 * 
 * @author wdougherty
 * @version 1.0
 */

public class FakeDataManager extends DataManager {

    protected GridCycler gridCycler;

    protected IParmManager parmManager;

    protected ISpatialDisplayManager spatialDisplayManager;

    protected IReferenceSetManager refManager;

    protected ISampleSetManager sampleSetManager;

    protected IFPClient client;

    protected ParmOp parmOp;

    protected IWEGroupManager weGroupManager;

    protected NotificationRouter router;

    protected ITopoManager topoManager;

    private IGridManager gridManager;

    private AutoSaveJob autoSaveJob;

    public FakeDataManager() throws GFEServerException {
        super(new DataManagerOffscreenFactory(), null);
        parmManager = super.getParmManager();
        spatialDisplayManager = super.getSpatialDisplayManager();
        parmOp = super.getParmOp();
        topoManager = super.getTopoManager();
    }

    /**
     * @return the gridCycler
     */
    @Override
    public GridCycler getGridCycler() {
        if (gridCycler == null) {
            return super.getGridCycler();
        }
        return gridCycler;
    }

    /**
     * @param gridCycler
     *            the gridCycler to set
     */
    public void setGridCycler(GridCycler gridCycler) {
        this.gridCycler = gridCycler;
    }

    /**
     * @return the parmManager
     */
    @Override
    public IParmManager getParmManager() {
        if (parmManager == null) {
            return super.getParmManager();
        }
        return parmManager;
    }

    /**
     * @param parmManager
     *            the parmManager to set
     */
    public void setParmManager(IParmManager parmManager) {
        this.parmManager = parmManager;
    }

    /**
     * @return the spatialDisplayManager
     */
    @Override
    public ISpatialDisplayManager getSpatialDisplayManager() {
        return spatialDisplayManager;
    }

    /**
     * @param spatialDisplayManager
     *            the spatialDisplayManager to set
     */
    public void setSpatialDisplayManager(
            ISpatialDisplayManager spatialDisplayManager) {
        this.spatialDisplayManager = spatialDisplayManager;
    }

    /**
     * @return the refManager
     */
    @Override
    public IReferenceSetManager getRefManager() {
        return refManager;
    }

    /**
     * @param refManager
     *            the refManager to set
     */
    public void setRefManager(IReferenceSetManager refManager) {
        this.refManager = refManager;
    }

    /**
     * @return the sampleSetManager
     */
    @Override
    public ISampleSetManager getSampleSetManager() {
        return sampleSetManager;
    }

    /**
     * @param sampleSetManager
     *            the sampleSetManager to set
     */
    public void setSampleSetManager(ISampleSetManager sampleSetManager) {
        this.sampleSetManager = sampleSetManager;
    }

    /**
     * @return the client
     */
    @Override
    public IFPClient getClient() {
        return client;
    }

    /**
     * @param client
     *            the client to set
     */
    public void setClient(IFPClient client) {
        this.client = client;
    }

    /**
     * @return the parmOp
     */
    @Override
    public ParmOp getParmOp() {
        return parmOp;
    }

    /**
     * @param parmOp
     *            the parmOp to set
     */
    public void setParmOp(ParmOp parmOp) {
        this.parmOp = parmOp;
    }

    /**
     * @return the weGroupManager
     */
    public IWEGroupManager getWeGroupManager() {
        return weGroupManager;
    }

    /**
     * @param weGroupManager
     *            the weGroupManager to set
     */
    public void setWeGroupManager(IWEGroupManager weGroupManager) {
        this.weGroupManager = weGroupManager;
    }

    /**
     * @return the router
     */
    public NotificationRouter getRouter() {
        return router;
    }

    /**
     * @param router
     *            the router to set
     */
    public void setRouter(NotificationRouter router) {
        this.router = router;
    }

    /**
     * @return the gridManager
     */
    @Override
    public IGridManager getGridManager() {
        return gridManager;
    }

    /**
     * @param gridManager
     *            the gridManager to set
     */
    @Override
    public void setGridManager(IGridManager gridManager) {
        this.gridManager = gridManager;
    }

    /**
     * @return the autoSaveJob
     */
    public AutoSaveJob getAutoSaveJob() {
        return autoSaveJob;
    }

    /**
     * @param autoSaveJob
     *            the autoSaveJob to set
     */
    public void setAutoSaveJob(AutoSaveJob autoSaveJob) {
        this.autoSaveJob = autoSaveJob;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.DataManager#serverParmInventory(com.raytheon
     * .edex.plugin.gfe.db.objects.ParmID)
     */
    @Override
    public List<TimeRange> serverParmInventory(final ParmID parmID) {
        return null;
    }
}
