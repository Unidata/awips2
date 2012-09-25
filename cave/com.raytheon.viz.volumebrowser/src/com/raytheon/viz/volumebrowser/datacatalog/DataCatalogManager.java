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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VbUtil;

/**
 * 
 * This class is the main data catalog manager class that routes the selected
 * data to create the proper product to be displayed in the Volume Browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataCatalogManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataCatalogManager.class);

    private static DataCatalogManager dataCatalogManager;

    /**
     * Catalog Map containing the data catalogs the volume browser is using to
     * query which products are available and how to load them.
     */
    private final List<IDataCatalog> catalogs;

    private static class QueryCatalogJob extends Job {

        private AvailableDataRequest request;

        public QueryCatalogJob(AvailableDataRequest request) {
            super("Updating Available Inventory");
            this.request = request;
            this.schedule();
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                for (IDataCatalog catalog : getDataCatalogManager().catalogs) {
                    catalog.getAvailableData(request);
                    if (monitor.isCanceled() || request.isCanceled()) {
                        break;
                    }
                }
            } catch (RuntimeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error Occured Updating Available Inventory", e);
            }
            return Status.OK_STATUS;
        }

        @Override
        protected void canceling() {
            request.cancel();
        }

    };

    private static class MenuUpdateJob extends UIJob {

        // Do not process more than this many in one batch to avoid hanging the
        // UI Thread
        private static final int MAX_TO_PROCESS = 200;

        private Job queryCatalogJob;

        private AvailableDataRequest request;

        public MenuUpdateJob(AvailableDataRequest request, Job queryCatalogJob) {
            super("Updating Menus");
            this.queryCatalogJob = queryCatalogJob;
            this.request = request;
            this.setSystem(true);
            this.setPriority(Job.INTERACTIVE);
            this.schedule();
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            int processed = 0;
            String next = request.getAvailableSource();
            while (next != null) {
                VbUtil.getDataListsProdTableComp().markAvailableSource(next);
                if (processed++ > MAX_TO_PROCESS - 2) {
                    break;
                }
                next = request.getAvailableSource();
            }
            next = request.getAvailableField();
            while (next != null) {
                VbUtil.getDataListsProdTableComp().markAvailableField(next);
                if (processed++ > MAX_TO_PROCESS - 1) {
                    break;
                }
                next = request.getAvailableField();
            }
            next = request.getAvailablePlane();
            while (next != null) {
                VbUtil.getDataListsProdTableComp().markAvailablePlane(next);
                if (processed++ > MAX_TO_PROCESS) {
                    break;
                }
                next = request.getAvailablePlane();
            }
            if (queryCatalogJob.getResult() != null && !request.anyAvailable()) {
                // we are done processing available data so quit
                VbUtil.getDataListsProdTableComp().performAutoSelect();

                return Status.OK_STATUS;
            } else {
                // There is more work to do so reschedule to free up UI Thread.
                this.schedule(10 + MAX_TO_PROCESS - processed);
                return Status.OK_STATUS;
            }
        }
    }

    private Job menuUpdateJob;

    private Job queryCatalogJob;

    /**
     * Constructor.
     * 
     * @param prodTable
     *            Volume Browser's product table.
     */
    private DataCatalogManager() {
        // TODO these should be read from an extension point.
        catalogs = new ArrayList<IDataCatalog>();
        catalogs.add(new GridDataCatalog());
        catalogs.add(new AcarsSoundingDataCatalog());
        catalogs.add(new DmdDataCatalog());
        catalogs.add(new PointDataCatalog());
        catalogs.add(new ModelSoundingCatalog());
        catalogs.add(new VwpDataCatalog());
    }

    public void addDataCatalog(IDataCatalog catalog) {
        catalogs.add(catalog);
    }

    public IDataCatalogEntry getDataCatalogEntry(SelectedData selectedData) {
        IDataCatalog catalog = getDataCatalog(selectedData);
        if (catalog != null) {
            return catalog.getCatalogEntry(selectedData);
        }
        return null;
    }

    /**
     * For each data catalog in dataCatalogClasses this will update all
     * available data with the given selection. This function runs the
     * availability calculation in a separate thread and will return before the
     * calculations have completed. If this function is called again while still
     * calculating availability it will attempt to interrupt the previous
     * calculation and clear the available data before starting a new
     * availability calculation.
     * 
     * @param dataCatalogClasses
     * @param selectedSources
     * @param selectedFields
     * @param selectedPlanes
     */
    public synchronized void updateAvailableData(String[] selectedSources,
            String[] selectedFields, String[] selectedPlanes) {
        if (queryCatalogJob != null) {
            queryCatalogJob.cancel();
        }
        if (menuUpdateJob != null) {
            menuUpdateJob.cancel();
        }
        VbUtil.getDataListsProdTableComp().clearAvailableData();
        AvailableDataRequest request = new AvailableDataRequest(
                selectedSources, selectedFields, selectedPlanes);
        queryCatalogJob = new QueryCatalogJob(request);
        menuUpdateJob = new MenuUpdateJob(request, queryCatalogJob);
    }

    public static DataCatalogManager getDataCatalogManager() {
        if (dataCatalogManager == null) {
            dataCatalogManager = new DataCatalogManager();
        }
        return dataCatalogManager;
    }

    /**
     * 
     * @param selectedData
     * @return an IDataCatalog for the selected data
     */
    public IDataCatalog getDataCatalog(SelectedData selectedData) {
        for (IDataCatalog catalog : getDataCatalogManager().catalogs) {
            if (catalog.getSupportedSources().contains(
                    selectedData.getSourcesKey())) {
                return catalog;
            }
        }
        return null;
    }

}
