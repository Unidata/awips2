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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

/**
 * This is the inventory available for a product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2009 #2592      jelkins     Initial creation
 * Jan 4, 2010  #3960      jelkins     Perform inventory updates in separate thread
 * Feb 12, 2015 4105       rferrel     Synchronize forecastTimeMap to prevent ConcurrentModificationException.
 * 
 * </pre>
 * 
 * @author jelkins
 * 
 */
public class ProductInventory extends Job {

    /**
     * Product Inventory Update Job
     * 
     * <p>
     * This implementation uses two Job threads. The UpdateJob thread will
     * perform the inventory query. When the query is complete the fields of the
     * ProductInventory class will be updated. The ProductInventory thread will
     * then be "run" to notify any listeners of the ProductInventory "Job" that
     * it has received updates.
     * <p>
     * The reason we can't use a single Job is because the order in which
     * listeners get called cannot be controlled. We must make sure that
     * listeners waiting for updates to ProductInventory don't get called before
     * the updates occur.
     * 
     */
    private static class UpdateJob extends Job {

        private final HashMap<String, RequestConstraint> productParameters;

        private DataTime[] dataTimes;

        /**
         * @param name
         */
        public UpdateJob(HashMap<String, RequestConstraint> productParameters) {
            super("Updating Inventory...");
            this.productParameters = productParameters;
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {

            try {
                dataTimes = DataCubeContainer.performTimeQuery(
                        productParameters, false);
            } catch (DataCubeException e) {
                throw new RuntimeException(e);
            }

            return Status.OK_STATUS;
        }

        /**
         * @return the dataTimes
         */
        public DataTime[] getDataTimes() {
            return dataTimes;
        }

    }

    /**
     * Key: Integer representing the hour the forecast is forcasting for. Value:
     * Date representing the time the forecast was made. Compare the value with
     * the lastestForcastTime.
     */
    private final TreeMap<Integer, Date> forecastTimeMap = new TreeMap<Integer, Date>();

    /**
     * String representing the latest forecast time.
     */
    private Date latestForecastTime;

    private final UpdateJob updateJob;

    /**
     * 
     */
    public ProductInventory(HashMap<String, RequestConstraint> productParameters) {
        super("");

        updateJob = new UpdateJob(productParameters);

        updateJob.addJobChangeListener(new JobChangeAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse
             * .core.runtime.jobs.IJobChangeEvent)
             */
            @Override
            public void done(IJobChangeEvent event) {
                if (event.getResult().isOK()) {
                    update();
                }
            }
        });
        updateJob.schedule();

    }

    /**
     * 
     */
    protected void update() {

        DataTime[] productTimes = updateJob.getDataTimes();

        if (productTimes.length != 0) {
            latestForecastTime = new Date(0);
        }

        // find the latest time
        synchronized (forecastTimeMap) {
            for (DataTime dataTime : productTimes) {
                if (dataTime.getRefTime().after(latestForecastTime)) {
                    latestForecastTime = dataTime.getRefTime();
                }
                Integer newFcstTime = dataTime.getFcstTime();
                if (!forecastTimeMap.containsKey(newFcstTime)) {
                    forecastTimeMap.put(newFcstTime, dataTime.getRefTime());
                } else if (forecastTimeMap.get(newFcstTime).before(
                        dataTime.getRefTime())) {
                    forecastTimeMap.put(newFcstTime, dataTime.getRefTime());
                }
            }
        }

        this.schedule();

    }

    public List<String> getForecastTimes() {

        List<String> list = null;

        synchronized (forecastTimeMap) {
            list = new ArrayList<String>(forecastTimeMap.size());
            for (Integer forcastHour : forecastTimeMap.keySet()) {

                list.add(getAWIPSDayHour(forecastTimeMap.get(forcastHour))
                        .substring(0, 5) + " " + (forcastHour / 3600) + "HR");

            }
        }

        return list;
    }

    public String getLatestForecastTime() {
        String latestForecastTimeString = "--.----";
        if (latestForecastTime != null) {
            latestForecastTimeString = getAWIPSDayHour(latestForecastTime);
        }
        return latestForecastTimeString;
    }

    public String getInventoryStatusString() {
        StringBuilder sb = new StringBuilder();

        synchronized (forecastTimeMap) {
            for (Integer forcastHour : forecastTimeMap.keySet()) {
                if (forecastTimeMap.get(forcastHour).equals(latestForecastTime)) {
                    sb.append("+");
                } else {
                    sb.append("-");
                }
            }
        }

        return sb.toString();

    }

    @SuppressWarnings("deprecation")
    private String getAWIPSDayHour(Date date) {

        String[] gmtDateSections = date.toGMTString().split(" ");

        String day = gmtDateSections[0];
        String hour = gmtDateSections[3].substring(0, 2);

        return String.format("%02d.%s00", Integer.parseInt(day), hour);
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        // Do nothing in this method. This method is run so that attached
        // listeners know that the data of ProductInventory has been updated.
        return Status.OK_STATUS;
    }

    /**
     * 
     */
    public boolean cancelUpdateJob() {
        return updateJob.cancel();
    }

}
