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
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * This is the inventory available for a product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 10, 2009  2592     jelkins   Initial creation
 * Jan 04, 2010  3960     jelkins   Perform inventory updates in separate thread
 * Feb 12, 2015  4105     rferrel   Synchronize forecastTimeMap to prevent
 *                                  ConcurrentModificationException.
 * Apr 05, 2018  6701     bsteffen  Listen for updates.
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

        private final Map<String, RequestConstraint> productParameters;

        private DataTime[] dataTimes;

        /**
         * @param name
         */
        public UpdateJob(Map<String, RequestConstraint> productParameters) {
            super("Updating Inventory...");
            this.productParameters = productParameters;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            try {
                dataTimes = DataCubeContainer
                        .performTimeQuery(productParameters, false);
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

    private final Map<String, RequestConstraint> productParameters;

    /**
     * Key: Integer representing the hour the forecast is forcasting for. Value:
     * Date representing the latest time the forecast was made. Compare the
     * value with the lastestForcastTime.
     */
    private final TreeMap<Integer, Date> forecastTimeMap = new TreeMap<>();

    /**
     * String representing the latest forecast time.
     */
    private Date latestForecastTime;

    private final UpdateJob updateJob;

    private final IAlertObserver observer = this::alertArrived;

    public ProductInventory(Map<String, RequestConstraint> productParameters) {
        super("");
        this.productParameters = productParameters;

        updateJob = new UpdateJob(productParameters);

        updateJob.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                if (event.getResult().isOK()) {
                    update();
                }
            }
        });
        updateJob.schedule();

        ProductAlertObserver.addObserver(getPluginName(), observer);
    }

    protected void update() {

        DataTime[] productTimes = updateJob.getDataTimes();

        if (productTimes.length != 0) {
            latestForecastTime = new Date(0);
        }

        // find the latest time
        synchronized (forecastTimeMap) {
            for (DataTime dataTime : productTimes) {
                addTime(dataTime);
            }
        }

        this.schedule();
    }

    protected void alertArrived(Collection<AlertMessage> alertMessages) {

        for (AlertMessage message : alertMessages) {
            boolean match = true;
            for (Entry<String, RequestConstraint> entry : productParameters
                    .entrySet()) {
                RequestConstraint constraint = entry.getValue();
                Object value = message.decodedAlert.get(entry.getKey());
                if (!constraint.evaluate(value)) {
                    match = false;
                    break;
                }
            }
            if (match) {
                synchronized (forecastTimeMap) {
                    DataTime dataTime = (DataTime) message.decodedAlert
                            .get(PluginDataObject.DATATIME_ID);
                    addTime(dataTime);
                    this.schedule();
                }

            }
        }

    }

    /**
     * Add a new inventory time for this product.
     * 
     * @param dataTime
     *            the time to add.
     */
    protected void addTime(DataTime dataTime) {
        /* Need to remember if this is the latest reftime ever seen. */
        if (latestForecastTime == null
                || dataTime.getRefTime().after(latestForecastTime)) {
            latestForecastTime = dataTime.getRefTime();
        }
        Integer newFcstTime = dataTime.getFcstTime();
        /*
         * Add it to the forecastTimeMap only if it is a new forecast time or
         * later then the current time for this forecast time.
         */
        if (!forecastTimeMap.containsKey(newFcstTime)) {
            forecastTimeMap.put(newFcstTime, dataTime.getRefTime());
        } else if (forecastTimeMap.get(newFcstTime)
                .before(dataTime.getRefTime())) {
            forecastTimeMap.put(newFcstTime, dataTime.getRefTime());
        }

    }

    public List<String> getForecastTimes() {

        List<String> list = null;

        synchronized (forecastTimeMap) {
            list = new ArrayList<>(forecastTimeMap.size());
            for (Entry<Integer, Date> forcastEntry : forecastTimeMap
                    .entrySet()) {

                list.add(
                        getAWIPSDayHour(forcastEntry.getValue()).substring(0, 5)
                                + " " + (forcastEntry.getKey() / 3600) + "HR");

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
            for (Date latestRef : forecastTimeMap.values()) {
                if (latestRef.equals(latestForecastTime)) {
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
        ProductAlertObserver.removeObserver(getPluginName(), observer);
        return updateJob.cancel();
    }

    private String getPluginName() {
        RequestConstraint constraint = productParameters
                .get(PluginDataObject.PLUGIN_NAME_ID);
        return constraint.getConstraintValue();
    }

}
