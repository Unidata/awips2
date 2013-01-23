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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.bandwidth.request.GraphDataRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.response.GraphDataResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.datadelivery.actions.BandwidthScheduleGraphAction;

/**
 * 
 * This is a utility class used to get data for the bandwidth graph it extends
 * the Thread class so you can retrieve data on a separate thread to keep the UI
 * thread from being blocked.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2012   1269     lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GraphDataUtil extends Thread {
    /** UFStatus handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthScheduleGraphAction.class);

    /** Graph data request object */
    private GraphDataRequest request;

    /** Graph data response object */
    private GraphDataResponse response;

    /** Bandwidth graph data */
    private BandwidthGraphData graphData;

    /** Callback called when the data has been updated */
    private IDataUpdated dataUpdatedCB;

    /** Executor service used for the threaded data retrieval */
    private final ExecutorService service = Executors.newSingleThreadExecutor();

    /**
     * Constructor.
     * 
     * @param dataUpdatedCB
     *            Call back called when the data has been updated via separate
     *            thread.
     */
    public GraphDataUtil(IDataUpdated dataUpdatedCB) {
        request = new GraphDataRequest();
        this.dataUpdatedCB = dataUpdatedCB;
    }

    /**
     * Set the updated data callback.
     * 
     * @param dataUpdatedCB
     *            Call back called when the data has been updated via separate
     *            thread.
     */
    public void setDataUpdateCallback(IDataUpdated dataUpdatedCB) {
        this.dataUpdatedCB = dataUpdatedCB;
    }

    /**
     * Perform a data retrieval on the UI thread.
     */
    public void retrieveData() {
        response = sendRequest(request);
        graphData = response.getGraphData();
    }

    /**
     * Get the graph data. If the new data flag is set to true then it will do a
     * new retrieval and return the new data otherwise it will return the
     * existing data.
     * 
     * @param newData
     *            Flag indicating if retrieving new data should occur before
     *            returning the data.
     * @return Bandwidth graph data.
     */
    public BandwidthGraphData getGraphData(boolean newData) {
        if (newData || graphData == null) {
            retrieveData();
        }
        return graphData;
    }

    /**
     * Request retrieving graph data using a thread. When the thread is complete
     * a data updated callback is called notifying the data retrieval has
     * completed.
     */
    public void requestGraphDataUsingThread() {
        service.submit(this);
    }

    /**
     * Cancel the data retrieval thread.
     */
    public void cancelThread() {
        service.shutdownNow();
    }

    /**
     * Send a request for the bandwidth graph data.
     * 
     * @param req
     *            Graph data request.
     * @return The graph data response.
     */
    private GraphDataResponse sendRequest(GraphDataRequest req) {
        try {
            return (GraphDataResponse) ThriftClient.sendRequest(req);
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR, "Error Requesting Data", e);
        }

        return null;
    }

    /**
     * Thread run method to retrieve the graph data.
     */
    public void run() {
        System.out.println("Thread - retrieving data...");
        retrieveData();

        if (dataUpdatedCB != null) {
            dataUpdatedCB.dataUpdated();
        }

        System.out.println("Thread - DONE retrieving data...");
    }
}
