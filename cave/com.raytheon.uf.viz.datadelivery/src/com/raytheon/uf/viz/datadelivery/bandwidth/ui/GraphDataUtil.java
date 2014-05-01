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
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * This is a utility class used to get data for the bandwidth graph it
 * implements {@link Runnable} so you can retrieve data on a separate thread to
 * keep the UI thread from being blocked.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2012   1269     lvenable     Initial creation
 * Feb 14, 2013 1596       djohnson     Remove sysouts, correct statusHandler class, handle null response.
 * Mar 26, 2013 1827       djohnson     Graph data should be requested from data delivery.
 * Jan 29, 2014 2722       mpduff       Callback is now passed in.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class GraphDataUtil implements Runnable {
    /** UFStatus handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GraphDataUtil.class);

    /** Graph data request object */
    private final GraphDataRequest request = new GraphDataRequest();

    /** Graph data response object */
    private GraphDataResponse response;

    /** Bandwidth graph data */
    private BandwidthGraphData graphData;

    /** Callback called when the data has been updated */
    private final IDataUpdated dataUpdatedCB;

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
        this.dataUpdatedCB = dataUpdatedCB;
    }

    /**
     * Perform a data retrieval on the UI thread.
     */
    private void retrieveData() {
        response = sendRequest(request);

        if (response != null) {
            graphData = response.getGraphData();
        }
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
    public BandwidthGraphData getGraphData() {
        if (graphData == null) {
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
            return (GraphDataResponse) RequestRouter.route(req,
                    DataDeliveryConstants.DATA_DELIVERY_SERVER);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error Requesting Data", e);
        }

        return null;
    }

    /**
     * Thread run method to retrieve the graph data.
     */
    @Override
    public void run() {
        retrieveData();

        if (dataUpdatedCB != null) {
            dataUpdatedCB.dataUpdated();
        }

    }
}
