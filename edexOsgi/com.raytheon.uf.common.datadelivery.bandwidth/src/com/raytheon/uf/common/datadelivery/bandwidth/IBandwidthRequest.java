package com.raytheon.uf.common.datadelivery.bandwidth;

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A bandwidth request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2012 1286       djohnson     Add SW history, move to common plugin.
 * Nov 20, 2012 1286       djohnson     Add PROPOSE_SCHEDULE_SUBSCRIPTION.
 * Dec 06, 2012 1397       djohnson     Add GET_BANDWIDTH_GRAPH_DATA.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class IBandwidthRequest extends AbstractPrivilegedRequest {

    public static enum RequestType {
        METADATA_UPDATE,
        // Get the current retrieval plan for the
        // specified begin and end time.
        RETRIEVAL_PLAN, SHOW_ALLOCATION,
        /**
         * Schedule a subscription.
         */
        SCHEDULE_SUBSCRIPTION, SHOW_BUCKET, SHOW_DEFERRED, GET_BANDWIDTH, FORCE_SET_BANDWIDTH, PROPOSE_SET_BANDWIDTH, PROPOSE_SCHEDULE_SUBSCRIPTION, REINITIALIZE, GET_ESTIMATED_COMPLETION, GET_BANDWIDTH_GRAPH_DATA
    }

    @DynamicSerializeElement
    private long id;

    @DynamicSerializeElement
    private Calendar begin;

    @DynamicSerializeElement
    private DataSetMetaData dataSetMetaData;

    @DynamicSerializeElement
    private Calendar end;

    @DynamicSerializeElement
    private Network network;

    @DynamicSerializeElement
    private RequestType requestType;

    @DynamicSerializeElement
    private List<Subscription> subscriptions;

    @DynamicSerializeElement
    private int bandwidth;

    public long getId() {
        return this.id;
    }

    /**
     * @return the begin
     */
    public Calendar getBegin() {
        return begin;
    }

    /**
     * @return the dataSetMetaData
     */
    public DataSetMetaData getDataSetMetaData() {
        return dataSetMetaData;
    }

    /**
     * @return the end
     */
    public Calendar getEnd() {
        return end;
    }

    public Network getNetwork() {
        return network;
    }

    /**
     * @return the requestType
     */
    public RequestType getRequestType() {
        return requestType;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * @param begin
     *            the begin to set
     */
    public void setBegin(Calendar begin) {
        this.begin = begin;
    }

    /**
     * @param dataSetMetaData
     *            the dataSetMetaData to set
     */
    public void setDataSetMetaData(DataSetMetaData dataSetMetaData) {
        this.dataSetMetaData = dataSetMetaData;
    }

    /**
     * @param end
     *            the end to set
     */
    public void setEnd(Calendar end) {
        this.end = end;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * @param requestType
     *            the requestType to set
     */
    public void setRequestType(RequestType requestType) {
        this.requestType = requestType;
    }

    /**
     * @param bandwidth
     */
    public void setBandwidth(int bandwidth) {
        this.bandwidth = bandwidth;
    }

    /**
     * @return the bandwidth
     */
    public int getBandwidth() {
        return bandwidth;
    }

    /**
     * @return the subscriptions
     */
    public List<Subscription> getSubscriptions() {
        return subscriptions;
    }

    /**
     * @param subscriptions
     *            the subscriptions to set
     */
    public void setSubscriptions(List<Subscription> subscriptions) {
        this.subscriptions = subscriptions;
    }
}
