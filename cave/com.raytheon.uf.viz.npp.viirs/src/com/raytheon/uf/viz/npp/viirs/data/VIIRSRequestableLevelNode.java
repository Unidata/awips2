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
package com.raytheon.uf.viz.npp.viirs.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;

/**
 * VIIRS Requestable level node
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSRequestableLevelNode extends AbstractRequestableLevelNode {

    private Map<String, RequestConstraint> requestConstraints;

    /**
     * @param requestConstraints
     */
    public VIIRSRequestableLevelNode(
            Map<String, RequestConstraint> requestConstraints) {
        this.requestConstraints = requestConstraints;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * processDataQueryResults
     * (com.raytheon.uf.common.dataquery.responses.DbQueryResponse)
     */
    @Override
    protected List<AbstractRequestableData> processDataQueryResults(
            DbQueryResponse queryResponse) throws VizException {
        List<Map<String, Object>> results = queryResponse.getResults();
        List<AbstractRequestableData> data = new ArrayList<AbstractRequestableData>(
                results.size());
        for (Map<String, Object> result : results) {
            data.add(new VIIRSRequestableData((VIIRSDataRecord) result
                    .get(null), getLevel()));
        }
        return data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        return Collections.emptyList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * isTimeAgnostic()
     */
    @Override
    public boolean isTimeAgnostic() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * hasRequestConstraints()
     */
    @Override
    public boolean hasRequestConstraints() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getRequestConstraintMap()
     */
    @Override
    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return requestConstraints;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * timeQueryInternal(boolean, java.util.Map, java.util.Map)
     */
    @Override
    protected Set<DataTime> timeQueryInternal(TimeQueryRequest originalRequest,
            boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        TimeQueryRequest timeQuery = getTimeQuery(originalRequest, latestOnly,
                cache, latestOnlyCache);
        @SuppressWarnings("unchecked")
        List<DataTime> dataTimes = (List<DataTime>) ThriftClient
                .sendRequest(timeQuery);
        return new HashSet<DataTime>(dataTimes);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getTimeQueryInternal(boolean, java.util.Map)
     */
    @Override
    protected TimeQueryRequest getTimeQueryInternal(
            TimeQueryRequest originalRequest, boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache)
            throws VizException {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                originalRequest.getQueryTerms());
        constraints.putAll(requestConstraints);
        TimeQueryRequest tr = new TimeQueryRequest();
        tr.setQueryTerms(constraints);
        tr.setMaxQuery(latestOnly);
        tr.setPluginName("viirs");
        return tr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDataInternal(com.raytheon.uf.viz.core.catalog.LayerProperty, int,
     * java.util.Map)
     */
    @Override
    protected List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        DbQueryRequest dataQuery = getDataQuery(property, timeOut, cache);
        return processDataQueryResults((DbQueryResponse) ThriftClient
                .sendRequest(dataQuery));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDataQueryInternal(com.raytheon.uf.viz.core.catalog.LayerProperty, int,
     * java.util.Map)
     */
    @Override
    protected DbQueryRequest getDataQueryInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                property.getEntryQueryParameters(true));
        constraints.putAll(requestConstraints);
        constraints.put("pluginName", new RequestConstraint("viirs"));
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(constraints);
        request.setEntityClass(VIIRSDataRecord.class);
        return request;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((requestConstraints == null) ? 0 : requestConstraints
                        .hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        VIIRSRequestableLevelNode other = (VIIRSRequestableLevelNode) obj;
        if (requestConstraints == null) {
            if (other.requestConstraints != null)
                return false;
        } else if (!requestConstraints.equals(other.requestConstraints))
            return false;
        return true;
    }

}
