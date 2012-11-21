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

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode;

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

public class VIIRSRequestableLevelNode extends AbstractBaseDataNode {

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

    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return requestConstraints;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode#
     * getAvailabilityRequest()
     */
    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                originalConstraints);
        constraints.putAll(requestConstraints);
        constraints.put("pluginName", new RequestConstraint("viirs"));
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(VIIRSDataRecord.class.getName());
        request.addRequestField("dataTime");
        request.setDistinct(true);
        request.setConstraints(constraints);
        return request;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode#getDataRequest
     * (java.util.Map, java.util.Set)
     */
    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> availability) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                originalConstraints);
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
     * @see
     * com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode#getAvailability
     * (java.lang.Object)
     */
    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws VizException {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>();
        DbQueryResponse dbresponse = (DbQueryResponse) response;
        for (Map<String, Object> map : dbresponse.getResults()) {
            DataTime time = (DataTime) map.get("dataTime");

            result.add(new TimeAndSpace(time));
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode#getData(java
     * .util.Map, java.util.Set, java.lang.Object)
     */
    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws VizException {
        DbQueryResponse queryResponse = (DbQueryResponse) response;
        List<Map<String, Object>> results = queryResponse.getResults();
        Set<AbstractRequestableData> data = new HashSet<AbstractRequestableData>(
                results.size());
        for (Map<String, Object> result : results) {
            data.add(new VIIRSRequestableData((VIIRSDataRecord) result
                    .get(null), getLevel()));
        }
        return data;
    }

}
