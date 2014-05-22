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
package com.raytheon.viz.satellite.inventory;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * Satellite Requestable level node
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SatelliteRequestableLevelNode extends AbstractBaseDataNode {

    private final SatelliteCoverageCache coverages;

    private final Map<String, RequestConstraint> requestConstraints;

    public SatelliteRequestableLevelNode(SatelliteCoverageCache coverages,
            Map<String, RequestConstraint> requestConstraints) {
        this.coverages = coverages;
        this.requestConstraints = requestConstraints;
    }


    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return requestConstraints;
    }

    protected DbQueryRequest getBaseRequest(
            Map<String, RequestConstraint> originalConstraints) {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                originalConstraints);
        constraints.putAll(requestConstraints);
        constraints.remove(SatelliteDataCubeAdapter.DERIVED);
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(SatelliteRecord.class);
        request.setDistinct(true);
        request.setConstraints(constraints);
        return request;
    }

    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        DbQueryRequest request = getBaseRequest(originalConstraints);
        request.addRequestField(PluginDataObject.DATATIME_ID);
        request.addRequestField(SatelliteInventory.GID);
        return request;
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> availability) {
        DbQueryRequest request = getBaseRequest(originalConstraints);
        RequestConstraint timeRc = new RequestConstraint();
        timeRc.setConstraintType(ConstraintType.IN);
        for (TimeAndSpace time : availability) {
            timeRc.addToConstraintValueList(time.getTime().toString());
        }
        request.addConstraint(PluginDataObject.DATATIME_ID, timeRc);
        return request;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws DataCubeException {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>();
        DbQueryResponse dbresponse = (DbQueryResponse) response;
        for (Map<String, Object> map : dbresponse.getResults()) {
            DataTime time = (DataTime) map.get(PluginDataObject.DATATIME_ID);
            int gid = ((Number) map.get(SatelliteInventory.GID)).intValue();
            SatMapCoverage coverage = coverages.get(gid);
            result.add(new TimeAndSpace(time, new ComparableSatMapCoverage(
                    coverage)));
        }
        return result;
    }


    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws DataCubeException {
        DbQueryResponse queryResponse = (DbQueryResponse) response;
        List<Map<String, Object>> results = queryResponse.getResults();
        Set<AbstractRequestableData> data = new HashSet<AbstractRequestableData>(
                results.size());
        SatelliteRecord[] records = queryResponse
                .getEntityObjects(SatelliteRecord.class);
        for (SatelliteRecord record : records) {
            data.add(new SatelliteRequestableData(record, getLevel()));
        }
        return data;
    }

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

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        SatelliteRequestableLevelNode other = (SatelliteRequestableLevelNode) obj;
        if (requestConstraints == null) {
            if (other.requestConstraints != null)
                return false;
        } else if (!requestConstraints.equals(other.requestConstraints))
            return false;
        return true;
    }

}
