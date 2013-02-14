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
package com.raytheon.viz.grid.inv;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.viz.grid.data.GridRequestableDataFactory;
import com.raytheon.viz.grid.util.CoverageUtils;

/**
 * 
 * A basic node which fulfills time and data requests by going to edex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridRequestableNode extends AbstractBaseDataNode {

    protected static final String TIME_FIELD = "dataTime";

    protected Map<String, RequestConstraint> rcMap;

    private List<String> ensembles;

    /**
     * Copy constructor
     * 
     * @param that
     */
    public GridRequestableNode(GridRequestableNode that) {
        super(that);
        this.rcMap = that.rcMap;
    }

    /**
     * Create a new requestable Level Node that is LevelNode clone of that and
     * uses rcMap for all Requests
     * 
     * @param that
     * @param rcMap
     */
    public GridRequestableNode(LevelNode that,
            Map<String, RequestConstraint> rcMap) {
        super(that);
        this.rcMap = rcMap;
    }

    /**
     * @param rcMap
     *            the rcMap to set
     */
    public void setRequestConstraintMap(Map<String, RequestConstraint> rcMap) {
        this.rcMap = rcMap;
    }

    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return rcMap;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws VizException {
        DbQueryResponse dbresponse = (DbQueryResponse) response;
        GridRequestableDataFactory grdf = GridRequestableDataFactory
                .getInstance();
        Set<AbstractRequestableData> ards = new HashSet<AbstractRequestableData>();
        for (Map<String, Object> result : dbresponse.getResults()) {
            GridRecord record = (GridRecord) result.get(null);
            ards.add(grdf.getGridRequestableData(record));
        }
        return ards;
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability) {
        boolean timeAgnostic = false;
        boolean spaceAgnostic = false;
        Set<DataTime> times = new HashSet<DataTime>();
        Set<ISpatialObject> spaces = new HashSet<ISpatialObject>();
        for (TimeAndSpace ast : availability) {
            if (ast.isTimeAgnostic()) {
                timeAgnostic = true;
            } else {
                times.add(ast.getTime());
            }
            if (ast.isSpaceAgnostic()) {
                spaceAgnostic = true;
            } else {
                spaces.add(ast.getSpace());
            }
        }
        DbQueryRequest dbRequest = new DbQueryRequest();
        dbRequest.setEntityClass(GridRecord.class.getName());
        dbRequest.setConstraints(new HashMap<String, RequestConstraint>(rcMap));
        for (Entry<String, RequestConstraint> e : orignalConstraints.entrySet()) {
            if (!rcMap.containsKey(e.getKey())) {
                dbRequest.addConstraint(e.getKey(), e.getValue());
            }
        }
        if (!timeAgnostic) {
            RequestConstraint timeRc = new RequestConstraint();
            timeRc.setConstraintType(ConstraintType.IN);
            for (DataTime time : times) {
                timeRc.addToConstraintValueList(time.toString());
            }
            dbRequest.addConstraint(TIME_FIELD, timeRc);
        }
        if (!spaceAgnostic) {
            RequestConstraint spaceRc = new RequestConstraint();
            spaceRc.setConstraintType(ConstraintType.IN);
            for (ISpatialObject space : spaces) {
                if (space instanceof GridCoverage) {
                    spaceRc.addToConstraintValueList(Integer
                            .toString(((GridCoverage) space).getId()));
                } else {
                    // TODO figure out the intersection of my spatial object
                    // with this spatial object.
                }
            }
            dbRequest.addConstraint(GridConstants.LOCATION_ID, spaceRc);
        }
        return dbRequest;
    }

    @Override
    public GridRequestableNode clone() {
        return new GridRequestableNode(this);
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
        result = prime * result + ((rcMap == null) ? 0 : rcMap.hashCode());
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
        GridRequestableNode other = (GridRequestableNode) obj;
        if (rcMap == null) {
            if (other.rcMap != null)
                return false;
        } else if (!rcMap.equals(other.rcMap))
            return false;
        return true;
    }

    public List<String> getEnsembles() {
        return ensembles;
    }

    public void setEnsembles(List<String> ensembles) {
        this.ensembles = ensembles;
    }

    private String getSource() {
        return rcMap.get(GridConstants.DATASET_ID).getConstraintValue();
    }

    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        if (GridTimeCache.getInstance().getTimes(this) != null) {
            return null;
        }
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridRecord.class.getName());
        request.addRequestField(TIME_FIELD);
        try {
            if (CoverageUtils.getInstance().getCoverages(getSource()).size() > 1) {
                request.addRequestField(GridConstants.LOCATION_ID);
            }
        } catch (VizException e) {
            // not really a problem, just request the location from db.
            UFStatus.getHandler().handle(Priority.VERBOSE,
                    e.getLocalizedMessage(), e);
            request.addRequestField(GridConstants.LOCATION_ID);
        }
        request.setDistinct(true);
        request.setConstraints(rcMap);
        return request;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws VizException {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>();
        if (response == null) {
            result = GridTimeCache.getInstance().getTimes(this);
            if (result == null) {
                // Oh No! the cache has been cleared since we made our request.
                response = ThriftClient
                        .sendRequest(getAvailabilityRequest(originalConstraints));
                return getAvailability(originalConstraints, response);
            }
            GridTimeCache.getInstance().setTimes(this, result);
        } else if (response instanceof DbQueryResponse) {
            DbQueryResponse dbresponse = (DbQueryResponse) response;
            for (Map<String, Object> map : dbresponse.getResults()) {
                DataTime time = (DataTime) map.get(TIME_FIELD);
                GridCoverage coverage = null;
                if (map.containsKey(GridConstants.LOCATION_ID)) {
                    Number locationId = (Number) map
                            .get(GridConstants.LOCATION_ID);
                    coverage = GridCoverageLookup.getInstance().getCoverage(
                            locationId.intValue());
                } else {
                    coverage = CoverageUtils.getInstance()
                            .getCoverages(getSource()).iterator().next();
                }
                result.add(new TimeAndSpace(time, coverage));
            }
            GridTimeCache.getInstance().setTimes(this, result);
        }
        return result;
    }

}
