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
import java.util.Set;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.viz.grid.data.RadarRequestableData;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * 
 * A basic node which fulfills time and data requests by going to edex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010 #4473      rjpeter      Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarRequestableLevelNode extends AbstractBaseDataNode {

    protected static final String TIME_FIELD = "dataTime";

    protected Map<String, RequestConstraint> rcMap;

    protected String paramAbbrev;

    protected String paramName;

    /**
     * Copy constructor
     * 
     * @param that
     */
    public RadarRequestableLevelNode(RadarRequestableLevelNode that) {
        super(that);
        this.rcMap = that.rcMap;
        this.paramAbbrev = that.paramAbbrev;
        this.paramName = that.paramName;
    }

    /**
     * Create a new requestable Level Node that is LevelNode clone of that and
     * uses rcMap for all Requests
     * 
     * @param that
     * @param rcMap
     */
    public RadarRequestableLevelNode(LevelNode that,
            Map<String, RequestConstraint> rcMap, String paramAbbrev,
            String paramName) {
        super(that);
        this.rcMap = rcMap;
        this.paramAbbrev = paramAbbrev;
        this.paramName = paramName;
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
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        return null;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws VizException {
        Set<TimeAndSpace> resultsSet = RadarUpdater.getInstance()
                .getTimes(this);
        if (resultsSet != null) {
            return resultsSet;
        }

        DataTime[] results = CatalogQuery.performTimeQuery(rcMap, false, null);
        if (results != null) {
            resultsSet = new HashSet<TimeAndSpace>(results.length);
            for (int i = 0; i < results.length; i++) {
                resultsSet.add(new TimeAndSpace(results[i], RadarAdapter
                        .getInstance().getCoverage()));
            }
            RadarUpdater.getInstance().setTimes(this, resultsSet);
            return resultsSet;
        } else {
            return null;
        }
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability) {
        Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>(
                rcMap);
        DbQueryRequest dbRequest = new DbQueryRequest();
        RequestConstraint dtRC = new RequestConstraint();
        dtRC.setConstraintType(ConstraintType.IN);
        for (TimeAndSpace ast : availability) {
            dtRC.addToConstraintValueList(ast.getTime().toString());
        }
        newQuery.put("dataTime", dtRC);
        newQuery.put("pluginName", new RequestConstraint("radar"));
        dbRequest.setConstraints(newQuery);
        return dbRequest;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws VizException {
        List<Map<String, Object>> rows = ((DbQueryResponse) response)
                .getResults();
        Set<AbstractRequestableData> rval = new HashSet<AbstractRequestableData>(
                rows.size());
        // switch to GribRequestableData and build a grib record from the radar
        // record... won't work because of the get data call, can't be a
        // GribRecord, needs to call getDataValue on Requestable
        for (Map<String, Object> objMap : rows) {
            rval.add(new RadarRequestableData((RadarRecord) objMap.get(null),
                    paramAbbrev));
        }
        return rval;
    }

    public String getParamAbbrev() {
        return paramAbbrev;
    }

    @Override
    public RadarRequestableLevelNode clone() {
        return new RadarRequestableLevelNode(this);
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
        RadarRequestableLevelNode other = (RadarRequestableLevelNode) obj;
        if (rcMap == null) {
            if (other.rcMap != null)
                return false;
        } else if (!rcMap.equals(other.rcMap))
            return false;
        return true;
    }
}
