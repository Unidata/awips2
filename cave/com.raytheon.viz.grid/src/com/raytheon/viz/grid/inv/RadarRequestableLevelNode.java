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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.viz.grid.data.RadarRequestableData;

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
public class RadarRequestableLevelNode extends AbstractRequestableLevelNode {

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

    @Override
    public boolean isTimeAgnostic() {
        return false;
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return rcMap;
    }

    @Override
    public boolean hasRequestConstraints() {
        return true;
    }

    @Override
    public Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        Set<DataTime> resultsSet = RadarUpdater.getInstance().getTimes(this);
        if (resultsSet != null) {
            return resultsSet;
        }

        DataTime[] results = CatalogQuery.performTimeQuery(rcMap, latestOnly,
                null);
        if (results != null) {
            resultsSet = new HashSet<DataTime>(results.length);
            for (int i = 0; i < results.length; i++) {
                resultsSet.add(results[i]);
            }
            if (!latestOnly) {
                RadarUpdater.getInstance().setTimes(this, resultsSet);
            }
            return resultsSet;
        } else {
            return null;
        }
    }

    @Override
    protected TimeQueryRequest getTimeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache)
            throws VizException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        DbQueryRequest dbRequest = getDataQueryInternal(property, timeOut,
                cache);
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(dbRequest);
        return processDataQueryResults(response);
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
        Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>(
                rcMap);
        DbQueryRequest dbRequest = new DbQueryRequest();
        DataTime[] times = property.getSelectedEntryTime();
        if (times != null && times.length > 0) {
            RequestConstraint dtRC = new RequestConstraint();
            dtRC.setConstraintType(ConstraintType.IN);
            for (DataTime t : times) {
                dtRC.addToConstraintValueList(t.toString());
            }
            newQuery.put("dataTime", dtRC);
        }
        newQuery.put("pluginName", new RequestConstraint("radar"));
        dbRequest.setConstraints(newQuery);
        return dbRequest;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * processQueryResults
     * (com.raytheon.uf.common.dataquery.responses.DbQueryResponse)
     */
    @Override
    protected List<AbstractRequestableData> processDataQueryResults(
            DbQueryResponse response) throws VizException {
        List<Map<String, Object>> rows = response.getResults();
        List<AbstractRequestableData> rval = new ArrayList<AbstractRequestableData>(
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

    @Override
    public List<Dependency> getDependencies() {
        return Collections.emptyList();
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
