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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
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
import com.raytheon.viz.grid.data.GribRequestableDataFactory;

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
public class GribRequestableLevelNode extends AbstractRequestableLevelNode {

    protected static final String TIME_FIELD = "dataTime";

    protected Map<String, RequestConstraint> rcMap;

    private List<Integer> perts;

    /**
     * Copy constructor
     * 
     * @param that
     */
    public GribRequestableLevelNode(GribRequestableLevelNode that) {
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
    public GribRequestableLevelNode(LevelNode that,
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
        Set<DataTime> resultsSet = GribTimeCache.getInstance().getTimes(this);
        if (resultsSet != null) {
            return resultsSet;
        }

        DataTime[] results = CatalogQuery.performTimeQuery(rcMap, latestOnly,
                null);
        if (results != null) {
            resultsSet = new HashSet<DataTime>(Arrays.asList(results));
            if (!latestOnly) {
                GribTimeCache.getInstance().setTimes(this, resultsSet);
            }
            return resultsSet;
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getTimeQueryInternal(boolean, java.util.Map)
     */
    @Override
    protected TimeQueryRequest getTimeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache)
            throws VizException {
        Set<DataTime> resultsSet = GribTimeCache.getInstance().getTimes(this);
        if (resultsSet != null) {
            return null;
        }
        return CatalogQuery.getTimeQuery(rcMap, latestOnly, null);
    }

    @Override
    protected void processTimeQueryResults(boolean latestOnly,
            List<DataTime> queryResponse) throws VizException {
        if (!latestOnly) {
            Set<DataTime> resultsSet = new HashSet<DataTime>(queryResponse);
            GribTimeCache.getInstance().setTimes(this, resultsSet);
        }
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
     * getDataQueryInternal(com.raytheon.uf.viz.core.catalog.LayerProperty, int)
     */
    @Override
    protected DbQueryRequest getDataQueryInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Map<String, RequestConstraint> oldQuery = property
                .getEntryQueryParameters(false);

        Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>(
                rcMap);
        for (Entry<String, RequestConstraint> e : oldQuery.entrySet()) {
            if (!newQuery.containsKey(e.getKey())) {
                newQuery.put(e.getKey(), e.getValue());
            }
        }

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
        newQuery.put("pluginName", new RequestConstraint("grib"));
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
        GribRequestableDataFactory factory = GribRequestableDataFactory
                .getInstance();
        for (Map<String, Object> objMap : rows) {
            rval.add(factory.getGribRequestableData((GribRecord) objMap
                    .get(null)));
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    @Override
    public List<Dependency> getDependencies() {
        return Collections.emptyList();
    }

    @Override
    public GribRequestableLevelNode clone() {
        return new GribRequestableLevelNode(this);
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
        GribRequestableLevelNode other = (GribRequestableLevelNode) obj;
        if (rcMap == null) {
            if (other.rcMap != null)
                return false;
        } else if (!rcMap.equals(other.rcMap))
            return false;
        return true;
    }

    /**
     * @return the perts
     */
    public List<Integer> getPerts() {
        return perts;
    }

    /**
     * @param perts
     *            the perts to set
     */
    public void setPerts(List<Integer> perts) {
        this.perts = perts;
    }
}
