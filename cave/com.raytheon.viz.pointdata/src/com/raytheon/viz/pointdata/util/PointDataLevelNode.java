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
package com.raytheon.viz.pointdata.util;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointDataLevelNode extends AbstractRequestableLevelNode {

    private String parameter;

    public PointDataLevelNode(PointDataLevelNode that) {
        super(that);
        this.parameter = that.parameter;
    }

    public PointDataLevelNode(LevelNode that, String parameter) {
        super(that);
        this.parameter = parameter;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDataInternal(com.raytheon.uf.viz.core.catalog.LayerProperty, int,
     * java.util.Map)
     */
    @Override
    protected List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        return cache.remove(this);
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
        throw new UnsupportedOperationException(
                "PointData nodes do not support returning data query");
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
            DbQueryResponse queryResponse) throws VizException {
        throw new UnsupportedOperationException(
                "PointData nodes do not support processing data query");
    }

    @Override
    protected TimeQueryRequest getTimeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache)
            throws VizException {
        throw new UnsupportedOperationException(
                "PointData nodes do not support returning time query");
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

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * timeQueryInternal(boolean, java.util.Map)
     */
    @Override
    protected Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        return TIME_AGNOSTIC;
    }

    @Override
    public boolean isTimeAgnostic() {
        return true;
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraintMap() {
        return null;
    }

    @Override
    public boolean hasRequestConstraints() {
        return false;
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
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
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
        PointDataLevelNode other = (PointDataLevelNode) obj;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        return true;
    }

    @Override
    public PointDataLevelNode clone() {
        return new PointDataLevelNode(this);
    }
}
