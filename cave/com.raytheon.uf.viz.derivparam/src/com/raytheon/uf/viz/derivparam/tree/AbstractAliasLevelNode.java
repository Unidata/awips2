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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * A Node for all derived nodes which take a single parameter and do something
 * to get the final result, either through time, perturbation, or model
 * manipulation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractAliasLevelNode extends AbstractDerivedLevelNode {

    protected AbstractRequestableLevelNode sourceNode;

    public AbstractAliasLevelNode(AbstractAliasLevelNode that) {
        super(that);
        this.sourceNode = that.sourceNode;
    }

    public AbstractAliasLevelNode(AbstractRequestableLevelNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName);
        this.sourceNode = sourceNode;
        setLevel(level);
    }

    @Override
    public Set<DataTime> timeQueryInternal(TimeQueryRequest originalRequest,
            boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        return sourceNode.timeQuery(originalRequest, latestOnly, cache,
                latestOnlyCache);
    }

    @Override
    public boolean isTimeAgnostic() {
        return sourceNode.isTimeAgnostic();
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraintMap() {
        // TODO Auto-generated method stub
        return sourceNode.getRequestConstraintMap();
    }

    @Override
    public boolean hasRequestConstraints() {
        return sourceNode.hasRequestConstraints();
    }

    @Override
    public List<Dependency> getDependencies() {
        return Arrays.asList(new Dependency(sourceNode, 0));
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
                + ((sourceNode == null) ? 0 : sourceNode.hashCode());
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
        AbstractAliasLevelNode other = (AbstractAliasLevelNode) obj;
        if (sourceNode == null) {
            if (other.sourceNode != null)
                return false;
        } else if (!sourceNode.equals(other.sourceNode))
            return false;
        return true;
    }

}
