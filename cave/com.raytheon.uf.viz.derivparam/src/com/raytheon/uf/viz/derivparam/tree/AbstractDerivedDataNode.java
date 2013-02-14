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

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * A Base class for derived level nodes, the primary purpose is to hold the
 * Description and method objects for a derived parameter and to provide a
 * common method for modifying records to match what is provided by these
 * objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractDerivedDataNode extends AbstractRequestableNode {

    protected DerivParamDesc desc;

    protected DerivParamMethod method;

    protected String modelName;

    public AbstractDerivedDataNode() {
    }

    public AbstractDerivedDataNode(Level level, DerivParamDesc desc,
            String modelName) {
        this(level, desc, null, modelName);
    }

    public AbstractDerivedDataNode(Level level, DerivParamDesc desc,
            DerivParamMethod method, String modelName) {
        super(level);
        this.desc = desc;
        this.method = method;
        this.modelName = modelName;
    }

    public AbstractDerivedDataNode(LevelNode that, DerivParamDesc desc,
            DerivParamMethod method, String modelName) {
        super(that);
        this.desc = desc;
        this.method = method;
        this.modelName = modelName;
    }

    public AbstractDerivedDataNode(AbstractDerivedDataNode that) {
        super(that);
        this.desc = that.desc;
        this.method = that.method;
        this.modelName = that.modelName;
    }

    /**
     * @return the desc
     */
    public DerivParamDesc getDesc() {
        return desc;
    }

    /**
     * @param desc
     *            the desc to set
     */
    public void setDesc(DerivParamDesc desc) {
        this.desc = desc;
    }

    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @param modelName
     *            the modelName to set
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public DerivParamMethod getMethod() {
        return method;
    }

    public void setMethod(DerivParamMethod method) {
        this.method = method;
    }

    /**
     * Sets the vector, abbreviation, name, unit, and level to match the desc
     * and level in this node
     * 
     * @param record
     */
    protected void modifyRequest(AbstractRequestableData requester) {
        if (desc != null) {
            requester.setSource(modelName);
            requester.setParameter(desc.getAbbreviation());
            requester.setParameterName(desc.getName());
            requester.setUnit(desc.getUnit());
        }
        if (method != null && method.getDisplayName() != null) {
            requester.setParameterName(method.getDisplayName());
        }
        requester.setLevel(getLevel());
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
        result = prime * result + ((desc == null) ? 0 : desc.hashCode());
        result = prime * result + ((method == null) ? 0 : method.hashCode());
        result = prime * result
                + ((modelName == null) ? 0 : modelName.hashCode());
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
        AbstractDerivedDataNode other = (AbstractDerivedDataNode) obj;
        if (desc == null) {
            if (other.desc != null)
                return false;
        } else if (!desc.equals(other.desc))
            return false;
        if (method == null) {
            if (other.method != null)
                return false;
        } else if (!method.equals(other.method))
            return false;
        if (modelName == null) {
            if (other.modelName != null)
                return false;
        } else if (!modelName.equals(other.modelName))
            return false;
        return true;
    }

    /**
     * Given the availability of all dependency nodes determine when this node
     * is available.
     * 
     * @param availability
     * @return
     * @throws VizException
     */
    public abstract Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException;

    /**
     * If this node has dependency on other nodes then this method should return
     * what data is needed for the dependencies to get data from this node for
     * the provided availability.
     * 
     * @param availability
     * @return
     */
    public abstract Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability, AvailabilityContainer container)
            throws VizException;

    /**
     * Get data from this node.
     * 
     * @param availability
     *            The time and spatial area where data is needed
     * @return
     */
    public abstract Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException;

    public abstract List<Dependency> getDependencies();

    @Override
    public boolean isConstant() {
        List<Dependency> dependencies = getDependencies();
        if (dependencies.isEmpty()) {
            return false;
        }
        for (Dependency dep : getDependencies()) {
            if (!dep.node.isConstant()) {
                return false;
            }
        }
        // All dependencies must be constant
        return true;
    }
}
