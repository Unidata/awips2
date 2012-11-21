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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.CubeRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher.MatchResult;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * Provided a mechanism for requesting data for an entire 3D cube. If a Level is
 * set it will request records for all Standard levels within that composite
 * level, otherwise it will request all MB records. It will respond to time
 * queries with the Union of all levels it represents, although in the future
 * this may need to be changed to the intersection, or a limited intersection
 * when at least 3 levels are available. It returns all the GribRecords from all
 * the level nodes it represents, these should be sorted by the requesting node.
 * Finally it attempts to merge any requests to avoid the overhead of multiple
 * requests to EDEX.
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
public abstract class AbstractCubeLevelNode extends AbstractDerivedDataNode {

    protected List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels;

    public AbstractCubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
        this.levels = that.levels;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels,
            String modelName) {
        this.levels = levels;
        this.modelName = modelName;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            Level level,
            String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels) {
        this.setLevel(level);
        this.levels = levels;
        this.modelName = modelName;
        this.setValue("3D");
    }

    public AbstractCubeLevelNode(
            Level level,
            DerivParamDesc desc,
            DerivParamMethod method,
            String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels) {
        super(level, desc, method, modelName);
        this.levels = levels;
        this.setValue("3D");
    }

    @Override
    public Level getLevel() {
        return null;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) throws VizException {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        for (CubeLevel<AbstractRequestableNode, AbstractRequestableNode> level : levels) {
            result.put(level.getParam(), availability);
            result.put(level.getPressure(), availability);
        }
        return result;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Map<TimeAndSpace, List<AbstractRequestableData>> paramMap = new HashMap<TimeAndSpace, List<AbstractRequestableData>>();
        Map<TimeAndSpace, List<AbstractRequestableData>> presMap = new HashMap<TimeAndSpace, List<AbstractRequestableData>>();
        for (CubeLevel<AbstractRequestableNode, AbstractRequestableNode> level : levels) {
            Set<AbstractRequestableData> paramRecs = dependencyData.get(level
                    .getParam());
            for (AbstractRequestableData paramRec : paramRecs) {
                TimeAndSpace ast = paramRec.getTimeAndSpace();
                List<AbstractRequestableData> paramList = paramMap.get(ast);
                if (paramList == null) {
                    paramList = new ArrayList<AbstractRequestableData>();
                    paramMap.put(ast, paramList);
                }
                paramList.add(paramRec);
            }
            Set<AbstractRequestableData> presRecs = dependencyData.get(level
                    .getPressure());
            for (AbstractRequestableData presRec : presRecs) {
                TimeAndSpace ast = presRec.getTimeAndSpace();
                List<AbstractRequestableData> presList = presMap.get(ast);
                if (presList == null) {
                    presList = new ArrayList<AbstractRequestableData>();
                    presMap.put(ast, presList);
                }
                presList.add(presRec);
            }

        }
        Map<TimeAndSpace, MatchResult> matches = new TimeAndSpaceMatcher()
                .match(paramMap.keySet(), presMap.keySet());
        Set<AbstractRequestableData> records = new HashSet<AbstractRequestableData>();
        for (Entry<TimeAndSpace, MatchResult> match : matches.entrySet()) {
            List<AbstractRequestableData> paramList = paramMap.get(match
                    .getValue().get1());
            CubeRequestableData record = new CubeRequestableData(
                    paramList.get(0));
            for (AbstractRequestableData paramRec : paramList) {
                record.addParam(paramRec);
            }
            List<AbstractRequestableData> presList = presMap.get(match
                    .getValue().get2());
            for (AbstractRequestableData presRec : presList) {
                record.addPressure(presRec);
            }
            record.validate();
            if (record.size() >= 2) {
                record.setDataTime(match.getKey().getTime());
                record.setSpace(match.getKey().getSpace());
                modifyRequest(record);
                records.add(record);
            }
        }
        return records;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        // things in one are available for one level
        Set<TimeAndSpace> one = new HashSet<TimeAndSpace>();
        // things in two are available for two levels.
        Set<TimeAndSpace> two = new HashSet<TimeAndSpace>();

        for (CubeLevel<AbstractRequestableNode, AbstractRequestableNode> level : levels) {
            for (TimeAndSpace time : availability.get(level.getParam())) {
                if (one.contains(time)) {
                    two.add(time);
                } else {
                    one.add(time);
                }
            }
        }
        return two;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode#
     * getDependencies()
     */
    public List<Dependency> getDependencies() {
        List<Dependency> dependencies = new ArrayList<Dependency>(
                levels.size() * 2);
        for (CubeLevel<AbstractRequestableNode, AbstractRequestableNode> level : levels) {
            dependencies.add(new Dependency(level.getPressure(), 0));
            dependencies.add(new Dependency(level.getParam(), 0));
        }
        return dependencies;
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
        result = prime * result + ((levels == null) ? 0 : levels.hashCode());
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
        AbstractCubeLevelNode other = (AbstractCubeLevelNode) obj;
        if (levels == null) {
            if (other.levels != null)
                return false;
        } else if (!levels.equals(other.levels))
            return false;
        return true;
    }

}
