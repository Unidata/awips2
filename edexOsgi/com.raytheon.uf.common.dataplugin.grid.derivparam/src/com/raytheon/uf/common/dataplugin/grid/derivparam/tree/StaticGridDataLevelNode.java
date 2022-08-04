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
package com.raytheon.uf.common.dataplugin.grid.derivparam.tree;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.StaticGridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * A LevelNode for static grid data that is either constant or can be calculated
 * based off the coverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Apr 13, 2012           bsteffen  Initial creation
 * Mar 03, 2016  5439     bsteffen  Allow grid derived parameters from edex
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class StaticGridDataLevelNode extends AbstractBaseDataNode {

    protected String dataType;

    protected String source;

    public StaticGridDataLevelNode(String source, String dataType) {
        this.source = source;
        this.dataType = dataType;
    }

    public StaticGridDataLevelNode(String source, String dataType, Level level) {
        this(source, dataType);
        setLevel(level);
    }

    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        return null;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws DataCubeException {
        Set<TimeAndSpace> result = new HashSet<>();
        for (GridCoverage coverage : CoverageUtils.getInstance().getCoverages(
                source)) {
            result.add(new TimeAndSpace(coverage));
        }
        return result;
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability) {
        return null;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> orignalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws DataCubeException {
        Set<AbstractRequestableData> results = new HashSet<>();
        for (TimeAndSpace ast : availability) {
            if (ast.getSpace() instanceof GridCoverage) {
                GridCoverage coverage = (GridCoverage) ast.getSpace();
                AbstractRequestableData data = createRequestableData(coverage);
                data.setDataTime(ast.getTime());
                results.add(data);
            } else {
                for (GridCoverage coverage : CoverageUtils.getInstance()
                        .getCoverages(source)) {
                    AbstractRequestableData data = createRequestableData(coverage);
                    data.setDataTime(ast.getTime());
                    results.add(data);
                }
            }
        }
        return results;
    }

    protected AbstractRequestableData createRequestableData(
            GridCoverage coverage) {
        if (StaticGridDataType.getStringValues().contains(dataType)) {
            StaticGridDataType staticGridDataType = StaticGridDataType
                    .valueOf(dataType);
            return new StaticGridRequestableData(staticGridDataType, source,
                    coverage);
        }
        return null;
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((dataType == null) ? 0 : dataType.hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
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
        StaticGridDataLevelNode other = (StaticGridDataLevelNode) obj;
        if (dataType == null) {
            if (other.dataType != null)
                return false;
        } else if (!dataType.equals(other.dataType))
            return false;
        if (source == null) {
            if (other.source != null)
                return false;
        } else if (!source.equals(other.source))
            return false;
        return true;
    }

}
