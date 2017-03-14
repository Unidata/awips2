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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.derivparam.CommonGridInventory;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.ImportRequestableData;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;

/**
 * This node handles all Alias derived parameters which includes Import in AWIPS
 * I. Data requests and Time queries are simply forwarded to the source nodes.
 * Returned records are wrapped in an AliasRecord which can handle unit
 * conversion and model conversion if necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 15, 2010  3965     rjpeter   Initial creation
 * Apr 11, 2014  2947     bsteffen  Switch spatial matching to use
 *                                  IGridGeometryProvider
 * Mar 03, 2016  5439     bsteffen  Allow grid derived parameters from edex
 * Mar 29, 2016  5446     skorolev  Use a time weighted average between the bounding grids for all import.
 * Apr 11, 2016  5439     bsteffen  Move to common.
 * 
 * </pre>
 * 
 * @author rjpeter
 */
public class ImportLevelNode extends AbstractAliasLevelNode {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImportLevelNode.class);

    private final CommonGridInventory inventory;

    private Map<DataTime, DataTime[]> boundingSourceTimes = new HashMap<>();

    private String sourceNodeModelName;

    public ImportLevelNode(ImportLevelNode that) {
        super(that);
        this.inventory = that.inventory;
        this.sourceNodeModelName = that.sourceNodeModelName;
    }

    /**
     * Constructor
     * 
     * @param sourceNode
     * @param sourceNodeModelName
     * @param desc
     * @param method
     * @param modelName
     * @param level
     */
    public ImportLevelNode(CommonGridInventory inventory,
            AbstractRequestableNode sourceNode,
            String sourceNodeModelName, DerivParamDesc desc,
            DerivParamMethod method, String modelName, Level level) {
        super(sourceNode, desc, method, modelName, level);
        this.inventory = inventory;
        this.sourceNodeModelName = sourceNodeModelName;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws DataCubeException {
        Set<AbstractRequestableData> origs = dependencyData.get(sourceNode);
        Set<AbstractRequestableData> results = new HashSet<>(
                origs.size());
        Map<DataTime, AbstractRequestableData> timeMap = new HashMap<>(
                (int) (origs.size() * 1.25) + 1);
        for (AbstractRequestableData orig : origs) {
            timeMap.put(orig.getDataTime(), orig);
        }

        // need to build ImportRequestableData by bounding nodes
        for (TimeAndSpace time : availability) {
            DataTime[] timesToAdd = boundingSourceTimes.get(time.getTime());
            if (timesToAdd != null) {
                AbstractRequestableData beforeData = timeMap.get(timesToAdd[0]);
                AbstractRequestableData afterData = null;

                if (beforeData == null) {
                    continue;
                }
                if (timesToAdd.length == 2) {
                    afterData = timeMap.get(timesToAdd[1]);
                    if (afterData == null) {
                        continue;
                    }
                }
                Collection<GridCoverage> spaces = null;
                IGridGeometryProvider space = time.getSpace();
                if (space.equals(TimeAndSpace.SPACE_AGNOSTIC)) {
                    spaces = CoverageUtils.getInstance().getCoverages(
                            sourceNodeModelName);
                } else if (space instanceof GridCoverage) {
                    spaces = Arrays.asList((GridCoverage) space);
                } else {
                    throw new IllegalArgumentException(
                            "Grid ImportLevelNode cannot import data into this space: "
                                    + String.valueOf(space));
                }

                for (GridCoverage coverage : spaces) {
                    AbstractRequestableData result = new ImportRequestableData(
                            beforeData, afterData, time.getTime());
                    result.setSpace(coverage);
                    modifyRequest(result);
                    results.add(result);
                }
            }
        }
        return results;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> times, AvailabilityContainer availabilityContainer) {
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<>();
        result.put(sourceNode, getSourceAvailability(times));
        return result;
    }

    /**
     * Gets Source Availability
     * 
     * @param times
     * @return
     */
    private Set<TimeAndSpace> getSourceAvailability(Set<TimeAndSpace> times) {
        Set<TimeAndSpace> neededTimes = new HashSet<>();
        for (TimeAndSpace time : times) {
            if (time.isTimeAgnostic()) {
                for (DataTime[] dtarr : boundingSourceTimes.values()) {
                    for (DataTime dt : dtarr) {
                        neededTimes.add(new TimeAndSpace(dt));
                    }
                }
                break;
            } else {
                for (DataTime dt : boundingSourceTimes.get(time.getTime())) {
                    neededTimes.add(new TimeAndSpace(dt));
                }
            }
        }
        return neededTimes;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws DataCubeException {
        boundingSourceTimes.clear();
        // grab this source and discover all available times ala time agnostic,
        // then see what is available in the imported source, use time
        // interpolation to verify what data can be achieved
        NavigableSet<DataTime> sourceDataTimes = new TreeSet<>();
        for (TimeAndSpace ast : availability.get(sourceNode)) {
            sourceDataTimes.add(ast.getTime());
        }
        // Get source dates for grid model
        List<DataTime> modelTimes = Collections.emptyList();
        try {
            Map<String, RequestConstraint> map = new HashMap<>();
            map.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                    GridConstants.GRID));
            map.put(GridConstants.DATASET_ID, new RequestConstraint(modelName));

            modelTimes = inventory.timeAgnosticQuery(map);

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Error to get dates for "
                    + modelName, e);
        }

        for (DataTime tm : modelTimes) {
            if (sourceDataTimes.contains(tm)) {
                boundingSourceTimes.put(tm, new DataTime[] { tm });
            } else {
                DataTime before = sourceDataTimes.floor(tm);
                DataTime after = sourceDataTimes.ceiling(tm);
                if (before != null && after != null) {
                    boundingSourceTimes.put(tm,
                            new DataTime[] {
                            before, after });
                }
            }
        }

        Set<TimeAndSpace> result = new HashSet<>();
        for (DataTime time : boundingSourceTimes.keySet()) {
            for (GridCoverage coverage : CoverageUtils.getInstance()
                    .getCoverages(modelName)) {
                result.add(new TimeAndSpace(time, coverage));
            }
        }
        return result;
    }

    @Override
    public ImportLevelNode clone() {
        return new ImportLevelNode(this);
    }
}
