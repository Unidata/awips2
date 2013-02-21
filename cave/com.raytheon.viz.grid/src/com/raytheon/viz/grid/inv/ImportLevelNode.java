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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.viz.grid.data.ImportRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * This node handles all Alias derived parameters which includes Import in AWIPS
 * I. Data requests and Time queries are simply forwarded to the source nodes.
 * Returned records are wrapped in an ALiasRecord which can handle unit
 * conversion and model conversion if necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2010 #3965      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ImportLevelNode extends AbstractAliasLevelNode {
    private Map<DataTime, DataTime[]> boundingSourceTimes = new HashMap<DataTime, DataTime[]>();

    private String sourceNodeModelName;

    public ImportLevelNode(ImportLevelNode that) {
        super(that);
        this.sourceNodeModelName = that.sourceNodeModelName;
    }

    public ImportLevelNode(AbstractRequestableNode sourceNode,
            String sourceNodeModelName, DerivParamDesc desc,
            DerivParamMethod method, String modelName, Level level) {
        super(sourceNode, desc, method, modelName, level);
        this.sourceNodeModelName = sourceNodeModelName;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Set<AbstractRequestableData> origs = dependencyData.get(sourceNode);
        Set<AbstractRequestableData> results = new HashSet<AbstractRequestableData>(
                origs.size());
        Map<DataTime, AbstractRequestableData> timeMap = new HashMap<DataTime, AbstractRequestableData>(
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
                ISpatialObject space = time.getSpace();
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
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        result.put(sourceNode, getSourceAvailability(times));
        return result;
    }

    private Set<TimeAndSpace> getSourceAvailability(Set<TimeAndSpace> times) {
        Set<TimeAndSpace> neededTimes = new HashSet<TimeAndSpace>();
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
            throws VizException {
        boundingSourceTimes.clear();

        // grab this source and discover all available times ala time agnostic,
        // then see what is available in the imported source, use time
        // interpolation to verify what data can be achieved
        NavigableSet<DataTime> sourceDataTimes = new TreeSet<DataTime>();
        for (TimeAndSpace ast : availability.get(sourceNode)) {
            sourceDataTimes.add(ast.getTime());
        }
        DatasetInfo sourceInfo = DatasetInfoLookup.getInstance().getInfo(
                sourceNodeModelName);
        long sourceDt = 0;
        if (sourceInfo != null) {
            sourceDt = sourceInfo.getDt();
        }

        if (sourceDt <= 24) {
            sourceDt *= 3600000;
        }
        if (RadarAdapter.RADAR_SOURCE.equals(modelName)) {
            Set<DataTime> radarTimes = RadarAdapter.getInstance()
                    .timeInvariantQuery();

            for (DataTime radarTime : radarTimes) {
                if (sourceDataTimes.contains(radarTime)) {
                    boundingSourceTimes.put(radarTime,
                            new DataTime[] { radarTime });
                } else {
                    DataTime before = sourceDataTimes.floor(radarTime);
                    DataTime after = sourceDataTimes.ceiling(radarTime);
                    if (before != null
                            && after != null
                            && (after.getValidTime().getTimeInMillis() - before
                                    .getValidTime().getTimeInMillis()) <= sourceDt) {
                        boundingSourceTimes.put(radarTime, new DataTime[] {
                                before, after });
                    }
                }
            }
        } else {
            for (DataTime time : sourceDataTimes) {
                boundingSourceTimes.put(time, new DataTime[] { time });
            }
        }
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>();
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
