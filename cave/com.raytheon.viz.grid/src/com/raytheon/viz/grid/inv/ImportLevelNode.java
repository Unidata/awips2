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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.viz.grid.data.ImportRequestableData;
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

    public ImportLevelNode(AbstractRequestableLevelNode sourceNode,
            String sourceNodeModelName, DerivParamDesc desc,
            DerivParamMethod method, String modelName, Level level) {
        super(sourceNode, desc, method, modelName, level);
        this.sourceNodeModelName = sourceNodeModelName;
    }

    @Override
    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        // change out the time queries
        DataTime[] selectedTimes = property.getSelectedEntryTime();
        int numberOfImages = property.getNumberOfImages();
        try {
            Set<DataTime> newSelectedTimes = new HashSet<DataTime>(
                    (int) (selectedTimes.length * 2.5));
            for (DataTime time : selectedTimes) {
                DataTime[] timesToAdd = boundingSourceTimes.get(time);

                // sanity check
                if (timesToAdd != null) {
                    for (DataTime t : timesToAdd) {
                        newSelectedTimes.add(t);
                    }
                }
            }

            property.setSelectedEntryTimes(newSelectedTimes
                    .toArray(new DataTime[0]));
            property.setNumberOfImages(Math.max(newSelectedTimes.size(),
                    numberOfImages));

            List<AbstractRequestableData> origs = sourceNode.getData(property,
                    timeOut, cache);
            List<AbstractRequestableData> results = new ArrayList<AbstractRequestableData>(
                    origs.size());
            Map<DataTime, AbstractRequestableData> timeMap = new HashMap<DataTime, AbstractRequestableData>(
                    (int) (origs.size() * 1.25) + 1);
            for (AbstractRequestableData orig : origs) {
                timeMap.put(orig.getDataTime(), orig);
            }

            // need to build ImportRequestableData by bounding nodes
            for (DataTime time : selectedTimes) {
                DataTime[] timesToAdd = boundingSourceTimes.get(time);
                if (timesToAdd != null) {
                    AbstractRequestableData beforeData = timeMap
                            .get(timesToAdd[0]);
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

                    AbstractRequestableData result = new ImportRequestableData(
                            beforeData, afterData, time);
                    modifyRequest(result);
                    results.add(result);
                }
            }
            return results;
        } finally {
            // make sure to return the selectedEntryTimes to proper value
            property.setSelectedEntryTimes(selectedTimes);
            property.setNumberOfImages(numberOfImages);
        }
    }

    @Override
    public Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        boundingSourceTimes.clear();

        // grab this source and discover all available times ala time agnostic,
        // then see what is available in the imported source, use time
        // interpolation to verify what data can be achieved
        NavigableSet<DataTime> sourceDataTimes = new TreeSet<DataTime>(
                sourceNode.timeQuery(latestOnly, cache, latestOnlyCache));
        GridModel sourceModel = GribModelLookup.getInstance().getModelByName(
                sourceNodeModelName);
        long sourceDt = 0;
        if (sourceModel != null) {
            sourceDt = sourceModel.getDt();
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

        return boundingSourceTimes.keySet();
    }

    @Override
    public ImportLevelNode clone() {
        return new ImportLevelNode(this);
    }
}
