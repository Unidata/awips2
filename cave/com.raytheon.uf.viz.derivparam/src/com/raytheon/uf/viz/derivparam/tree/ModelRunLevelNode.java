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
import java.util.Set;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * Builds AggregateRecords which contain all forecast times up to the requested
 * time.
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
public class ModelRunLevelNode extends AbstractAliasLevelNode {

    public ModelRunLevelNode(ModelRunLevelNode that) {
        super(that);
    }

    public ModelRunLevelNode(AbstractRequestableLevelNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName, level);
    }

    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Set<DataTime> allTimes = this.timeQuery(false);
        Set<DataTime> neededTimes = null;
        DataTime[] requestedTimes = property.getSelectedEntryTime();
        if (requestedTimes == null) {
            neededTimes = allTimes;
        } else {
            neededTimes = new HashSet<DataTime>();
            for (DataTime time : allTimes) {
                for (DataTime rTime : property.getSelectedEntryTime()) {
                    if (rTime.getRefTime().equals(time.getRefTime())
                            && rTime.getFcstTime() >= time.getFcstTime()) {
                        neededTimes.add(time);
                        break;
                    }
                }
            }

        }
        Map<DataTime, List<AbstractRequestableData>> timeBins = new HashMap<DataTime, List<AbstractRequestableData>>();
        if (requestedTimes == null) {
            for (DataTime time : allTimes) {
                timeBins.put(time, new ArrayList<AbstractRequestableData>());
            }
        } else {
            for (DataTime time : requestedTimes) {
                timeBins.put(time, new ArrayList<AbstractRequestableData>());
            }
        }
        property.setSelectedEntryTimes(neededTimes
                .toArray(new DataTime[neededTimes.size()]));

        for (AbstractRequestableData record : sourceNode.getData(property,
                timeOut, cache)) {
            for (Entry<DataTime, List<AbstractRequestableData>> entry : timeBins
                    .entrySet()) {
                DataTime keyTime = entry.getKey();
                DataTime recordTime = record.getDataTime();
                if (keyTime.getRefTime().getTime() == recordTime.getRefTime()
                        .getTime()
                        && keyTime.getFcstTime() >= recordTime.getFcstTime()) {
                    entry.getValue().add(record);
                }
            }
        }
        List<AbstractRequestableData> records = new ArrayList<AbstractRequestableData>();
        for (Entry<DataTime, List<AbstractRequestableData>> entry : timeBins
                .entrySet()) {
            if (!entry.getValue().isEmpty()) {
                AggregateRequestableData newRecord = new AggregateRequestableData(
                        entry.getValue());
                newRecord.setDataTime(entry.getKey());
                modifyRequest(newRecord);
                records.add(newRecord);
            }
        }
        return records;
    }

    @Override
    public ModelRunLevelNode clone() {
        return new ModelRunLevelNode(this);
    }

}
