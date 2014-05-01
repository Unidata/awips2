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
package com.raytheon.uf.viz.d2d.xy.adapters.timeseries;

import java.util.ArrayList;
import java.util.Iterator;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.radar.util.DmdTools;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Radar DMD data on a Time Series display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DmdTimeSeriesAdapter extends
        AbstractTimeSeriesAdapter<RadarRecord> {

    private static final double MIN_DISTANCE = 0.5;

    private Unit<?> unit = Unit.ONE;

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#
     * getDataUnit()
     */
    @Override
    public Unit<?> getDataUnit() {
        return unit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#getLevel
     * ()
     */
    @Override
    public SingleLevel getLevel() {
        SingleLevel level = new SingleLevel("SURFACE");
        level.setValue(0.0);
        return level;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#
     * getParamterName()
     */
    @Override
    public String getParameterName() {
        return resourceData.getYParameter().name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter#loadData
     * (com.raytheon.uf.viz.core.rsc.DisplayType)
     */
    @Override
    public XYDataList loadData() throws VizException {
        RadarRecord[] recordsToLoad = null;
        synchronized (records) {
            recordsToLoad = new RadarRecord[records.size()];
            Iterator<RadarRecord> iter = records.iterator();
            for (int i = 0; i < recordsToLoad.length; ++i) {
                recordsToLoad[i] = iter.next();
            }
        }
        return loadInternal(recordsToLoad);
    }

    private XYDataList loadInternal(RadarRecord[] recordsToLoad)
            throws VizException {

        Coordinate target = this.resourceData.getCoordinate();
        String parameter = this.resourceData.getYParameter().code;
        String level = this.resourceData.getLevelKey();
        ArrayList<XYData> data = new ArrayList<XYData>();

        // The toArray prevents concurrent modification
        for (PluginDataObject pdo : recordsToLoad) {
            RadarRecord record = (RadarRecord) pdo;
            DmdTools.retrieveFromDataStore(record);
            for (String featureId : RadarRecordUtil.getDMDFeatureIDs(record)) {
                Coordinate loc = RadarRecordUtil.getDMDLonLatFromFeatureID(
                        record, featureId);
                if (target.distance(loc) < MIN_DISTANCE) {
                    Number y = DmdTools.getParameter(record, featureId, level,
                            parameter);
                    if (y != null && y.intValue() != -9999) {
                        DataTime x = record.getDataTime();
                        data.add(new XYData(x, y));
                        if (this.unit == Unit.ONE) {
                            this.unit = DmdTools.getUnit(record, featureId,
                                    level, parameter);
                        }
                        break;
                    }
                }
            }
        }
        if (data.size() == 0) {
            throw new VizException("No DMD data available for selected point");
        }

        XYDataList list = new XYDataList();
        list.setData(data);

        return list;
    }

    /**
     * loads an individual record instead of all data
     * 
     * @param pdo
     */
    @Override
    public XYDataList loadRecord(PluginDataObject pdo) throws VizException {
        RadarRecord[] recordsToLoad = new RadarRecord[1];
        recordsToLoad[0] = (RadarRecord) pdo;
        return loadInternal(recordsToLoad);
    }

}
