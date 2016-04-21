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
package com.raytheon.uf.viz.d2d.xy.adapters.varheight;

import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.radar.util.DmdTools;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
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

public class DmdVarHeightAdapter extends AbstractVarHeightAdapter<RadarRecord> {

    private static final double MIN_DISTANCE = 0.5;

    protected Unit<?> xUnit = Unit.ONE;

    protected Unit<?> yUnit = Unit.ONE;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getParamterName
     * ()
     */
    @Override
    public String getParameterName() {
        return resourceData.getParameter();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getXUnits()
     */
    @Override
    public Unit<?> getXUnit() {
        return xUnit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getYUnits()
     */
    @Override
    public Unit<?> getYUnit() {
        return yUnit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#loadData(
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public List<XYData> loadData(DataTime currentTime) throws VizException {
        Coordinate target = this.resourceData.getPoint();
        double min = Math.min(heightScale.getMinVal(), heightScale.getMaxVal());
        double max = Math.max(heightScale.getMinVal(), heightScale.getMaxVal());
        String parameter = resourceData.getParameter();
        List<XYData> list = new ArrayList<XYData>();
        RadarRecord[] arrayRecords = this.records
                .toArray(new RadarRecord[this.records.size()]);
        for (int i = 0; i < arrayRecords.length; i++) {
            RadarRecord record = arrayRecords[i];
            DataTime pdoTime = record.getDataTime().clone();
            pdoTime.setLevelValue(null);
            if (resourceData.getBinOffset() != null) {
                pdoTime = resourceData.getBinOffset()
                        .getNormalizedTime(pdoTime);
            }
            DataTime cTime = currentTime.clone();
            cTime.setLevelValue(null);
            if (pdoTime.equals(cTime)) {
                DmdTools.retrieveFromDataStore(record);
                for (String featureId : RadarRecordUtil
                        .getDMDFeatureIDs(record)) {
                    Coordinate loc = RadarRecordUtil.getDMDLonLatFromFeatureID(
                            record, featureId);
                    if (target.distance(loc) < MIN_DISTANCE) {
                        for (String level : DmdTools.levels3d) {
                            Number x = DmdTools.getParameter(record, featureId,
                                    level, parameter);
                            Number y = DmdTools.getParameter(record, featureId,
                                    level, heightScale.getParameter());
                            if (y.intValue() > -9998 && x.intValue() > -9998) {
                                if (y.doubleValue() >= min
                                        && y.doubleValue() <= max) {
                                    list.add(new XYData(x, y));
                                    if (xUnit == Unit.ONE) {
                                        xUnit = DmdTools.getUnit(record,
                                                featureId, level, parameter);

                                    }
                                    if (yUnit == Unit.ONE) {
                                        yUnit = DmdTools.getUnit(record,
                                                featureId, level,
                                                heightScale.getParameter());

                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return list;
    }

}
