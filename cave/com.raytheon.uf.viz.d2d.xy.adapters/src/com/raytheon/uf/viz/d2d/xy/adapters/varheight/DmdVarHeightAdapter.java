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

import javax.measure.Unit;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.xy.tools.DmdTools;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;

import tech.units.indriya.AbstractUnit;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            bsteffen     Initial creation
 * Oct 13, 2015 4897      bkowal       Relocated {@link DmdTools} to a plugin
 *                                     that actually uses it.
 * Oct 29, 2022 8959      mapeters     Update how data time levels are set
 *
 * </pre>
 *
 * @author bsteffen
 */
public class DmdVarHeightAdapter extends AbstractVarHeightAdapter<RadarRecord> {

    private static final double MIN_DISTANCE = 0.5;

    protected Unit<?> xUnit = AbstractUnit.ONE;

    protected Unit<?> yUnit = AbstractUnit.ONE;

    @Override
    public String getParameterName() {
        return resourceData.getParameter();
    }

    @Override
    public Unit<?> getXUnit() {
        return xUnit;
    }

    @Override
    public Unit<?> getYUnit() {
        return yUnit;
    }

    @Override
    public List<XYData> loadData(DataTime currentTime) throws VizException {
        Coordinate target = this.resourceData.getPoint();
        double min = Math.min(heightScale.getMinVal(), heightScale.getMaxVal());
        double max = Math.max(heightScale.getMinVal(), heightScale.getMaxVal());
        String parameter = resourceData.getParameter();
        List<XYData> list = new ArrayList<>();
        RadarRecord[] arrayRecords = this.records
                .toArray(new RadarRecord[this.records.size()]);
        for (RadarRecord record : arrayRecords) {
            DataTime pdoTime = record.getDataTime().clone();
            pdoTime.clearLevel();
            if (resourceData.getBinOffset() != null) {
                pdoTime = resourceData.getBinOffset()
                        .getNormalizedTime(pdoTime);
            }
            DataTime cTime = currentTime.clone();
            cTime.clearLevel();
            if (pdoTime.equals(cTime)) {
                DmdTools.retrieveFromDataStore(record);
                for (String featureId : RadarRecordUtil
                        .getDMDFeatureIDs(record)) {
                    Coordinate loc = RadarRecordUtil
                            .getDMDLonLatFromFeatureID(record, featureId);
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
                                    if (xUnit == AbstractUnit.ONE) {
                                        xUnit = DmdTools.getUnit(record,
                                                featureId, level, parameter);

                                    }
                                    if (yUnit == AbstractUnit.ONE) {
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
