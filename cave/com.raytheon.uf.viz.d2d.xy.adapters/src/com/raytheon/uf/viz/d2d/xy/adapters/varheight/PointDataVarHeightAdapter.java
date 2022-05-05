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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.CubeUtil;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

import tec.uom.se.AbstractUnit;

/**
 * Adapter for converting pdos that are compatible with the point data api into
 * data that can be used for Var Height graphs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 07, 2010           bsteffen    Initial creation
 * May 13, 2013  1869     bsteffen    Modified D2D height Graphs to work
 *                                    without dataURI column.
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * Feb 06, 2018  6829     njensen     Check for wind
 * Apr 26, 2018  6974     bsteffen    Improve results when yUnit does not match scale.
 * Apr 15, 2019  7596     lsingh      Updated units framework to JSR-363.
 *                                    Handled unit conversion.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointDataVarHeightAdapter
        extends AbstractVarHeightAdapter<PluginDataObject> {

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
        Map<String, RequestConstraint> constraints = new HashMap<>(
                resourceData.getMetadataMap());
        if (resourceData.getBinOffset() != null) {
            TimeRange range = resourceData.getBinOffset()
                    .getTimeRange(currentTime);
            RequestConstraint timeConstraint = new RequestConstraint();
            timeConstraint.setConstraintType(ConstraintType.BETWEEN);
            String start = TimeUtil.formatToSqlTimestamp(range.getStart());
            String end = TimeUtil.formatToSqlTimestamp(range.getEnd());
            timeConstraint.setBetweenValueList(new String[] { start, end });
            constraints.put(PluginDataObject.REFTIME_ID, timeConstraint);
        } else if (currentTime.getUtilityFlags().contains(FLAG.FCST_USED)) {
            constraints.put(PluginDataObject.DATATIME_ID,
                    new RequestConstraint(currentTime.toString()));
        } else {
            constraints.put(PluginDataObject.REFTIME_ID, new RequestConstraint(
                    TimeUtil.formatToSqlTimestamp(currentTime.getRefTime())));
        }
        String parameter = resourceData.getParameter();
        PointDataContainer pdc;
        try {
            pdc = DataCubeContainer.getPointData(
                    records.iterator().next().getPluginName(),
                    new String[] { parameter, heightScale.getParameter() },
                    constraints);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
        xUnit = pdc.getDescription(parameter).getUnitObject();
        yUnit = pdc.getDescription(heightScale.getParameter()).getUnitObject();

        double yMin = Math.min(heightScale.getMinVal(),
                heightScale.getMaxVal());
        double yMax = Math.max(heightScale.getMinVal(),
                heightScale.getMaxVal());
        Unit<?> scaleYUnit = heightScale.getParameterUnit();
        if (scaleYUnit != null && scaleYUnit.isCompatible(yUnit)) {
            UnitConverter converter = UnitConv.getConverterToUnchecked(scaleYUnit, yUnit);
            yMin = converter.convert(yMin);
            yMax = converter.convert(yMax);
        }

        for (int uriCounter = 0; uriCounter < pdc
                .getAllocatedSz(); uriCounter++) {
            List<XYData> list = new ArrayList<>();
            PointDataView pdv = pdc.readRandom(uriCounter);
            Number[] x = pdv.getNumberAllLevels(parameter);
            Number[] y = pdv.getNumberAllLevels(heightScale.getParameter());

            // array and checks for wind direction
            Number[] windV = null;
            if (pdc.getParameters().contains(parameter + "[1]")) {
                windV = pdv.getNumberAllLevels(parameter + "[1]");
            }

            if (windV != null) {
                wind = true;
            }

            // look for valid data points
            for (int i = 0; i < x.length; i++) {
                if (y[i].intValue() > CubeUtil.MISSING
                        && x[i].intValue() > CubeUtil.MISSING) {
                    if (y[i].doubleValue() >= yMin
                            && y[i].doubleValue() <= yMax) {
                        if (windV != null) {
                            double u = x[i].doubleValue();
                            double v = windV[i].doubleValue();

                            double speed = Math.hypot(u, v);
                            double dir = Math.toDegrees(Math.atan2(-u, -v));

                            list.add(new XYWindImageData(speed, y[i], speed,
                                    dir));
                        } else {
                            list.add(new XYData(x[i], y[i]));
                        }
                    }
                }
            }
            if (!list.isEmpty()) {
                return list;
            }
        }
        return new ArrayList<>();
    }
}
