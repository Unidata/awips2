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

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

/**
 * Adapter for converting pdos that are compatible with the point data api into
 * data that can be used for Var Height graphs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 07, 2010            bsteffen    Initial creation
 * May 13, 2013 1869       bsteffen    Modified D2D height Graphs to work
 *                                     without dataURI column.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointDataVarHeightAdapter extends
        AbstractVarHeightAdapter<PluginDataObject> {

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
        double min = Math.min(heightScale.getMinVal(), heightScale.getMaxVal());
        double max = Math.max(heightScale.getMinVal(), heightScale.getMaxVal());
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>(
                resourceData.getMetadataMap());
        if (resourceData.getBinOffset() != null) {
            TimeRange range = resourceData.getBinOffset().getTimeRange(
                    currentTime);
            RequestConstraint timeConstraint = new RequestConstraint();
            timeConstraint.setConstraintType(ConstraintType.BETWEEN);
            String start = TimeUtil.formatToSqlTimestamp(range.getStart());
            String end = TimeUtil.formatToSqlTimestamp(range.getEnd());
            timeConstraint.setBetweenValueList(new String[] { start, end });
            constraints.put("dataTime.refTime", timeConstraint);
        } else if (currentTime.getUtilityFlags().contains(FLAG.FCST_USED)) {
            constraints.put("dataTime",
                    new RequestConstraint(currentTime.toString()));
        } else {
            constraints.put(
                    "dataTime.refTime",
                    new RequestConstraint(TimeUtil
                            .formatToSqlTimestamp(currentTime.getRefTime())));
        }
        String parameter = resourceData.getParameter();
        PointDataContainer pdc = DataCubeContainer.getPointData(records
                .iterator().next().getPluginName(), new String[] { parameter,
                heightScale.getParameter() }, constraints);
        xUnit = pdc.getDescription(parameter).getUnitObject();
        yUnit = pdc.getDescription(heightScale.getParameter()).getUnitObject();
        for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
            List<XYData> list = new ArrayList<XYData>();
            PointDataView pdv = pdc.readRandom(uriCounter);
            Number[] x = pdv.getNumberAllLevels(parameter);
            Number[] y = pdv.getNumberAllLevels(heightScale.getParameter());

            // array and checks for wind direction
            Number[] windDir = null;
            boolean hasWind1Unit = false;
            if (pdc.getParameters().contains(parameter + "[1]")) {
                hasWind1Unit = true;
                windDir = pdv.getNumberAllLevels(parameter + "[1]");
            }

            // look for valid data points
            for (int i = 0; i < x.length; i++) {
                if (y[i].intValue() > -9998 && x[i].intValue() > -9998) {
                    if (y[i].doubleValue() >= min && y[i].doubleValue() <= max) {
                        if (hasWind1Unit) {
                            double windSpeed = x[i].doubleValue();
                            double windDirection = windDir[i].doubleValue();
                            list.add(new XYWindImageData(x[i], y[i], windSpeed,
                                    windDirection));
                        } else {
                            list.add(new XYData(x[i], y[i]));
                        }
                    }
                } else {
                }
            }
            if (!list.isEmpty()) {
                return list;
            }
        }
        return new ArrayList<XYData>();
    }
}
