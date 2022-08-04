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
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.LineString;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.viz.core.graphing.xy.XYData;

import tec.uom.se.AbstractUnit;

/**
 * Adapter for converting pdos that are compatible with the point data api into
 * data that can be used for Cross Section graphs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 04, 2009           mschenke  Initial creation
 * May 13, 2013  1869     bsteffen  Modified D2D height Graphs to work without
 *                                  dataURI column.
 * Mar 07, 2018  6736     bsteffen  Use InterpUtils instead of
 *                                  MeteolibInterpolation to support wind.
 * Apr 15, 2019 7596      lsingh    Updated units framework to JSR-363. Handled
 *                                  unit conversion.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class PointCSAdapter
        extends AbstractCrossSectionAdapter<PluginDataObject> {

    private static final long serialVersionUID = -4886988215150419992L;

    protected Unit<?> unit = AbstractUnit.ONE;

    @Override
    public String getParameterName() {
        return resourceData.getParameter();
    }

    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    @Override
    public List<float[]> loadData(DataTime currentTime, CrossSectionGraph graph,
            GridGeometry2D geometry) throws VizException {
        HeightScale heightScale = descriptor.getHeightScale();
        String parameter = resourceData.getParameter();
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
        PointDataContainer pdc;
        try {
            pdc = DataCubeContainer.getPointData(records.get(0).getPluginName(),
                    new String[] { parameter,
                            PointDataConstants.DATASET_STATIONID,
                            heightScale.getParameter() },
                    constraints);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
        unit = pdc.getDescription(parameter).getUnitObject();
        List<String> stationIDs = resourceData.getStationIDs();
        Map<String, Float> stationDistances = new HashMap<>();
        stationDistances.put(stationIDs.get(0), 0.0f);
        GeodeticCalculator gc = new GeodeticCalculator();
        LineString line = descriptor.getLine(currentTime);
        for (int i = 1; i < line.getNumPoints(); i++) {
            Coordinate c0 = line.getCoordinateN(i - 1);
            Coordinate c1 = line.getCoordinateN(i);
            gc.setStartingGeographicPoint(c0.x, c0.y);
            gc.setDestinationGeographicPoint(c1.x, c1.y);
            stationDistances.put(stationIDs.get(i),
                    (float) gc.getOrthodromicDistance());
        }
        List<float[]> result = new ArrayList<>(1);

        result.add(makeGrid(parameter, pdc, graph, geometry, stationDistances));
        /*
         * Handle multipart data such as vectors. The first part is encoded as a
         * normal parameter and retrieved above but subsequent parts are encoded
         * by putting array notation in the parameter name. Since the first part
         * is considered element 0 in the array, the array notation starts at
         * element 1.
         */
        for (int c = 1; pdc.getParameters()
                .contains(parameter + "[" + c + "]"); c++) {
            result.add(makeGrid(parameter + "[" + c + "]", pdc, graph, geometry,
                    stationDistances));

        }
        return result;
    }

    private float[] makeGrid(String parameter, PointDataContainer pdc,
            CrossSectionGraph graph, GridGeometry2D geometry,
            Map<String, Float> stationDistances) {
        HeightScale heightScale = descriptor.getHeightScale();
        Unit<?> dataYUnit = pdc.getDescription(heightScale.getParameter())
                .getUnitObject();
        Unit<?> desiredYUnit = heightScale.getParameterUnit();
        UnitConverter yConverter;
        yConverter = UnitConv.getConverterToUnchecked(dataYUnit, desiredYUnit);
        /*
         * Separate out the data out by station. The key in this map is the
         * distance along the x axis of the cross section(from the
         * stationDistances) and the values are basically the raw var-height
         * data for a particular station.
         */
        Map<Float, List<XYData>> columnMap = new TreeMap<>();
        for (int uriCounter = 0; uriCounter < pdc
                .getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            String stationId = null;
            if (pdv.getType(
                    PointDataConstants.DATASET_STATIONID) == PointDataDescription.Type.STRING) {
                stationId = pdv.getString(PointDataConstants.DATASET_STATIONID);
            } else {
                stationId = pdv.getNumber(PointDataConstants.DATASET_STATIONID)
                        .toString();
            }
            Float dist = stationDistances.get(stationId);
            List<XYData> column = columnMap.get(dist);
            if (column == null) {
                column = new ArrayList<>();
                columnMap.put(dist, column);
            }

            Number[] xArr = pdv.getNumberAllLevels(parameter);
            Number[] yArr = pdv.getNumberAllLevels(heightScale.getParameter());
            for (int i = 0; i < xArr.length; i++) {
                Number x = xArr[i];
                Number y = yArr[i];
                if (!Double.isNaN(x.doubleValue())
                        && !Double.isNaN(y.doubleValue())) {
                    y = yConverter.convert(y.doubleValue());
                    column.add(new XYData(x, y));
                }
            }
        }
        float[] xVals = new float[columnMap.size()];
        float[][] columns = new float[xVals.length][];
        boolean lowToHigh = heightScale.getMinVal() < heightScale.getMaxVal();
        int nx = geometry.getGridRange().getSpan(1);
        int ny = geometry.getGridRange().getSpan(0);
        int i = 0;
        /*
         * Interpolate the columns vertically so they all have the same number
         * of points at the locations needed to cover the height of the graph.
         */
        for (Entry<Float, List<XYData>> entry : columnMap.entrySet()) {
            columns[i] = InterpUtils.makeColumn(entry.getValue(), ny, graph,
                    lowToHigh, Float.NaN);
            xVals[i] = entry.getKey();
            i += 1;
        }
        /*
         * Finally interpolate the values between the columns to make a complete
         * grid of data.
         */
        return InterpUtils.makeRows(columns, xVals, nx, graph, true, Float.NaN);
    }

}
