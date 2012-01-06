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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.interp.IInterpolation;
import com.raytheon.uf.viz.core.interp.InterpolationRequest;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.viz.core.graphing.util.MeteolibInterpolation;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PointCSAdapter extends
        AbstractCrossSectionAdapter<PluginDataObject> {

    private static final long serialVersionUID = -4886988215150419992L;

    protected Unit<?> unit = Unit.ONE;

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
    public Unit<?> getUnit() {
        return unit;
    }

    @Override
    public List<float[]> loadData(DataTime currentTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException {
        HeightScale heightScale = descriptor.getHeightScale();
        RequestConstraint uriConstraint = new RequestConstraint();
        uriConstraint.setConstraintType(RequestConstraint.ConstraintType.IN);
        // Perhaps this should just be done using the resource metadatamap
        for (PluginDataObject pdo : records) {
            DataTime pdoTime = pdo.getDataTime().clone();
            pdoTime.setLevelValue(null);
            if (resourceData.getBinOffset() != null) {
                pdoTime = resourceData.getBinOffset()
                        .getNormalizedTime(pdoTime);
            }
            DataTime cTime = currentTime.clone();
            cTime.setLevelValue(null);
            if (pdoTime.equals(cTime)) {
                uriConstraint.addToConstraintValueList(pdo.getDataURI());
            }
        }
        String parameter = resourceData.getParameter();
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("dataURI", uriConstraint);
        PointDataContainer pdc = DataCubeContainer.getPointData(records.get(0)
                .getPluginName(), new String[] { parameter, "stationId",
                heightScale.getParameter() }, constraints);
        unit = pdc.getDescription(parameter).getUnitObject();
        Unit<?> dataYUnit = pdc.getDescription(heightScale.getParameter())
                .getUnitObject();
        Unit<?> desiredYUnit = heightScale.getParameterUnit();
        UnitConverter yConverter = dataYUnit.getConverterTo(desiredYUnit);
        Map<String, Float> stationDistances = new HashMap<String, Float>();
        stationDistances.put(resourceData.getStationIDs().get(0), 0.0f);
        GeodeticCalculator gc = new GeodeticCalculator();
        for (int i = 1; i < descriptor.getLine(currentTime).getNumPoints(); i++) {
            Coordinate c0 = descriptor.getLine(currentTime).getCoordinateN(
                    i - 1);
            Coordinate c1 = descriptor.getLine(currentTime).getCoordinateN(i);
            gc.setStartingGeographicPoint(c0.x, c0.y);
            gc.setDestinationGeographicPoint(c1.x, c1.y);
            stationDistances.put(resourceData.getStationIDs().get(i),
                    (float) gc.getOrthodromicDistance());
        }
        List<Float> xData = new ArrayList<Float>();
        List<Float> yData = new ArrayList<Float>();
        List<List<Float>> zData = new ArrayList<List<Float>>();
        zData.add(new ArrayList<Float>());
        for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            double x = 0.0;
            if (pdv.getType("stationId") == com.raytheon.uf.common.pointdata.PointDataDescription.Type.STRING) {
                x = stationDistances.get(pdv.getString("stationId"));
            } else {
                x = stationDistances.get(pdv.getNumber("stationId").toString());
            }

            Number[] zArr = pdv.getNumberAllLevels(parameter);
            Number[] yArr = pdv.getNumberAllLevels("P");
            for (int i = 0; i < zArr.length; i++) {
                if (yArr[i].intValue() > -9998 && zArr[i].intValue() > -9998) {
                    double y = yConverter.convert(yArr[i].doubleValue());
                    double[] loc = graph.getGridLocation(x, y);
                    xData.add((float) loc[0]);
                    yData.add((float) loc[1]);
                    zData.get(0).add(zArr[i].floatValue());
                }
            }
            for (int c = 1; pdc.getParameters().contains(
                    parameter + "[" + c + "]"); c++) {
                zArr = pdv.getNumberAllLevels(parameter + "[" + c + "]");
                for (int i = 0; i < zArr.length; i++) {
                    if (yArr[i].intValue() > -9998
                            && zArr[i].intValue() > -9998) {
                        double y = yConverter.convert(yArr[i].doubleValue());
                        double[] loc = graph.getGridLocation(x, y);
                        xData.add((float) loc[0]);
                        yData.add((float) loc[1]);
                        zData.get(0).add(zArr[i].floatValue());
                    }
                }
            }
        }
        List<float[]> result = new ArrayList<float[]>();
        IInterpolation interpolation = new MeteolibInterpolation(ScaleType.LIN);
        InterpolationRequest request = new InterpolationRequest();
        request.setXData(InterpUtils.convertToArray(xData));
        request.setYData(InterpUtils.convertToArray(yData));
        request.setGridX((int) geometry.getGridRange2D().getWidth());
        request.setGridY((int) geometry.getGridRange2D().getHeight());
        request.setMinX((float) graph.getExtent().getMinX());
        request.setMaxX((float) graph.getExtent().getMaxX());
        request.setMinY((float) graph.getExtent().getMaxY());
        request.setMaxY((float) graph.getExtent().getMinY());
        for (List<Float> zDataList : zData) {
            request.setZData(InterpUtils.convertToArray(zDataList));
            result.add(interpolation.interpolate(request).getValues());
        }
        return result;
    }

}
