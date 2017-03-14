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
import java.util.List;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.interp.IInterpolation;
import com.raytheon.uf.viz.core.interp.InterpolationRequest;
import com.raytheon.uf.viz.d2d.xy.tools.DmdTools;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.viz.core.graphing.util.MeteolibInterpolation;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2009            bsteffen     Initial creation
 * Oct 13, 2015 4897       bkowal       Relocated {@link DmdTools} to a plugin
 *                                      that actually uses it.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DmdCSAdapter extends AbstractCrossSectionAdapter<RadarRecord> {

    private static final long serialVersionUID = -2331111151642125898L;

    private static final double MIN_DISTANCE = 0.5;

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
        LineString line = descriptor.getLine(currentTime);
        HeightScale heightScale = descriptor.getHeightScale();
        // Coordinate target = this.resourceData.getPoint();
        double minY = Math
                .min(heightScale.getMinVal(), heightScale.getMaxVal());
        double maxY = Math
                .max(heightScale.getMinVal(), heightScale.getMaxVal());
        String parameter = resourceData.getParameter();

        List<Float> xData = new ArrayList<Float>();
        List<Float> yData = new ArrayList<Float>();
        List<Float> zData = new ArrayList<Float>();

        for (int i = 0; i < this.records.size(); i++) {
            RadarRecord record = this.records.get(i);
            DataTime pdoTime = record.getDataTime().clone();
            pdoTime.setLevelValue(null);
            if (resourceData.getBinOffset() != null) {
                pdoTime = resourceData.getBinOffset()
                        .getNormalizedTime(pdoTime);
            }
            DataTime cTime = currentTime.clone();
            cTime.setLevelValue(null);
            if (!pdoTime.equals(cTime)) {
                continue;
            }
            DmdTools.retrieveFromDataStore(record);
            for (String featureId : RadarRecordUtil.getDMDFeatureIDs(record)) {
                Coordinate c = RadarRecordUtil.getDMDLonLatFromFeatureID(
                        record, featureId);
                double x = findDistance(line, c);
                if (x < -9998) {
                    continue;
                }
                for (String level : DmdTools.levels3d) {
                    float z = DmdTools.getParameter(record, featureId, level,
                            parameter).floatValue();
                    double y = DmdTools.getParameter(record, featureId, level,
                            heightScale.getParameter()).doubleValue();
                    if (y > -9998 && z > -9998 && y >= minY && y <= maxY) {
                        Unit<?> yDataUnit = DmdTools.getUnit(record, featureId,
                                level, heightScale.getParameter());
                        y = yDataUnit.getConverterTo(
                                heightScale.getParameterUnit()).convert(y);
                        if (unit == Unit.ONE) {
                            unit = DmdTools.getUnit(record, featureId, level,
                                    parameter);

                        }
                        double[] loc = graph.getGridLocation(x, y);
                        xData.add((float) loc[0]);
                        yData.add((float) loc[1]);
                        zData.add(z);
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
        request.setZData(InterpUtils.convertToArray(zData));
        result.add(interpolation.interpolate(request).getValues());
        return result;
    }

    private double findDistance(LineString line, Coordinate c) {
        double bestDistance = Double.MAX_VALUE;
        double totalDistance = 0.0;
        double bestTotalDistance = 0.0;
        GeodeticCalculator gc = new GeodeticCalculator();
        for (int i = 1; i < line.getNumPoints(); i++) {
            Coordinate c0 = line.getCoordinateN(i - 1);
            Coordinate c1 = line.getCoordinateN(i);
            LineSegment segment = new LineSegment(c0, c1);
            gc.setStartingGeographicPoint(c0.x, c0.y);
            double distance = segment.distance(c);
            if (distance < bestDistance) {
                bestDistance = distance;
                gc.setDestinationGeographicPoint(c.x, c.y);
                bestTotalDistance = totalDistance + gc.getOrthodromicDistance();
            }
            gc.setDestinationGeographicPoint(c1.x, c1.y);
            totalDistance += gc.getOrthodromicDistance();

        }
        if (bestDistance > MIN_DISTANCE) {
            return -999999;
        }
        return bestTotalDistance;
    }
}
