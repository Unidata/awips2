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

import javax.measure.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.LineSegment;
import org.locationtech.jts.geom.LineString;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.xy.tools.DmdTools;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.uf.viz.xy.interp.IInterpolation;
import com.raytheon.uf.viz.xy.interp.InterpolationRequest;
import com.raytheon.uf.viz.xy.interp.MeteolibInterpolation;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.scales.HeightScale.ScaleType;

import tech.units.indriya.AbstractUnit;

/**
 * Cross section adapter for radar Digital Mesocyclone Display (DMD) data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2009            bsteffen     Initial creation
 * Oct 13, 2015 4897       bkowal       Relocated {@link DmdTools} to a plugin
 *                                      that actually uses it.
 * Apr 15, 2019  7596      lsingh       Updated units framework to JSR-363.
 * Oct 29, 2022 8959       mapeters     Update how data time levels are set
 * Jun 20, 2024 2037565    mapeters     Remove unused getParameterName()
 * Aug 14, 2024 2037631    mapeters     Wrap float data in new class
 *
 * </pre>
 *
 * @author bsteffen
 */
public class DmdCSAdapter extends AbstractCrossSectionAdapter<RadarRecord> {

    private static final long serialVersionUID = -2331111151642125898L;

    private static final double MIN_DISTANCE = 0.5;

    protected Unit<?> unit = AbstractUnit.ONE;

    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    @Override
    public CrossSectionFrameData loadData(DataTime currentTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException {
        LineString line = descriptor.getLine(currentTime);
        HeightScale heightScale = descriptor.getHeightScale();
        double minY = Math.min(heightScale.getMinVal(),
                heightScale.getMaxVal());
        double maxY = Math.max(heightScale.getMinVal(),
                heightScale.getMaxVal());
        String parameter = resourceData.getParameter();

        List<Float> xData = new ArrayList<>();
        List<Float> yData = new ArrayList<>();
        List<Float> zData = new ArrayList<>();

        for (int i = 0; i < this.records.size(); i++) {
            RadarRecord record = this.records.get(i);
            DataTime pdoTime = record.getDataTime().clone();
            pdoTime.clearLevel();
            if (resourceData.getBinOffset() != null) {
                pdoTime = resourceData.getBinOffset()
                        .getNormalizedTime(pdoTime);
            }
            DataTime cTime = currentTime.clone();
            cTime.clearLevel();
            if (!pdoTime.equals(cTime)) {
                continue;
            }
            DmdTools.retrieveFromDataStore(record);
            for (String featureId : RadarRecordUtil.getDMDFeatureIDs(record)) {
                Coordinate c = RadarRecordUtil.getDMDLonLatFromFeatureID(record,
                        featureId);
                double x = findDistance(line, c);
                if (x < -9998) {
                    continue;
                }
                for (String level : DmdTools.levels3d) {
                    float z = DmdTools
                            .getParameter(record, featureId, level, parameter)
                            .floatValue();
                    double y = DmdTools.getParameter(record, featureId, level,
                            heightScale.getParameter()).doubleValue();
                    if (y > -9998 && z > -9998 && y >= minY && y <= maxY) {
                        Unit<?> yDataUnit = DmdTools.getUnit(record, featureId,
                                level, heightScale.getParameter());
                        y = UnitConv
                                .getConverterToUnchecked(yDataUnit,
                                        heightScale.getParameterUnit())
                                .convert(y);
                        if (unit == AbstractUnit.ONE) {
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
        List<float[]> result = new ArrayList<>();
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
        return new CrossSectionFrameData(result, null);
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
            return -999_999;
        }
        return bestTotalDistance;
    }
}
