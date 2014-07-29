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
package com.raytheon.uf.viz.ncwf.rsc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.bufrncwf.BUFRncwf;
import com.raytheon.uf.common.dataplugin.bufrncwf.NCWFFeature;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A resource responsible for drawing the movement component of ncwf data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2009            bsteffen     Initial creation
 * Jul 24, 2014 3429       mapeters     Updated deprecated drawLine() calls.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class NcwfMovementResource extends
        AbstractNcwfResource<NcwfMovementResourceData> {

    private static final String[] parameters = { "centroid_lat",
            "centroid_lon", "or_centroid_lat", "or_centroid_lon", "storm_dir",
            "storm_speed", "storm_top" };

    private Map<DataTime, Collection<BUFRncwf>> recordsToParse = new HashMap<DataTime, Collection<BUFRncwf>>();

    private Map<DataTime, Collection<BUFRncwf>> recordsMap = new HashMap<DataTime, Collection<BUFRncwf>>();

    protected NcwfMovementResource(NcwfMovementResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.resourceName = "NCWF Storm Tops and Movement ";
    }

    @Override
    protected void disposeInternal() {

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        DataTime displayedDataTime = paintProps.getDataTime();

        updateRecords(displayedDataTime);

        Collection<BUFRncwf> curRecords = recordsMap.get(displayedDataTime);

        if (curRecords == null) {
            displayedDataTime = null;
            return;
        }

        for (BUFRncwf record : curRecords) {
            RGB color = getCapability(ColorableCapability.class).getColor();
            // This should adjust the direction so 0 is north
            Double dir = (record.getStormDir() + 180) % 360;
            // TODO use units to convert to knots rather than hardcode
            Double spd = Math.floor(record.getStormSpeed() * 1.9434);
            Double top = record.getStormTop();
            Coordinate centerll = record.getForecast().getCentroidLocation();
            double[] centerPixel = descriptor.worldToPixel(new double[] {
                    centerll.x, centerll.y, centerll.z });
            Coordinate or_centerll = record.getDetection()
                    .getCentroidLocation();
            double[] or_centerPixel = descriptor.worldToPixel(new double[] {
                    or_centerll.x, or_centerll.y, or_centerll.z });
            // 90 are added to dir so that 0 is now on the right
            paintArrowHead(target, centerPixel, spd, dir + 90, color);

            // Get the string objects.
            DrawableString topStr = new DrawableString(String.format("%.0f",
                    top), color);
            topStr.setCoordinates(or_centerPixel[0], or_centerPixel[1],
                    or_centerPixel[2]);
            DrawableString spdStr = new DrawableString(String.format("%.0f",
                    spd), color);
            spdStr.setCoordinates(centerPixel[0], centerPixel[1],
                    centerPixel[2]);

            // This works but it is supposed to avoid putting text over arrows
            spdStr.horizontalAlignment = HorizontalAlignment.CENTER;
            spdStr.verticallAlignment = VerticalAlignment.MIDDLE;
            topStr.horizontalAlignment = HorizontalAlignment.CENTER;
            topStr.verticallAlignment = VerticalAlignment.MIDDLE;
            if (dir <= 90 && dir >= 0) {
                topStr.verticallAlignment = VerticalAlignment.BOTTOM;
                topStr.horizontalAlignment = HorizontalAlignment.LEFT;
                spdStr.verticallAlignment = VerticalAlignment.TOP;
                spdStr.horizontalAlignment = HorizontalAlignment.RIGHT;
            } else if (dir >= 270 && dir < 360) {
                topStr.verticallAlignment = VerticalAlignment.BOTTOM;
                topStr.horizontalAlignment = HorizontalAlignment.RIGHT;
                spdStr.verticallAlignment = VerticalAlignment.TOP;
                spdStr.horizontalAlignment = HorizontalAlignment.LEFT;
            } else if (dir > 90 && dir < 180) {
                topStr.verticallAlignment = VerticalAlignment.TOP;
                topStr.horizontalAlignment = HorizontalAlignment.LEFT;
                spdStr.verticallAlignment = VerticalAlignment.BOTTOM;
                spdStr.horizontalAlignment = HorizontalAlignment.RIGHT;
            } else {
                topStr.verticallAlignment = VerticalAlignment.TOP;
                topStr.horizontalAlignment = HorizontalAlignment.RIGHT;
                spdStr.verticallAlignment = VerticalAlignment.BOTTOM;
                spdStr.horizontalAlignment = HorizontalAlignment.LEFT;
            }
            // Draw the tops string
            target.drawStrings(topStr);
            // Draw the body of the arrow
            DrawableLine line = new DrawableLine();
            line.setCoordinates(centerPixel[0], centerPixel[1], centerPixel[2]);
            line.addPoint(or_centerPixel[0], or_centerPixel[1], or_centerPixel[2]);
            line.basics.color = color;
            line.width = 1.5f;
            target.drawLine(line);

            // Draw the wind speed string
            target.drawStrings(spdStr);

        }
    }

    private void paintArrowHead(IGraphicsTarget target, double[] center,
            Double length, Double dir, RGB color) throws VizException {
        double[] pointPixel = target.getPointOnCircle(center[0], center[1],
                center[2], length, dir + 210);

        DrawableLine line = new DrawableLine();
        line = new DrawableLine();
        line.setCoordinates(pointPixel[0], pointPixel[1], pointPixel[2]);
        line.addPoint(center[0], center[1], center[2]);
        pointPixel = target.getPointOnCircle(center[0], center[1], center[2],
                length, dir + 150);
        line.addPoint(pointPixel[0], pointPixel[1], pointPixel[2]);
        line.basics.color = color;
        line.width = 1.5f;
        target.drawLine(line);
    }

    private void updateRecords(DataTime displayedDataTime) throws VizException {
        Collection<BUFRncwf> records = this.recordsToParse
                .get(displayedDataTime);
        // Return if there is nothing to do
        if (records == null || records.size() == 0) {
            return;
        }
        if (!recordsMap.containsKey(displayedDataTime)) {
            recordsMap.put(displayedDataTime, new ArrayList<BUFRncwf>());
        }
        RequestConstraint constraint = new RequestConstraint();

        constraint.setConstraintType(ConstraintType.IN);

        for (BUFRncwf r : records) {

            constraint.addToConstraintValueList(r.getDataURI());
        }
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("dataURI", constraint);
        // Request the point data
        PointDataContainer pdc = PointDataRequest.requestPointDataAllLevels(
                displayedDataTime,
                resourceData.getMetadataMap().get("pluginName")
                        .getConstraintValue(), parameters, null, constraints);

        for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            // Get all the data into a simple type
            float centroid_lat = pdv.getFloat(parameters[0]);
            float centroid_lon = pdv.getFloat(parameters[1]);
            float or_centroid_lat = pdv.getFloat(parameters[2]);
            float or_centroid_lon = pdv.getFloat(parameters[3]);
            float storm_dir = pdv.getFloat(parameters[4]);
            float storm_speed = pdv.getFloat(parameters[5]);
            float storm_top = pdv.getFloat(parameters[6]);
            // Append the data to the record
            // This builds the full record except the boundaries because
            // those are currently drawn by the polygon resources
            BUFRncwf r = new BUFRncwf();
            r.setForecast(new NCWFFeature((double) centroid_lat,
                    (double) centroid_lon));
            r.setDetection(new NCWFFeature((double) or_centroid_lat,
                    (double) or_centroid_lon));
            r.setStormDir((double) storm_dir);
            r.setStormSpeed((double) storm_speed);
            r.setStormTop((double) storm_top);
            // Add the recordunder the correct datatime
            // now the record is ready to go
            recordsMap.get(displayedDataTime).add(r);
        }
        this.recordsToParse.get(displayedDataTime).clear();
    }

    protected void addRecord(BUFRncwf obj) {
        if (!recordsToParse.containsKey(obj.getDataTime())) {
            recordsToParse.put(obj.getDataTime(), new ArrayList<BUFRncwf>());
            this.dataTimes.add(obj.getDataTime());
        }
        this.recordsToParse.get(obj.getDataTime()).add(obj);
    }

}
