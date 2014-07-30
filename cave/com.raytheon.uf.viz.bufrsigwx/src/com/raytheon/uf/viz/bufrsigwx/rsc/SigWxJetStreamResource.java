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
package com.raytheon.uf.viz.bufrsigwx.rsc;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.bufrsigwx.JetStreamData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.bufrsigwx.common.SigWxCommon;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides a resource that will display jet stream data for a given reference
 * time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/24/2009             jsanchez    Initial creation.
 * Sep 28, 2009 3099       bsteffen    Updated to conform with common SigWxResource
 * Jul 29, 2014 3465       mapeters    Updated deprecated drawString() and 
 *                                     drawStrings() calls.
 * 
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class SigWxJetStreamResource extends SigWxResource {

    private static UnitConverter meterToHft = SI.METER.getConverterTo(SI
            .HECTO(NonSI.FOOT));

    private static UnitConverter mpsToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static final String format = "%3.0f";

    private static final double MAX_VALUE = 1e10;

    private static final String LAT_STR = "latitude";

    private static final String LON_STR = "longitude";

    private static final String NUM_OF_POINTS_STR = "numOfPoints";

    private static final String SPD_STR = "jetSpeed";

    private static final String ALT_STR = "jetAltitude";

    private static final String BOT_HGT_STR = "isotach80Blo";

    private static final String TOP_HGT_STR = "isotach80Abv";

    protected SigWxJetStreamResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public String getName() {
        String layerString = resourceData.getMetadataMap().get("wxLayer")
                .getConstraintValue();
        SigWxLayer layer = SigWxLayer.valueOf(layerString);
        String level = "";
        if (layer == SigWxLayer.SWH) {
            level = "High";
        } else if (layer == SigWxLayer.SWM) {
            level = "Medium";
        }

        return level + " Level SIGWX Jet Streams";
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();

        IWireframeShape shape = target.createWireframeShape(false, descriptor);
        shape.addLineSegment(makeLine(pdv));
        target.drawWireframeShape(shape, color, 1.5f);
        shape.dispose();

        JetStreamData[] jetData = makeJetStreamData(pdv);
        JetStreamData lastData = jetData[0];
        for (JetStreamData data : jetData) {
            paintJetStreamText(target, paintProps, color, data, lastData);
            lastData = data;
        }
        // If there is only one jetData you can't make any error head
        if (jetData.length > 1) {
            paintArrowHead(target, color, jetData[jetData.length - 2],
                    jetData[jetData.length - 1]);
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord, PointDataView pdv)
            throws VizException {

        int LIMIT = 100;
        String result = "";
        double[] point = null;
        try {
            point = descriptor.worldToPixel(new double[] { coord.asLatLon().x,
                    coord.asLatLon().y, });
        } catch (Exception e) {
            return "";
        }
        JetStreamData[] data = makeJetStreamData(pdv);
        boolean found = false;
        double[] prevLinePt = SigWxCommon.lonLatToWorldPixel(descriptor,
                data[0].getLongitude(), data[0].getLatitude());
        for (int i = 1; i < data.length; i++) {
            if (data[i].getJetAltitude() != SigWxCommon.MISSING
                    || data[i].getJetSpeed() != SigWxCommon.MISSING) {
                continue;
            }
            double[] linePt = SigWxCommon.lonLatToWorldPixel(descriptor,
                    data[i].getLongitude(), data[i].getLatitude());
            double distance = distanceFromLine(prevLinePt, linePt, point);
            if (distance < LIMIT) {
                found = true;
                break;
            }
            prevLinePt = linePt;
        }

        if (found) {
            int bestIndex = -1;
            double bestDistance = MAX_VALUE;
            for (int j = 0; j < data.length; j++) {
                if (data[j].getJetAltitude() == SigWxCommon.MISSING
                        || data[j].getJetSpeed() == SigWxCommon.MISSING) {
                    continue;
                }
                double dist = distance(point, SigWxCommon.lonLatToWorldPixel(
                        descriptor, data[j].getLongitude(), data[j]
                                .getLatitude()));
                if (dist < bestDistance) {
                    bestIndex = j;
                    bestDistance = dist;
                }
            }
            if (bestIndex >= 0) {
                JetStreamData jsd = data[bestIndex];
                double speed = jsd.getJetSpeed();
                double fltLvl = jsd.getJetAltitude();
                double baseHgt = jsd.getBaseHeight();
                double topHgt = jsd.getTopHeight();

                if (speed != SigWxCommon.MISSING) {
                    result += SigWxCommon.format(mpsToKnots.convert(speed),
                            format)
                            + "kts ";
                }

                if (fltLvl != SigWxCommon.MISSING) {
                    result += "FL"
                            + SigWxCommon.format(meterToHft.convert(fltLvl),
                                    format);
                }

                if (baseHgt != SigWxCommon.MISSING
                        && topHgt != SigWxCommon.MISSING) {
                    result += " DEPTH:"
                            + SigWxCommon.format(meterToHft.convert(baseHgt),
                                    format)
                            + "/"
                            + SigWxCommon.format(meterToHft.convert(topHgt),
                                    format);
                }
                return result;
            }
        }

        return "";

    }

    private JetStreamData[] makeJetStreamData(PointDataView pdv) {
        Number[] hdf5Speeds = pdv.getNumberAllLevels(SPD_STR);
        Number[] hdf5Altitudes = pdv.getNumberAllLevels(ALT_STR);
        Number[] hdf5BotHgt = pdv.getNumberAllLevels(BOT_HGT_STR);
        Number[] hdf5TopHgt = pdv.getNumberAllLevels(TOP_HGT_STR);
        Number[] hdf5Lats = pdv.getNumberAllLevels(LAT_STR);
        Number[] hdf5Lons = pdv.getNumberAllLevels(LON_STR);
        int numOfPoints = pdv.getInt(NUM_OF_POINTS_STR);

        JetStreamData[] data = new JetStreamData[numOfPoints];

        for (int i = 0; i < numOfPoints; i++) {
            data[i] = new JetStreamData(hdf5Lons[i].floatValue(), hdf5Lats[i]
                    .floatValue(), hdf5Speeds[i].floatValue(), hdf5Altitudes[i]
                    .floatValue(), hdf5BotHgt[i].floatValue(), hdf5TopHgt[i]
                    .floatValue()); // Should
            // the
            // hdf5BotHgt
            // and
            // hdf5TopHgt
            // be
            // used?

        }
        return data;
    }

    private Coordinate[] makeLine(PointDataView pdv) {
        Number[] hdf5Lats = pdv.getNumberAllLevels(LAT_STR);
        Number[] hdf5Lons = pdv.getNumberAllLevels(LON_STR);
        int numOfPoints = pdv.getInt(NUM_OF_POINTS_STR);

        Coordinate[] js = new Coordinate[numOfPoints];

        for (int i = 0; i < numOfPoints; i++) {
            js[i] = new Coordinate(hdf5Lons[i].floatValue(), hdf5Lats[i]
                    .floatValue());
        }

        return js;
    }

    /**
     * Calculates the direction of jet stream and passes the drawing to the
     * paintArrowhead method
     * 
     * @param target
     * @param color
     *            the color of the arrow
     * @param data1
     *            the second to last coordinates of the stream
     * @param data2
     *            the last coordinates of the stream
     * @throws VizException
     */
    private void paintArrowHead(IGraphicsTarget target, RGB color,
            JetStreamData data1, JetStreamData data2) throws VizException {
        double[] locPixA = SigWxCommon.lonLatToWorldPixel(descriptor, data1
                .getLongitude(), data1.getLatitude());
        double[] locPixB = SigWxCommon.lonLatToWorldPixel(descriptor, data2
                .getLongitude(), data2.getLatitude());
        double dir = Math.toDegrees(Math.atan2(locPixA[1] - locPixB[1],
                locPixA[0] - locPixB[0]));
        SigWxCommon.paintArrowHead(target, locPixB, 100.0, dir + 180, color);
    }

    /**
     * Draws the Jet Stream Data (flight level, jet speed, etc.) on CAVE
     * 
     * @param target
     * @param color
     *            the color of the strings
     * @param data
     *            the Jet Stream Data
     * @throws VizException
     */
    private void paintJetStreamText(IGraphicsTarget target,
            PaintProperties paintProps, RGB color, JetStreamData data,
            JetStreamData lastData) throws VizException {
        double lat = data.getLatitude();
        double lon = data.getLongitude();
        if (lat == SigWxCommon.MISSING || lon == SigWxCommon.MISSING) {
            return;
        }
        double[] locationPixel = SigWxCommon.lonLatToWorldPixel(descriptor,
                lon, lat);
        HorizontalAlignment halignLevel = HorizontalAlignment.RIGHT;
        VerticalAlignment valignLevel = VerticalAlignment.BOTTOM;
        HorizontalAlignment halignSpeed = HorizontalAlignment.LEFT;
        VerticalAlignment valignSpeed = VerticalAlignment.TOP;
        double[] lastLocationPixel = SigWxCommon.lonLatToWorldPixel(descriptor,
                lastData.getLongitude(), lastData.getLatitude());
        double dx = locationPixel[0] - lastLocationPixel[0];
        double dy = locationPixel[1] - lastLocationPixel[1];
        // target.drawString(font, String.format("%.0f", dx) + ", "
        // + String.format("%.0f", dy),
        // locationPixel[0],
        // locationPixel[1], 0.0, TextStyle.BOXED, color,
        // HorizontalAlignment.CENTER, VerticalAlignment.MIDDLE, null);
        if (dx > 0 == dy > 0) {
            valignLevel = VerticalAlignment.TOP;
            valignSpeed = VerticalAlignment.BOTTOM;
            halignLevel = HorizontalAlignment.RIGHT;
            halignSpeed = HorizontalAlignment.LEFT;
        } else {
            valignLevel = VerticalAlignment.TOP;
            valignSpeed = VerticalAlignment.BOTTOM;
            halignLevel = HorizontalAlignment.LEFT;
            halignSpeed = HorizontalAlignment.RIGHT;
        }

        double flightLevel = data.getJetAltitude();
        double speed = data.getJetSpeed();
        double baseHgt = data.getBaseHeight();
        double topHgt = data.getTopHeight();
        if (flightLevel != SigWxCommon.MISSING) {
            flightLevel = meterToHft.convert(flightLevel);
            String flightLevelStr = SigWxCommon.format(flightLevel, format);
            if (baseHgt != SigWxCommon.MISSING && topHgt != SigWxCommon.MISSING) {
                baseHgt = meterToHft.convert(baseHgt);
                topHgt = meterToHft.convert(topHgt);
                String depthStr = SigWxCommon.format(baseHgt, format) + "/"
                        + SigWxCommon.format(topHgt, format);

                double tmpX = locationPixel[0];
                double width = target.getStringBounds(font, depthStr)
                        .getWidth()
                        * getScale(paintProps)[1];
                if (halignLevel == HorizontalAlignment.LEFT) {
                    tmpX += width / 2;
                    halignLevel = HorizontalAlignment.CENTER;
                } else if (halignLevel == HorizontalAlignment.LEFT) {
                    tmpX -= width / 2;
                    halignLevel = HorizontalAlignment.CENTER;
                }
                DrawableString string = new DrawableString(new String[] {
                        flightLevelStr, depthStr }, color);
                string.font = font;
                string.setCoordinates(tmpX, locationPixel[1]);
                string.horizontalAlignment = halignLevel;
                string.verticallAlignment = valignLevel;
                target.drawStrings(string);
            } else {
                DrawableString string = new DrawableString(flightLevelStr,
                        color);
                string.font = font;
                string.setCoordinates(locationPixel[0], locationPixel[1]);
                string.horizontalAlignment = halignLevel;
                string.verticallAlignment = valignLevel;
                target.drawStrings(string);
            }
        }
        if (speed != SigWxCommon.MISSING) {
            speed = mpsToKnots.convert(speed);
            String speedStr = SigWxCommon.format(speed, format);
            DrawableString string = new DrawableString(speedStr, color);
            string.font = font;
            string.setCoordinates(locationPixel[0], locationPixel[1]);
            string.horizontalAlignment = halignSpeed;
            string.verticallAlignment = valignSpeed;
            target.drawStrings(string);
        }
    }

    /**
     * Calculates the dot product of a and b
     * 
     * @param a
     *            {x1, y1}
     * @param b
     *            {x2, y2}
     * @return the dot product of a and b
     */
    private double dotProduct(double[] a, double[] b) {
        return (a[0] * b[0]) + (a[1] * b[1]);
    }

    /**
     * Calculates the distance between a and b
     * 
     * @param a
     *            {x1, y1}
     * @param b
     *            {x2, y2}
     * @return the distance between a and b
     */
    private double distance(double[] a, double[] b) {
        double x = a[0] - b[0];
        double y = a[1] - b[1];
        return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
    }

    /*
     * Calculates the distance of the cursor away from the line between a and b.
     * Note: This code was ported from SigWxDepict.C
     * 
     * @param a the previous line coordinates
     * 
     * @param b the current line coordinates
     * 
     * @param p the coordinates of the cursor
     * 
     * @return the distance of the cursor away from the line
     */
    private double distanceFromLine(double[] a, double[] b, double[] p) {

        double dy = b[1] - a[1];
        double dx = b[0] - a[0];
        double[] fa = new double[] { 0, 0 };
        double[] fb = new double[] { dx, dy };
        double[] fp = new double[] { p[0] - a[0], p[1] - a[1] };
        double d = distance(fa, fb);
        double m = (dotProduct(fb, fp) / (d * d));
        double[] projP = new double[] { fb[0] * m, fb[1] * m };
        double dA = distance(fa, projP);
        double dB = distance(fb, projP);

        if (dy == 0) {
            if (((p[1] < a[1] && p[1] < b[1]) || (p[1] > a[1] && p[1] > b[1]))
                    && Math.abs(p[1] - a[1]) > 4 && Math.abs(p[1] - b[1]) > 4) {
                return MAX_VALUE;
            } else {
                return Math.abs(p[1] - a[1]);
            }
        } else if (dx == 0) {
            if (((p[0] < a[0] && p[0] < b[0]) || (p[0] > a[0] && p[0] > b[0]))
                    && Math.abs(p[0] - a[0]) > 4 && Math.abs(p[0] - b[0]) > 4) {
                return MAX_VALUE;
            } else {
                return Math.abs(p[0] - a[0]);
            }
        }

        if (dA - 4 > d && dB - 4 > d) {
            return MAX_VALUE;
        }

        return distance(projP, fp);
    }

    protected String[] getParameters() {
        return new String[] { LAT_STR, LON_STR, NUM_OF_POINTS_STR, SPD_STR,
                ALT_STR, TOP_HGT_STR, BOT_HGT_STR };
    }

}