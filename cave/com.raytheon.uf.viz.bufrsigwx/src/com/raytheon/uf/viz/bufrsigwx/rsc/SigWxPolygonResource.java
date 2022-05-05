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

import java.util.ArrayList;
import java.util.List;

import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxLayer;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.bufrsigwx.util.Declutter;
import com.raytheon.uf.viz.bufrsigwx.util.Declutter.TextBoxData;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.display.SymbolLoader;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 *
 * A base resource for polygon based sigWx data(CAT anc Clouds). This resource
 * does most of the work, but requires subclasses for some details such as
 * drawing the textBoxes and formating inspection data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2009 3099       bsteffen    Initial creation
 * Sep 28, 2009 3099       bsteffen    Updated to conform with common SigWxResource
 * Sep 12, 2016 5886       tgurney     Use isUpdateNeeded for performance
 * Sep 15, 2016 5886       tgurney     Keep cached polygons when panning/zooming
 * Sep 19, 2016 5886       tgurney     Use drawRect/drawLine instead of
 *                                     wireframe shape for text boxes
 * Mar 19, 2018 7243       njensen     Use Geometry instead of Polygon to handle 2-point geometries
 *
 *
 * </pre>
 *
 * @author bsteffen
 */
public abstract class SigWxPolygonResource extends SigWxResource {

    protected static final String P_LATITUDE = "latitude";

    protected static final String P_LONGITUDE = "longitude";

    protected static final String P_NUM_OF_POINTS = "numOfPoints";

    protected static final String[] turbTypes = { "NO", "LIGHT", "MODERATE",
            "SEVERE", "NO", "LIGHT", "MODERATE", "SEVERE", "NO", "LIGHT",
            "MODERATE", "SEVERE", "EXTREME", "EXTREME", "EXTREME",
            "LIGHT ISOL MOD", "LIGHT OCNL MOD", "LIGHT FRQT MOD",
            "MOD ISOL SEV", "MOD OCNL SEV", "MOD FRQ SEV", "SEV ISOL EXTREME",
            "SEV OCNL EXTREME", "SEV FRQ EXTREME" };

    protected static final String[] turbTypesSym = { "\u0000", "\u007a",
            "\u007b", "\u007c", "\u0000", "\u007a", "\u007b", "\u007c",
            "\u0000", "\u007a", "\u007b", "\u007c", "\u007c\u00b6",
            "\u007c\u00b6", "\u007c\u00b6", "\u007a\u007b", "\u007a\u007b",
            "\u007a\u007b", "\u007b\u007c", "\u007b\u007c", "\u007b\u007c",
            "\u007c\u007c\b6", "\u007c\u007c\u00b6", "\u007c\u007c\u00b6" };

    private static UnitConverter meterToHft = SI.METRE
            .getConverterTo(MetricPrefix.HECTO(USCustomary.FOOT));

    protected IExtent lastExtent;

    protected IWireframeShape lastPolygons;

    protected List<Declutter.TextBoxData> lastTextBoxes = new ArrayList<>();

    protected Declutter declutter;

    protected SymbolLoader symbolLoader;

    protected SigWxPolygonResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        disposeShapes();
    }

    private void disposeShapes() {
        if (lastPolygons != null) {
            lastPolygons.dispose();
            lastPolygons = null;
        }
        lastTextBoxes.clear();
    }

    @Override
    public String inspect(ReferencedCoordinate coord, PointDataView pdv)
            throws VizException {

        Geometry llPolygon = getPolygon(pdv);
        // If you don't convert it to pixels then everything breaks when
        // you have polygons over 180,-180 latitude
        Geometry pixelPoly = worldToPixel(llPolygon);
        Point point;
        try {
            point = pixelPoly.getFactory()
                    .createPoint(coord.asPixel(descriptor.getGridGeometry()));
        } catch (Exception e) {
            throw new VizException("Error inspecting SigWx Data", e);
        }
        if (pixelPoly.contains(point)) {
            return getInspectString(pdv);
        }

        return "";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        this.symbolLoader = new SymbolLoader();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataContainer pdc)
                    throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        IExtent curExtent = paintProps.getView().getExtent();
        if (curExtent == null) {
            return;
        }
        if (!curExtent.equals(lastExtent) || isUpdateNeeded()) {
            if (lastPolygons != null && isUpdateNeeded()) {
                lastPolygons.dispose();
            }
            lastTextBoxes.clear();
            lastExtent = curExtent;
            declutter = new Declutter(paintProps.getClippingPane()
                    .intersection(paintProps.getView().getExtent()));
            if (isUpdateNeeded()) {
                lastPolygons = target.createWireframeShape(false, descriptor);
            }
            for (int i = 0; i < pdc.getCurrentSz(); i++) {
                PointDataView pdv = pdc.readRandom(i);
                double[] scale = getScale(paintProps);
                Geometry polygon = getPolygon(pdv);
                if (isUpdateNeeded()) {
                    lastPolygons.addLineSegment(polygon.getCoordinates());
                }
                double[] dimensions = getTextBoxDimensions(target, pdv);
                dimensions[0] = dimensions[0] * scale[0];
                dimensions[1] = dimensions[1] * scale[1];
                Declutter.TextBoxData textBox = declutter
                        .infoBoxForPolygon2(worldToPixel(polygon), dimensions);
                lastTextBoxes.add(textBox);
            }
        }
        target.drawWireframeShape(lastPolygons, color, 1.5f, getLineStyle());
        double[] scale = getScale(paintProps);
        for (int i = 0; i < lastTextBoxes.size(); i++) {
            TextBoxData textBox = lastTextBoxes.get(i);
            if (textBox != null) {
                textBox.line.basics.color = color;
                target.drawRect(textBox.box, color, 1.0f, 1.0);
                target.drawLine(textBox.line);
                drawText(target, paintProps, textBox.textLoc, scale, color,
                        pdc.readRandom(i));
            }
        }
    }

    @Override
    public String getName() {
        String layerString = resourceData.getMetadataMap().get("wxLayer")
                .getConstraintValue();
        SigWxLayer layer = SigWxLayer.valueOf(layerString);
        String typeString = resourceData.getMetadataMap().get("wxType")
                .getConstraintValue();
        typeString = typeString.substring(0, 1)
                + typeString.substring(1).toLowerCase();
        String level = "";
        if (layer == SigWxLayer.SWH) {
            level = "High";
        } else if (layer == SigWxLayer.SWM) {
            level = "Medium";
        }
        return level + " Level SIGWX " + typeString;
    }

    protected abstract LineStyle getLineStyle();

    protected abstract String getInspectString(PointDataView pdv);

    protected abstract int getNumPoints(PointDataView pdv);

    protected abstract double[] getTextBoxDimensions(IGraphicsTarget target,
            PointDataView pdv);

    protected abstract void drawText(IGraphicsTarget target,
            PaintProperties paintProps, double[] textLoc, double[] scale,
            RGB color, PointDataView pdv) throws VizException;

    protected Geometry getPolygon(PointDataView pdv) {
        Number[] lats = pdv.getNumberAllLevels(P_LATITUDE);
        Number[] lons = pdv.getNumberAllLevels(P_LONGITUDE);

        int length = getNumPoints(pdv);
        Coordinate[] coords;
        if (lats[length] == lats[0] && lons[length] == lons[0]) {
            coords = new Coordinate[length];
            for (int i = 0; i < length; i++) {
                coords[i] = new Coordinate(lons[i].floatValue(),
                        lats[i].floatValue());
            }
        } else {
            coords = new Coordinate[length + 1];
            for (int i = 0; i < length; i++) {
                coords[i] = new Coordinate(lons[i].floatValue(),
                        lats[i].floatValue());
            }
            coords[length] = new Coordinate(lons[0].floatValue(),
                    lats[0].floatValue());
        }
        GeometryFactory factory = new GeometryFactory();
        if (coords.length < 4) {
            LineString line = factory.createLineString(coords);
            return line;
        }
        LinearRing ring = factory.createLinearRing(coords);
        return factory.createPolygon(ring, null);
    }

    protected double metersToFl(float m) {
        if (m == -9999) {
            return m;
        }
        return meterToHft.convert(m);
    }

    protected String formatNumber(double m) {
        if (m == -9999) {
            return "XXX";
        }
        return String.format("%.0f", m);
    }

    protected Geometry worldToPixel(Geometry llPolygon) {
        Coordinate[] llCoords = llPolygon.getCoordinates();
        Coordinate[] pixelCoords = new Coordinate[llCoords.length];
        for (int i = 0; i < llCoords.length; i++) {
            double[] pixelCoord = descriptor.worldToPixel(
                    new double[] { llCoords[i].x, llCoords[i].y });
            pixelCoords[i] = new Coordinate(pixelCoord[0], pixelCoord[1]);
        }
        GeometryFactory factory = new GeometryFactory();
        if (llCoords.length < 4) {
            LineString line = factory.createLineString(pixelCoords);
            return line;
        }
        LinearRing ring = factory.createLinearRing(pixelCoords);
        return factory.createPolygon(ring, null);
    }

    @Override
    protected String[] getParameters() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        disposeShapes();
        super.project(crs);
    }

}