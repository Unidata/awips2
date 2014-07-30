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
package com.raytheon.uf.viz.tcs.rsc;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.tcs.Radius;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.dataplugin.tcs.util.TCSConstants;
import com.raytheon.uf.common.dataplugin.tcs.util.Util;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.raytheon.viz.pointdata.util.SymbolLoader;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2010            jsanchez     Initial creation
 * Jul 29, 2014 #3465      mapeters     Updated deprecated drawString() calls.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class TCSResource extends
        AbstractVizResource<TCSResourceData, MapDescriptor> implements
        IResourceDataChanged, TCSConstants {
    private IFont font;

    private SymbolLoader symbolLoader;

    private RGB color;

    private Map<DataTime, PointDataContainer> recordsToDisplay = new HashMap<DataTime, PointDataContainer>();

    private Map<DataTime, Collection<TropicalCycloneSummary>> recordsToParse = new HashMap<DataTime, Collection<TropicalCycloneSummary>>();

    private GeodeticCalculator gc = new GeodeticCalculator();

    private static final UnitConverter NM_TO_M = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    protected TCSResource(TCSResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
        this.dataTimes = new ArrayList<DataTime>();
    }

    @Override
    public String getName() {
        String name = this.resourceData.getPlotSource();
        return name == null ? "" : name;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (font != null) {
            font.dispose();
        }
        this.font = target.initializeFont("Monospace", 11,
                new Style[] { Style.ITALIC });
        this.symbolLoader = new SymbolLoader();
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        color = getCapability(ColorableCapability.class).getColor();
        font.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());
        
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double scale = (PLOT_WIDTH / 2.0) / screenToWorldRatio;

        DataTime dt = resourceData.isHourlyForecast ? new DataTime(paintProps
                .getDataTime().getRefTime()) : paintProps.getDataTime();
        Collection<TropicalCycloneSummary> toParse = recordsToParse.get(dt);
        if (toParse != null && toParse.size() > 0) {
            updateRecords(dt);
        }
        PointDataContainer pdc = null;
        if (resourceData.isHourlyForecast) {
            pdc = recordsToDisplay.get(dt);
        } else {
            pdc = recordsToDisplay.get(dt);
        }

        if (pdc != null) {
            for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                PointDataView pdv = pdc.readRandom(uriCounter);
                if (resourceData.isHourlyForecast) {
                    paintHourlyForecast(target, paintProps, pdv, scale);
                } else {
                    paintTrackSummary(target, paintProps, pdv, scale);
                }
            }
        }
    }

    private void paintHourlyForecast(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv, double scale)
            throws VizException {

        TropicalCycloneSummary storm = Util.interplateStorm(pdv,
                paintProps.getDataTime());
        displayOneStorm(target, paintProps, (float) storm.getLongitude(),
                (float) storm.getLatitude(), storm.getPressure(),
                storm.getName(), storm.getWindSpeed(), storm.isTropical(),
                storm.getDisplayTime(), scale, true, storm.getRadiusList());
    }

    private void paintTrackSummary(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv, double scale)
            throws VizException {
        int pressure = pdv.getInt(PRESSURE);
        String name = pdv.getString(NAME);

        Number[] longitudes = pdv.getNumberAllLevels(LON);
        Number[] latitudes = pdv.getNumberAllLevels(LAT);
        Number[] windSpeeds = pdv.getNumberAllLevels(WIND_SPEED);
        Number[] isTropicals = pdv.getNumberAllLevels(TROPICAL);
        String[] displayTimes = pdv.getStringAllLevels(DISPLAY_TIME);

        int size = pdv.getInt(SIZE);
        for (int i = 0; i < size; i++) {
            displayOneStorm(target, paintProps, longitudes[i].floatValue(),
                    latitudes[i].floatValue(), i == 0 ? pressure : 0, name,
                    windSpeeds[i].intValue(),
                    isTropicals[i].intValue() == 1 ? true : false,
                    displayTimes[i], scale, false, null);
        }

        double[] loc = calcNameLocation(latitudes[0].floatValue(),
                longitudes[0].floatValue(), latitudes[size - 1].floatValue(),
                longitudes[size - 1].floatValue());
        DrawableString string = new DrawableString(name, color);
        string.font = font;
        string.setCoordinates(loc[0], loc[1]);
        string.horizontalAlignment = HorizontalAlignment.CENTER;
        string.verticallAlignment = VerticalAlignment.MIDDLE;
        target.drawStrings(string);
    }

    private void displayOneStorm(IGraphicsTarget target,
            PaintProperties paintProps, float longitude, float latitude,
            int pressure, String name, int windSpeed, boolean isTropical,
            String displayTime, double scale, boolean drawRadius,
            ArrayList<Radius> radiusList) throws VizException {

        double[] loc = descriptor.worldToPixel(new double[] { longitude,
                latitude });
        scale *= getCapability(MagnificationCapability.class)
                .getMagnification();

        // Plotting a text string "Dissipated" if there is no forecast
        // data available.
        if (displayTime.equals("Dissipated")) {
            DrawableString[] strings = new DrawableString[2];
            strings[0] = new DrawableString(DISSIPATED, color);
            strings[0].font = font;
            strings[0].setCoordinates(loc[0], loc[1]);
            strings[0].horizontalAlignment = HorizontalAlignment.CENTER;
            strings[0].verticallAlignment = VerticalAlignment.MIDDLE;

            strings[1] = new DrawableString(name, color);
            strings[1].font = font;
            strings[1].setCoordinates(loc[0], loc[1] + (7 * scale));
            strings[1].horizontalAlignment = HorizontalAlignment.CENTER;
            strings[1].verticallAlignment = VerticalAlignment.MIDDLE;

            target.drawStrings(strings);
            return;
        }

        // Display control, display all storms or hurricanes with the press,
        // time, wind speed of its center.

        // Plotting storm symbol
        drawStormSymbol(target, paintProps, loc, windSpeed, isTropical, scale);

        List<DrawableString> strings = new ArrayList<DrawableString>(4);
        // Plotting time
        DrawableString string1 = new DrawableString(displayTime, color);
        string1.font = font;
        string1.setCoordinates(loc[0] + (2 * scale), loc[1]);
        string1.verticallAlignment = VerticalAlignment.MIDDLE;
        strings.add(string1);

        // Plotting wind speed
        DrawableString string2 = new DrawableString(
                formatter.format(windSpeed), color);
        string2.font = font;
        string2.setCoordinates(loc[0] - (1.5 * scale), loc[1]);
        string2.horizontalAlignment = HorizontalAlignment.RIGHT;
        string2.verticallAlignment = VerticalAlignment.MIDDLE;
        strings.add(string2);

        // Plotting pressure
        if (pressure != 0) {
            DrawableString string3 = new DrawableString(
                    formatter.format(pressure), color);
            string3.font = font;
            string3.setCoordinates(loc[0], loc[1] + (2 * scale));
            string3.horizontalAlignment = HorizontalAlignment.CENTER;
            string3.verticallAlignment = VerticalAlignment.MIDDLE;
            strings.add(string3);
        }

        // // -------------Draw the wind radius.---------------
        if (!drawRadius) {
            target.drawStrings(strings);
            return;
        }
        // Plotting name
        DrawableString string4 = new DrawableString(name, color);
        string4.font = font;
        string4.setCoordinates(loc[0], loc[1] + (7 * scale));
        string4.horizontalAlignment = HorizontalAlignment.CENTER;
        string4.verticallAlignment = VerticalAlignment.MIDDLE;
        strings.add(string4);

        target.drawStrings(strings);

        if (paintProps.getZoomLevel() * descriptor.getMapWidth() / 1000 < ZOOM_LEVEL) {
            drawLegends(target, paintProps, scale);
            drawRadius(target, name, descriptor.pixelToWorld(loc), scale,
                    radiusList);
        }
    }

    private void drawRadius(IGraphicsTarget target, String name,
            double[] latLon, double scale, ArrayList<Radius> radiusList)
            throws VizException {
        Coordinate[] coordinates = null;

        // Draw the XX KT... lines.
        LineStyle lineStyle = null;
        if (radiusList != null) {
            for (Radius rad : radiusList) {
                // Make line styles
                switch (rad.getKT_FT()) {
                case 50: // - - - for 50 KT
                    lineStyle = LineStyle.DASHED;
                    break;
                case 34: // . . . . for 34 KT
                    lineStyle = LineStyle.DOTTED;
                    break;
                default: // ----- for 64KT and 12FT
                    lineStyle = LineStyle.SOLID;
                }
                coordinates = makePolygon8(latLon, rad);

                if (lineStyle != null && coordinates != null) {
                    IWireframeShape shape = target.createWireframeShape(false,
                            descriptor);
                    shape.addLineSegment(coordinates);
                    target.drawWireframeShape(shape, color, 1.5f, lineStyle);
                    shape.dispose();
                }
            }
        }
    }

    private Coordinate[] makePolygon8(double[] latLon, Radius rad) {
        ArrayList<Coordinate> coordinates = new ArrayList<Coordinate>();

        Coordinate[] p = new Coordinate[8];
        Coordinate pT = new Coordinate();

        int distT;
        int directT;
        int[] direct = new int[8];
        int[] dist = new int[8];
        dist[1] = rad.getNE();
        dist[3] = rad.getSE();
        dist[5] = rad.getSW();
        dist[7] = rad.getNW();
        dist[0] = (rad.getNE() + rad.getNW()) / 2;
        dist[2] = (rad.getNE() + rad.getSE()) / 2;
        dist[4] = (rad.getSE() + rad.getSW()) / 2;
        dist[6] = (rad.getNW() + rad.getSW()) / 2;

        // Calculate the 8 points;
        for (int i = 0; i < 8; i++) {
            direct[i] = i * 45;
            p[i] = getLocation(latLon, direct[i], dist[i]);
        }

        // Drawing lines
        for (int j = 0; j < 8; j++) {
            coordinates.add(p[j]);
            if (rad.getKFUnit() == 'F') {
                if (j == 7) {
                    distT = (dist[7] + dist[0]) / 2;
                    directT = 338;
                } else {
                    distT = (dist[j + 1] + dist[j]) / 2;
                    directT = (direct[j + 1] + direct[j]) / 2;
                }
                pT = getLocation(latLon, directT, distT);
                coordinates.add(pT);
            }
        }

        coordinates.add(p[0]);
        return coordinates.toArray(new Coordinate[coordinates.size()]);
    }

    private void drawStormSymbol(IGraphicsTarget target,
            PaintProperties paintProps, double[] loc, int windSpeed,
            boolean isTropical, double scaleValue) throws VizException {
        char stormSymbol = UNKNOWN_SYMBOL;

        if (!isTropical) {
            stormSymbol = EXTRATROPICAL_SYMBOL;
        } else if (windSpeed >= 64) {
            stormSymbol = HURRICANE_SYMBOL;
        } else if (windSpeed >= 34) {
            stormSymbol = STORM_SYMBOL;
        }
        // Plotting Center
        double[] ul = new double[] { loc[0] - scaleValue, loc[1] - scaleValue,
                0 };

        double[] ur = new double[] { loc[0] + scaleValue, loc[1] - scaleValue,
                0 };

        double[] lr = new double[] { loc[0] + scaleValue, loc[1] + scaleValue,
                0 };

        double[] ll = new double[] { loc[0] - scaleValue, loc[1] + scaleValue,
                0 };

        PixelCoverage pc = new PixelCoverage(
                new Coordinate(ul[0], ul[1], ul[2]), new Coordinate(ur[0],
                        ur[1], ur[2]), new Coordinate(lr[0], lr[1], lr[2]),
                new Coordinate(ll[0], ll[1], ll[2]));

        IImage image = symbolLoader.getImage(target, color, stormSymbol);
        if (image != null) {
            target.drawRaster(image, pc, paintProps);
        }

    }

    private void drawLegends(IGraphicsTarget target,
            PaintProperties paintProps, double scale) throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        double maxX = extent.getMaxX();
        double minX = extent.getMinX();
        double maxY = extent.getMaxY();
        double minY = extent.getMinY();

        maxX = maxX < 0 ? 0 : maxX;
        maxX = maxX > 19999 ? 19999 : maxX;
        minX = minX < 0 ? 0 : minX;
        minX = minX > 19999 ? 19999 : minX;
        maxY = maxY < 0 ? 0 : maxY;
        maxY = maxY > 19999 ? 19999 : maxY;
        minY = minY < 0 ? 0 : minY;
        minY = minY > 19999 ? 19999 : minY;

        PixelExtent correctedExtent = new PixelExtent(minX, maxX, minY, maxY);
        double x = correctedExtent.getMinX();
        double y = correctedExtent.getMinY();

        target.clearClippingPlane();
        DrawableString[] strings = new DrawableString[4];
        strings[0] = new DrawableString(LEGEND_12_FT, color);
        strings[0].font = font;
        strings[0].setCoordinates(x, y + (scale * 2));
        strings[0].verticallAlignment = VerticalAlignment.MIDDLE;

        strings[1] = new DrawableString(LEGEND_34_KT, color);
        strings[1].font = font;
        strings[1].setCoordinates(x, y + (scale * 4));
        strings[1].verticallAlignment = VerticalAlignment.MIDDLE;

        strings[2] = new DrawableString(LEGEND_50_KT, color);
        strings[2].font = font;
        strings[2].setCoordinates(x, y + (scale * 6));
        strings[2].verticallAlignment = VerticalAlignment.MIDDLE;

        strings[3] = new DrawableString(LEGEND_64_KT, color);
        strings[3].font = font;
        strings[3].setCoordinates(x, y + (scale * 8));
        strings[3].verticallAlignment = VerticalAlignment.MIDDLE;

        target.drawStrings(strings);
        target.setupClippingPlane(paintProps.getView().getExtent());

    }

    private double[] calcNameLocation(float startLat, float startLon,
            float endLat, float endLon) {
        double[] d1 = { startLon, startLat };
        double[] d2 = { endLon, endLat };
        float latc = (float) (d1[1] + d2[1]) / 2;
        float lonc = (float) (d1[0] + d2[0]) / 2;

        float dlat = (float) (d1[1] - d2[1]);
        float dlon = (float) (d1[0] - d2[0]);

        float mag = (float) (4 / Math.sqrt((dlat * dlat) + (dlon * dlon)));
        dlat *= mag;
        dlon *= mag;
        latc += dlon;
        latc -= dlat;

        return descriptor.worldToPixel(new double[] { lonc, latc });
    }

    private Coordinate getLocation(double[] latLon, int direction, int distance) {
        double adjusted = direction % 360;
        if (adjusted > 180) {
            adjusted -= 360;
        } else if (adjusted < -180) {
            adjusted += 360;
        }

        double m = NM_TO_M.convert(Math.abs(distance));
        gc.setStartingGeographicPoint(latLon[0], latLon[1]);
        gc.setDirection(adjusted, m);

        Point2D p = gc.getDestinationGeographicPoint();

        return new Coordinate(p.getX(), p.getY());
    }

    protected void updateRecords(DataTime dataTime) throws VizException {
        PointDataContainer pdc = null;
        RequestConstraint constraint = new RequestConstraint();
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

        for (TropicalCycloneSummary record : recordsToParse.get(dataTime)) {
            constraint.setConstraintType(ConstraintType.IN);
            constraint.addToConstraintValueList(record.getDataURI());
        }
        constraints.put(DATAURI, constraint);
        // Request the point data
        pdc = PointDataRequest.requestPointDataAllLevels(dataTime, resourceData
                .getMetadataMap().get("pluginName").getConstraintValue(),
                getParameters(), null, constraints);

        if (recordsToDisplay.containsKey(dataTime)) {
            recordsToDisplay.get(dataTime).combine(pdc);
        } else {
            recordsToDisplay.put(dataTime, pdc);
        }
        recordsToParse.get(dataTime).clear();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            for (PluginDataObject p : pdo) {
                if (p instanceof TropicalCycloneSummary) {
                    addRecord((TropicalCycloneSummary) p);
                }
            }
        }
        issueRefresh();
    }

    protected void addRecord(TropicalCycloneSummary obj) {
        DataTime dataTime = obj.getDataTime();
        Collection<TropicalCycloneSummary> toParse = recordsToParse
                .get(dataTime);
        if (toParse == null) {
            dataTimes.add(dataTime);
            Collections.sort(this.dataTimes);
            toParse = new ArrayList<TropicalCycloneSummary>();
            recordsToParse.put(dataTime, toParse);
        }
        toParse.add(obj);
    }

    private String[] getParameters() {
        return new String[] { LAT, LON, WIND_SPEED, PRESSURE, TYPE, SIZE,
                DISPLAY_TIME, TROPICAL, NAME, RAD_64, RAD_50, RAD_34, RAD_12 };
    }

}
