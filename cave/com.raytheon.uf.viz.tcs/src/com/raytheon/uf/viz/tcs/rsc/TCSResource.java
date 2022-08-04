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
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.tcs.Radius;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.dataplugin.tcs.util.TCSConstants;
import com.raytheon.uf.common.dataplugin.tcs.util.TcsUtil;
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
import com.raytheon.uf.viz.core.point.display.SymbolLoader;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.PointDataRequest;

import si.uom.SI;
import systems.uom.common.USCustomary;

/**
 * Resource for displaying data from a {@link TropicalCycloneSummary}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 22, 2010           jsanchez  Initial creation
 * Jul 29, 2014  3465     mapeters  Updated deprecated drawString() calls.
 * Sep 17, 2014  3632     bclement  fixed index out of bounds
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Nov 30, 2015  5149     bsteffen  Rename TcsUtil, update class javadoc
 * Jan 27, 2016  5285     tgurney   Remove dependency on dataURI
 * Sep 30, 2016  5887     mapeters  Added shapeMap to allow reuse of wireframes
 *                                  across paintInternal() calls
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * 
 * </pre>
 *
 * @author jsanchez
 */

public class TCSResource
        extends AbstractVizResource<TCSResourceData, MapDescriptor>
        implements IResourceDataChanged, TCSConstants {

    private static final UnitConverter NM_TO_M = USCustomary.NAUTICAL_MILE
            .getConverterTo(SI.METRE);

    private final Map<TropicalCycloneSummary, Map<LineStyle, IWireframeShape>> shapeMap = new HashMap<>();

    private final Map<DataTime, PointDataContainer> recordsToDisplay = new HashMap<>();

    private final Set<DataTime> dataTimesToUpdate = new HashSet<>();

    private final GeodeticCalculator gc = new GeodeticCalculator();

    private IFont font;

    private SymbolLoader symbolLoader;

    private RGB color;

    protected TCSResource(TCSResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        resourceData.addChangeListener(this);
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
        this.font = target.initializeFont("Monospace", 9,
                new Style[] { Style.ITALIC });
        this.symbolLoader = new SymbolLoader();
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }

        disposeShapes();
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

        DataTime dt = paintProps.getDataTime();
        if (resourceData.isHourlyForecast && dt != null) {
            dt = new DataTime(dt.getRefTime());
        }
        if (dataTimesToUpdate.contains(dt)) {
            updateRecords(dt);
        }

        PointDataContainer pdc = recordsToDisplay.get(dt);
        if (pdc != null) {
            for (int uriCounter = 0; uriCounter < pdc
                    .getCurrentSz(); uriCounter++) {
                PointDataView pdv = pdc.readRandom(uriCounter);
                if (resourceData.isHourlyForecast) {
                    TropicalCycloneSummary storm = TcsUtil.interplateStorm(pdv,
                            paintProps.getDataTime());
                    /*
                     * Set the storm's DataTime as the same refTime-only one
                     * passed to updateRecords() above, so that switching
                     * between frame counts correctly adds/removes records and
                     * their corresponding shapes
                     */
                    storm.setDataTime(dt);
                    paintHourlyForecast(target, paintProps, scale, storm);
                } else {
                    paintTrackSummary(target, paintProps, scale, pdv);
                }
            }
        }
    }

    private void paintHourlyForecast(IGraphicsTarget target,
            PaintProperties paintProps, double scale,
            TropicalCycloneSummary storm) throws VizException {
        displayOneStorm(target, paintProps, storm.getLongitude(),
                storm.getLatitude(), storm.getPressure(), storm.getName(),
                storm.getWindSpeed(), storm.isTropical(),
                storm.getDisplayTime(), scale, true);

        if (paintProps.getZoomLevel() * descriptor.getMapWidth()
                / 1000 < ZOOM_LEVEL) {
            drawLegends(target, paintProps, scale);
            drawRadius(target, storm);
        }
    }

    private void paintTrackSummary(IGraphicsTarget target,
            PaintProperties paintProps, double scale, PointDataView pdv)
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
            displayOneStorm(target, paintProps, longitudes[i].doubleValue(),
                    latitudes[i].doubleValue(), i == 0 ? pressure : 0, name,
                    windSpeeds[i].intValue(), isTropicals[i].intValue() == 1,
                    displayTimes[i], scale, false);
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
            PaintProperties paintProps, double longitude, double latitude,
            int pressure, String name, int windSpeed, boolean isTropical,
            String displayTime, double scale, boolean drawRadiusLabel)
            throws VizException {

        double[] loc = descriptor
                .worldToPixel(new double[] { longitude, latitude });
        scale *= getCapability(MagnificationCapability.class)
                .getMagnification();

        // Plotting a text string "Dissipated" if there is no forecast
        // data available.
        if ("Dissipated".equals(displayTime)) {
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

        List<DrawableString> strings = new ArrayList<>(4);
        // Plotting time
        DrawableString string1 = new DrawableString(displayTime, color);
        string1.font = font;
        string1.setCoordinates(loc[0] + (2 * scale), loc[1]);
        string1.verticallAlignment = VerticalAlignment.MIDDLE;
        strings.add(string1);

        // Plotting wind speed
        DrawableString string2 = new DrawableString(formatter.format(windSpeed),
                color);
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

        if (drawRadiusLabel) {
            // Plot the name of the wind radius
            DrawableString string4 = new DrawableString(name, color);
            string4.font = font;
            string4.setCoordinates(loc[0], loc[1] + (7 * scale));
            string4.horizontalAlignment = HorizontalAlignment.CENTER;
            string4.verticallAlignment = VerticalAlignment.MIDDLE;
            strings.add(string4);
        }

        target.drawStrings(strings);
    }

    private void drawRadius(IGraphicsTarget target,
            TropicalCycloneSummary storm) throws VizException {
        Coordinate[] coordinates = null;
        List<Radius> radiusList = storm.getRadiusList();
        double[] latLon = new double[] { storm.getLongitude(),
                storm.getLatitude() };

        // Draw the XX KT... lines.
        LineStyle lineStyle = null;

        /*
         * From shapeMap, get the map of LineStyles to the IWireframeShapes to
         * be drawn with each LineStyle for this storm. If the lineStyleMap
         * isn't in shapeMap yet, create it.
         */
        Map<LineStyle, IWireframeShape> lineStyleMap;
        synchronized (shapeMap) {
            lineStyleMap = shapeMap.get(storm);
            if (lineStyleMap == null && radiusList != null) {
                for (Radius rad : radiusList) {
                    // Make line styles
                    switch (rad.getKT_FT()) {
                    case 50:
                        // - - - for 50 KT
                        lineStyle = LineStyle.DASHED;
                        break;
                    case 34:
                        // . . . . for 34 KT
                        lineStyle = LineStyle.DOTTED;
                        break;
                    default:
                        // ----- for 64KT and 12FT
                        lineStyle = LineStyle.SOLID;
                    }
                    coordinates = makePolygon8(latLon, rad);

                    if (lineStyle != null && coordinates != null) {
                        if (lineStyleMap == null) {
                            lineStyleMap = new EnumMap<>(LineStyle.class);
                            shapeMap.put(storm, lineStyleMap);
                        }

                        IWireframeShape shape = lineStyleMap.get(lineStyle);
                        if (shape == null) {
                            shape = target.createWireframeShape(false,
                                    descriptor);
                            lineStyleMap.put(lineStyle, shape);
                        }

                        shape.addLineSegment(coordinates);
                    }
                }
            }
        }

        if (lineStyleMap != null) {
            for (Entry<LineStyle, IWireframeShape> entry : lineStyleMap
                    .entrySet()) {
                target.drawWireframeShape(entry.getValue(), color, 1.5f,
                        entry.getKey());
            }
        }
    }

    private Coordinate[] makePolygon8(double[] latLon, Radius rad) {
        List<Coordinate> coordinates = new ArrayList<>();

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
        Coordinate centerCoord = new Coordinate(loc[0], loc[1]);
        PixelCoverage pc = new PixelCoverage(centerCoord, scaleValue * 2,
                scaleValue * 2);

        IImage image = symbolLoader.getImage(target, color, stormSymbol);
        if (image != null) {
            target.drawRaster(image, pc, paintProps);
        }

    }

    private void drawLegends(IGraphicsTarget target, PaintProperties paintProps,
            double scale) throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        double maxX = extent.getMaxX();
        double minX = extent.getMinX();
        double maxY = extent.getMaxY();
        double minY = extent.getMinY();

        maxX = maxX < 0 ? 0 : maxX;
        maxX = maxX > 19_999 ? 19_999 : maxX;
        minX = minX < 0 ? 0 : minX;
        minX = minX > 19_999 ? 19_999 : minX;
        maxY = maxY < 0 ? 0 : maxY;
        maxY = maxY > 19_999 ? 19_999 : maxY;
        minY = minY < 0 ? 0 : minY;
        minY = minY > 19_999 ? 19_999 : minY;

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

    private Coordinate getLocation(double[] latLon, int direction,
            int distance) {
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
        // Request the point data
        pdc = PointDataRequest.requestPointDataAllLevels(dataTime,
                resourceData.getMetadataMap().get("pluginName")
                        .getConstraintValue(),
                getParameters(), null, resourceData.getMetadataMap());

        if (recordsToDisplay.containsKey(dataTime)) {
            recordsToDisplay.get(dataTime).combine(pdc);
        } else {
            recordsToDisplay.put(dataTime, pdc);
        }
        dataTimesToUpdate.remove(dataTime);

        disposeShapesForDataTime(dataTime);
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
        dataTimes.add(dataTime);
        dataTimesToUpdate.add(dataTime);
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        dataTimesToUpdate.remove(dataTime);
        disposeShapesForDataTime(dataTime);
    }

    private String[] getParameters() {
        return new String[] { LAT, LON, WIND_SPEED, PRESSURE, TYPE, SIZE,
                DISPLAY_TIME, TROPICAL, NAME, RAD_64, RAD_50, RAD_34, RAD_12 };
    }

    private void disposeShapes() {
        synchronized (shapeMap) {
            for (Map<LineStyle, IWireframeShape> map : shapeMap.values()) {
                for (IWireframeShape shape : map.values()) {
                    shape.dispose();
                }
            }
            shapeMap.clear();
        }
    }

    /**
     * Remove all TropicalCycloneSummary keys with the given dataTime from
     * shapeMap, and dispose the wireframe shapes they map to.
     *
     * @param dataTime
     */
    private void disposeShapesForDataTime(DataTime dataTime) {
        synchronized (shapeMap) {
            Iterator<TropicalCycloneSummary> stormItr = shapeMap.keySet()
                    .iterator();
            while (stormItr.hasNext()) {
                TropicalCycloneSummary storm = stormItr.next();
                if (dataTime.equals(storm.getDataTime())) {
                    for (IWireframeShape shape : shapeMap.get(storm).values()) {
                        shape.dispose();
                    }
                    stormItr.remove();
                }
            }
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) {
        disposeShapes();
        issueRefresh();
    }
}
