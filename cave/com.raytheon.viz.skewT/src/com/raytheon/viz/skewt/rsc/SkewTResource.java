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
package com.raytheon.viz.skewt.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.edex.meteoLib.WindComp;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.sounding.SoundingLayer.DATA_TYPE;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.raytheon.uf.viz.xy.map.rsc.PointRenderable;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.LineStroke;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.core.graphing.WindBarbFactory;
import com.raytheon.viz.skewt.Activator;
import com.raytheon.viz.skewt.SkewTDescriptor;
import com.raytheon.viz.skewt.SkewtDisplay;
import com.raytheon.viz.skewt.rscdata.SkewTResourceData;
import com.raytheon.viz.skewt.ui.SkewTConstants;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Resource to render the skewt data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SkewTResource extends
        AbstractVizResource<SkewTResourceData, SkewTDescriptor> implements
        IInsetMapResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(SkewTResource.class);
    protected static final int TEMP_CHANGE_WINDOW = 4;

    protected static final double BARB_LENGTH = 3.5;

    private static final UnitConverter metersToFeet = SI.METER
            .getConverterTo(NonSI.FOOT);

    private static final UnitConverter celciusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private static final UnitConverter kelvinToCelsius = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static final UnitConverter footToMeters = NonSI.FOOT
            .getConverterTo(SI.METER);

    private static final char degree = '\u00B0';

    private static final char squared = '\u00B2';

    private float _hgtHelLyr = 3000.0f; // hgt of the layer for determining

    // helicity

    protected Map<Date, SoundingParams> soundingMap;

    private Hashtable<Coordinate, SoundingLayer> windMap = new Hashtable<Coordinate, SoundingLayer>();

    protected DataTime displayedSounding;

    protected SkewtDisplay display;

    protected boolean editable = false;

    protected IFont smallFont;

    protected IFont normalFont;

    protected ListenerList listenerList = new ListenerList();

    private PointRenderable point;

    /**
     * @param resourceData
     * @param properties
     */
    public SkewTResource(SkewTResourceData resourceData,
            LoadProperties properties) {
        super(resourceData, properties);
        dataTimes = new ArrayList<DataTime>();
        soundingMap = new HashMap<Date, SoundingParams>();

        if (resourceData != null) {
            for (VerticalSounding vs : resourceData.getSoundings()) {
                addSounding(vs.getDataTime(), vs);
            }

            resourceData.addChangeListener(new IResourceDataChanged() {
                @Override
                public void resourceChanged(ChangeType type, Object object) {
                    if (type == ChangeType.CAPABILITY) {
                        if (object instanceof MagnificationCapability) {
                            if (smallFont != null) {
                                smallFont.dispose();
                                smallFont = null;
                            }
                            if (normalFont != null) {
                                normalFont.dispose();
                                normalFont = null;
                            }
                        }
                    }

                }

            });
        }
    }

    @Override
    protected void disposeInternal() {
        if (smallFont != null) {
            smallFont.dispose();
            smallFont = null;
        }
        if (normalFont != null) {
            normalFont.dispose();
            normalFont = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (smallFont == null) {
            float fontSize = (float) (10 * getCapability(
                    MagnificationCapability.class).getMagnification());
            smallFont = target.initializeFont(target.getDefaultFont()
                    .getFontName(), fontSize, null);
        }
        if (normalFont == null) {
            float fontSize = (float) (12 * getCapability(
                    MagnificationCapability.class).getMagnification());
            normalFont = target.initializeFont(target.getDefaultFont()
                    .getFontName(), fontSize, null);
        }

        setSoundingDate(paintProps.getDataTime());

        double zoomLevel = paintProps.getZoomLevel();
        double density = getCapability(DensityCapability.class).getDensity();
        SoundingParams sp = getSoundingParameters();
        if (sp != null && sp.getInterleavedData().size() > 0) {
            RGB color = getCapability(ColorableCapability.class).getColor();

            WGraphics skewTWorld = getSkewTWorld();
            plotTempCurve(target, zoomLevel, skewTWorld, sp, color);
            plotTdCurve(target, zoomLevel, skewTWorld, sp, color);

            drawHgtlabel(target, zoomLevel, skewTWorld, sp, color);

            WGraphics tempChangeWorld = getTempChangeWorld();
            draw24HrTempChange(target, tempChangeWorld, sp, color);

            WGraphics hodoWorld = getHodoWorld();
            drawHodo(target, zoomLevel, hodoWorld, sp);
            drawWindPlot(target, zoomLevel, density, skewTWorld, sp, color);
            if (zoomLevel <= .50) {
                // draw storm inflow info and helicity lines
                plotStormInflow(target, hodoWorld, zoomLevel, sp);
            }

        }
    }

    public void addSounding(DataTime dataTime, VerticalSounding sounding) {

        dataTimes.add(dataTime);
        soundingMap.put(dataTime.getValidTime().getTime(), new SoundingParams(
                sounding));
        Collections.sort(dataTimes);
    }

    public void setSoundingMap(Map<DataTime, SoundingParams> map) {
        soundingMap = new HashMap<Date, SoundingParams>();
        for (DataTime dt : map.keySet()) {
            soundingMap.put(dt.getValidTime().getTime(), map.get(dt));
        }
        dataTimes.clear();
        dataTimes.addAll(map.keySet());
        Collections.sort(dataTimes);

        if (soundingMap == null) {
            soundingMap = new HashMap<Date, SoundingParams>();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.ITimeSeqResource#setDisplayedDate
     * (java.util.Date)
     */
    public DataTime getDisplayedDate() {
        return displayedSounding;
    }

    protected SoundingParams retrieveVerticalSounding(DataTime time) {
        if (time == null) {
            return null;
        }
        return soundingMap.get(time.getValidTime().getTime());
    }

    @Override
    public String getName() {
        String name = "Skewt";
        SoundingParams sp = this.getSoundingByDataTime(descriptor
                .getTimeForResource(this));

        if (sp != null) {
            VerticalSounding vs = sp.getInterleavedData();
            String display = vs.getDisplayFormat();
            if (display != null) {
                if (display.indexOf("{$pointId$}") > 0) {
                    String pt = resourceData.getPointLetter();
                    if (pt != null) {
                        name = display.replace("{$pointId$}", " pt" + pt + " ")
                                + "Skewt";
                    }
                } else {
                    name = display + " Skewt";
                }
            } else {
                name = vs.getName() + " Skewt";
            }
        }

        return name;
    }

    @Override
    public void setDescriptor(SkewTDescriptor descriptor) {
        super.setDescriptor(descriptor);
        RGB rgb = ColorUtil.getNewColor(descriptor);
        getCapability(ColorableCapability.class).setColor(rgb);
    }

    public void setSoundingDate(DataTime dt) {
        if (dt == null && displayedSounding == null) {
            return;
        }
        if (dt != null && dt.equals(displayedSounding)) {
            return;
        }
        displayedSounding = dt;
        fireListeners();
    }

    public SoundingParams getSoundingParameters() {
        if (displayedSounding == null) {
            return null;
        }
        SoundingParams sp = this.retrieveVerticalSounding(displayedSounding);
        return sp;
    }

    /**
     * @param c
     * @return
     */
    private Coordinate getWindfromSounding(Coordinate c) {
        double dist = 4;
        Coordinate eo = null;
        Coordinate ee = null;
        Iterator<Coordinate> iter = windMap.keySet().iterator();

        while (iter.hasNext()) {
            eo = iter.next();
            if (eo.distance(c) < dist) {
                dist = eo.distance(c);
                ee = eo;
            }
        }
        return ee;
    }

    protected SkewtDisplay getDisplay() {
        return display;
    }

    public void setDisplay(SkewtDisplay display) {
        this.display = display;
    }

    protected WGraphics getSkewTWorld() {
        return getDisplay().getSkewTWorld();
    }

    protected WGraphics getHodoWorld() {
        return getDisplay().getHodoWorld();
    }

    protected WGraphics getTempChangeWorld() {
        return getDisplay().getTempChangeWorld();
    }

    /**
     * Allows you to grab a sounding from a different time
     * 
     * @param dt
     * @return
     */
    public SoundingParams getSoundingByDataTime(DataTime dt) {
        // find a close time to 24 hours, give 4 hour lee way
        SoundingParams sp = this.retrieveVerticalSounding(dt);
        return sp;
    }

    /**
     * Allows you to grab a sounding from a similar time span back
     * 
     * @param dt
     * @return
     */
    public SoundingParams getSoundingByTimeSpan(DataTime dt, int span) {
        // find a time within span of dt
        SoundingParams sp = null;

        try {
            sp = getSoundingByDataTime(dt);
        } catch (NullPointerException npe) {
            for (DataTime date : dataTimes) {
                if (Math.abs(date.getRefTimeAsCalendar().get(Calendar.HOUR)
                        - dt.getRefTimeAsCalendar().get(Calendar.HOUR)) <= span) {
                    sp = this.retrieveVerticalSounding(dt);
                }
            }
        }
        return sp;
    }

    /**
     * Returns coordinate of sounding station
     * 
     * @return loc
     */
    public Coordinate getLocation() {
        Coordinate loc = null;
        SoundingParams sp = getSoundingParameters();
        if (sp != null) {
            loc = new Coordinate();
            loc.x = sp.getAnalysisData().getLongitude();
            loc.y = sp.getAnalysisData().getLatitude();
        }
        return loc;
    }

    /**
     * Draw the temperature curve
     * 
     * @throws VizException
     */
    protected void plotTempCurve(IGraphicsTarget target, double zoomLevel,
            WGraphics world, SoundingParams sp, RGB color) throws VizException {

        int lineWidth = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        LineStyle lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();

        double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;

        PixelExtent extent = new PixelExtent(getDisplay().getSkewTBackground()
                .getRectangle());
        target.setupClippingPlane(extent);

        Coordinate c0 = null;
        for (SoundingLayer layer : sp.getInterleavedData()) {
            double t = layer.getTemperature();
            double pressure = layer.getPressure();
            if (t < SoundingLayer.MISSING && pressure >= minPressure
                    && pressure <= maxPressure) {

                Coordinate c1 = WxMath.getSkewTXY(pressure, kelvinToCelsius
                        .convert(t));

                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);

                if (c0 != null) {
                    target.drawLine(c0.x, c0.y, 0.0, c1.x, c1.y, 0.0, color,
                            lineWidth, lineStyle);
                }

                if (editable) {
                    drawEditHandle(target, world, zoomLevel, c1.x, c1.y, color);
                }

                c0 = c1;
            }
        }

        target.clearClippingPlane();
    }

    /**
     * Draw the dewpoint curve
     * 
     * @throws VizException
     */
    protected void plotTdCurve(IGraphicsTarget target, double zoomLevel,
            WGraphics world, SoundingParams sp, RGB color) throws VizException {

        int lineWidth = getCapability(OutlineCapability.class)
                .getOutlineWidth();
        LineStyle lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();

        double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;

        PixelExtent extent = new PixelExtent(getDisplay().getSkewTBackground()
                .getRectangle());
        target.setupClippingPlane(extent);

        Coordinate c0 = null;
        for (SoundingLayer layer : sp.getInterleavedData()) {
            double td = layer.getDewpoint();
            double pressure = layer.getPressure();
            if (td < SoundingLayer.MISSING && pressure >= minPressure
                    && pressure <= maxPressure) {

                // set the line style to dashed for the interpolated layers
                if (layer.isDptInterpolated()) {
                    lineStyle = LineStyle.DASHED;
                } else {
                    lineStyle = LineStyle.SOLID;
                }
                Coordinate c1 = WxMath.getSkewTXY(pressure, kelvinToCelsius
                        .convert(td));

                c1.x = world.mapX(c1.x);
                c1.y = world.mapY(c1.y);

                if (c0 != null) {
                    target.drawLine(c0.x, c0.y, 0.0, c1.x, c1.y, 0.0, color,
                            lineWidth, lineStyle);
                }

                if (editable) {
                    drawEditHandle(target, world, zoomLevel, c1.x, c1.y, color);
                }

                c0 = c1;
            }
        }

        target.clearClippingPlane();
    }

    /**
     * Draws the windstaff bar. Draws to scales one on left of bar at 2K
     * intervals (Meters). Draws right scale in feet (5K intervals.)
     * 
     * @param drawColor
     *            Color to draw staff in.
     * @throws VizException
     */
    protected void drawHgtlabel(IGraphicsTarget target, double zoomLevel,
            WGraphics world, SoundingParams sp, RGB drawColor)
            throws VizException {

        double x = world.mapX(SkewTConstants.right + 2 * BARB_LENGTH
                * zoomLevel)
                + target.getStringBounds(smallFont, "99").getWidth();

        double y;
        double y0 = 0;

        double maxPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double minPressure = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;

        float[] pressures = sp.getPressures();
        float[] heights = sp.getHeights();
        int minIndex = heights.length;
        for (int i = 0; i < heights.length; i++) {
            if (heights[i] < SoundingLayer.MISSING
                    && pressures[i] < SoundingLayer.MISSING
                    && pressures[i] <= maxPressure) {
                minIndex = i;
                break;
            }
        }
        int maxIndex = -1;
        for (int i = heights.length - 1; i >= 0; i--) {
            if (heights[i] < SoundingLayer.MISSING
                    && pressures[i] < SoundingLayer.MISSING
                    && pressures[i] >= minPressure) {
                maxIndex = i;
                break;
            }
        }

        if (maxIndex <= minIndex) {
            return;
        }

        int kft = 0;
        while (footToMeters.convert(kft * 1000) <= heights[minIndex]) {
            kft += 5;
        }
        int km = 0;
        while (km * 1000 <= heights[minIndex]) {
            km += 2;
        }
        int tickLength = 5;
        int prevIndex = -1;
        for (int index = minIndex; index <= maxIndex; index++) {
            // skip levels with missing height
            if (pressures[index] >= SoundingLayer.MISSING
                    || heights[index] >= SoundingLayer.MISSING) {
                continue;
            }

            // Special case of top and bottom of sounding
            if ((index == minIndex) || (index == maxIndex)) {
                y = world.mapY(WxMath.getSkewTXY(pressures[index], 0).y);
                target.drawString(smallFont, String.format("%.0f",
                        heights[index]), x - tickLength - 1, y, 0.0,
                        TextStyle.NORMAL, drawColor, HorizontalAlignment.RIGHT,
                        VerticalAlignment.MIDDLE, null);
                target
                        .drawLine(x - tickLength, y, 0.0, x, y, 0.0, drawColor,
                                1);

                if (index == minIndex) {
                    y0 = y;
                } else {
                    target.drawLine(x, y0, 0.0, x, y, 0.0, drawColor, 1);
                }
            } else {

                // plot the kilometers scale
                double meters;
                while ((meters = (km * 1000)) <= heights[index]) {
                    double pressure = pressures[prevIndex]
                            + (meters - heights[prevIndex])
                            * (pressures[index] - pressures[prevIndex])
                            / (heights[index] - heights[prevIndex]);

                    y = world.mapY(WxMath.getSkewTXY(pressure, 0).y);
                    target.drawString(smallFont, Integer.toString(km), x
                            - tickLength - 1, y, 0.0, TextStyle.NORMAL,
                            drawColor, HorizontalAlignment.RIGHT,
                            VerticalAlignment.MIDDLE, null);
                    target.drawLine(x - tickLength, y, 0.0, x, y, 0.0,
                            drawColor, 1);

                    km += 2;
                }

                // plot the feet scale
                while ((meters = footToMeters.convert(kft * 1000)) <= heights[index]) {
                    double pressure = pressures[prevIndex]
                            + (meters - heights[prevIndex])
                            * (pressures[index] - pressures[prevIndex])
                            / (heights[index] - heights[prevIndex]);

                    y = world.mapY(WxMath.getSkewTXY(pressure, 0).y);
                    target.drawString(smallFont, Integer.toString(kft), x
                            + tickLength + 1, y, 0.0, TextStyle.NORMAL,
                            drawColor, HorizontalAlignment.LEFT,
                            VerticalAlignment.MIDDLE, null);
                    target.drawLine(x + tickLength, y, 0.0, x, y, 0.0,
                            drawColor, 1);

                    kft += 5;
                }
            }
            prevIndex = index;
        }
    }

    /**
     * Draws the sounding data in the appropriate color.
     * 
     * @param sp
     * @param color
     */
    protected void drawWindPlot(IGraphicsTarget target, double zoomLevel,
            double density, WGraphics world, SoundingParams sp, RGB color) {
        List<List<LineStroke>> winds = makeWindData(sp.getInterleavedData(),
                zoomLevel, density);
        for (List<LineStroke> barb : winds) {
            for (LineStroke stroke : barb) {
                stroke.render(target, world, color);
            }
        }
    }

    /**
     * Gets the windData for the sounding.
     * 
     * @param sounding
     * @return
     */
    protected List<List<LineStroke>> makeWindData(VerticalSounding sounding,
            double zoomLevel, double density) {
        ArrayList<List<LineStroke>> windList = new ArrayList<List<LineStroke>>();

        double windX = SkewTConstants.right + BARB_LENGTH * zoomLevel;

        double prevY = 1e37;
        double dVert = BARB_LENGTH * 0.3 * zoomLevel / Math.min(density, 4.0);
        double windY;
        for (SoundingLayer layer : sounding) {
            float pressure = layer.getPressure();
            float spd = layer.getWindSpeed();
            float dir = layer.getWindDirection();

            if (pressure >= SoundingLayer.NODATA || pressure < 100 || spd > 250
                    || dir > 360) {
                continue;
            }

            // Get the vertical ordinate.
            windY = WxMath.getSkewTXY(pressure, 0).y;
            double dBarb = Math.abs(windY - prevY);
            if (dBarb < dVert) {
                continue;
            }
            prevY = windY;

            List<LineStroke> barb = WindBarbFactory.getWindGraphics(
                    metersPerSecondToKnots.convert(spd), (double) dir);
            if (barb != null) {
                WindBarbFactory.scaleBarb(barb, zoomLevel);
                WindBarbFactory.translateBarb(barb, windX, windY);
                windList.add(barb);
            }
        }
        return windList;
    }

    protected void drawHodo(IGraphicsTarget target, double zoomLevel,
            WGraphics world, SoundingParams sp) throws VizException {
        RGB color = getCapability(ColorableCapability.class).getColor();
        int width = getCapability(OutlineCapability.class).getOutlineWidth();
        LineStyle lineStyle = getCapability(OutlineCapability.class)
                .getLineStyle();

        PixelExtent extent = new PixelExtent(getDisplay().getHodoBackground()
                .getRectangle());
        target.setupClippingPlane(extent);

        windMap.clear();
        Coordinate c0 = null;
        Coordinate c1;
        for (SoundingLayer layer : sp.getInterleavedData()) {
            double pressure = layer.getPressure();
            if (pressure < 100 || pressure >= SoundingLayer.NODATA) {
                continue;
            }
            float u = layer.getWindU();
            float v = layer.getWindV();
            if (u > 999) {
                continue;
            }

            c1 = new Coordinate(u, v);
            windMap.put(c1, layer);
            if (c0 != null) {
                target.drawLine(world.mapX(c0.x), world.mapY(c0.y), 0, world
                        .mapX(c1.x), world.mapY(c1.y), 0, color, width,
                        lineStyle);

            }
            if (zoomLevel <= .50) {
                String levl = String
                        .format("%.1f", layer.getGeoHeight() * .001);
                target.drawString(normalFont, levl, world.mapX(c1.x), world
                        .mapY(c1.y), 0.0, IGraphicsTarget.TextStyle.NORMAL,
                        color, IGraphicsTarget.HorizontalAlignment.CENTER, 0.0);
            }
            c0 = c1;
        }

        target.clearClippingPlane();
    }

    /**
     * compute the 0-3 km storm inflow and plot the lines.
     */
    private void plotStormInflow(IGraphicsTarget target, WGraphics world,
            double zoomLevel, SoundingParams sp) {

        if (sp.helicity() == null) {
            return;
        }

        float elev = sp.getAnalysisData().get(0).getGeoHeight();
        ArrayList<Coordinate> hodoInflow = new ArrayList<Coordinate>();
        float uCompStorm = 0f;
        float vCompStorm = 0f;
        float[] _heights = sp.getHeights();
        float[] _uComp = sp.getWindUs();
        float[] _vComp = sp.getWindVs();
        float helHGT = sp.get_hgtHelLyr();

        for (int i = 0; i < _uComp.length; i++) {
            if (_heights[i] > helHGT + elev) {
                break;
            }
            if (_uComp[i] > 999) {
                continue;
            }
            hodoInflow.add(new Coordinate(_uComp[i], _vComp[i]));
        }
        Coordinate cc = WxMath.uvComp(sp.helicity().getStormMotionSpd(), sp
                .helicity().getStormMotionDir());
        uCompStorm = (float) cc.x;
        vCompStorm = (float) cc.y;

        // draw the Storm Motion indicator
        try {
            target.drawFilledCircle(world.mapX(uCompStorm), world
                    .mapY(vCompStorm), 0.0, SkewTConstants.endpointRadius
                    * zoomLevel, SkewTConstants.pointEditColor);
        } catch (VizException e) {
            e.printStackTrace();
        }
        // Calculate the storm inflow lines and draw every third line.
        // If the last entry in the sequence is a third multiple, draw
        // it anyway to mark the end of the inflow pattern.
        RGB color = getCapability(ColorableCapability.class).getColor();
        int index = 0;
        for (int i = 0; i < hodoInflow.size(); i++) {
            if ((i % 3) != 0) {
                if (i != hodoInflow.size() - 1) {
                    continue;
                }
            }
            try {
                target.drawLine(world.mapX(hodoInflow.get(i).x), world
                        .mapY(hodoInflow.get(i).y), 0.0,
                        world.mapX(uCompStorm), world.mapY(vCompStorm), 0.0,
                        color, SkewTConstants.parcelLineWidth,
                        IGraphicsTarget.LineStyle.DOTTED);
            } catch (VizException e) {
                e.printStackTrace();
            }

            index++;
        }
        plotHelicityContours(target, world, zoomLevel, sp);
    }

    /**
     * compute the 0-3 km helicity contours and plot the lines.
     */
    private void plotHelicityContours(IGraphicsTarget target, WGraphics world,
            double zoomLevel, SoundingParams sp) {
        float uMinExtent = 0f;
        float uMaxExtent = 0f;
        float vMinExtent = 0f;
        float vMaxExtent = 0f;
        String scratch = "";
        int srh = 0;
        int labelInterval = 0;
        float labelOffset = 0;
        int startContour = 0;
        int endContour = 0;
        int contourInterval = 0;
        float _ghx = sp.get_ghx();
        float _ghy = sp.get_ghy();
        float _helicity = sp.helicity().getHelicity();

        // Indexer to the helicity contours and labels
        RGB color = getCapability(ColorableCapability.class).getColor();

        // Get the predefined values of umin, umax, vmin, and vmax
        PixelExtent extent = new PixelExtent(getDisplay().getHodoBackground()
                .getRectangle());
        Coordinate newUVupperLeft = world.unMap(extent.getMinX(), extent
                .getMinY());
        Coordinate newUVlowerRight = world.unMap(extent.getMaxX(), extent
                .getMaxY());
        float newUmin = (float) newUVupperLeft.x;
        float newVmax = (float) newUVupperLeft.y;
        float newUmax = (float) newUVlowerRight.x;
        float newVmin = (float) newUVlowerRight.y;

        // Increase the contour interval size if at a higher zoom level;
        // Coincides with the sounding depictable threshold for zoom contouring
        startContour = 50;
        endContour = 800;
        contourInterval = 50;
        labelInterval = 50;
        labelOffset = 1.5f;

        if (Math.abs(_ghy) > Math.abs(_ghx)) {
            for (srh = startContour; srh <= endContour; srh += contourInterval) {
                uMinExtent = newUmin;
                uMaxExtent = newUmax;
                vMinExtent = ((srh - _helicity) - (_ghx * uMinExtent)) / _ghy;
                vMaxExtent = ((srh - _helicity) - (_ghx * uMaxExtent)) / _ghy;
                // check to see if lines run off of top left edge, if so clip
                // them
                // and adjust the labels
                if (vMaxExtent > newVmax) {
                    uMaxExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmax - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMaxExtent = newVmax;
                }
                // check to see if helicity lines go below bottom left of the
                // hodo, if so clip
                // them and adjust the labels
                if (vMinExtent < newVmin) {
                    uMinExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmin - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMinExtent = newVmin;
                }
                // check to see if helicity lines go below bottom right of the
                // hodo, if so clip
                // them and adjust the labels
                if (vMaxExtent < newVmin) {
                    uMaxExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmin - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMaxExtent = newVmin;
                }
                // check to see if lines run off of top left edge, if so clip
                // them
                // and adjust the labels
                if (vMinExtent > newVmax) {
                    uMinExtent = uMinExtent
                            + ((uMaxExtent - uMinExtent) * ((newVmax - vMinExtent) / (vMaxExtent - vMinExtent)));
                    vMinExtent = newVmax;
                }
                if (vMaxExtent <= newVmax && uMaxExtent <= newUmax
                        && uMinExtent >= newUmin && vMaxExtent >= newVmin
                        && vMinExtent <= newVmax) {
                    try {
                        target.drawLine(world.mapX(uMinExtent), world
                                .mapY(vMinExtent), 0.0, world.mapX(uMaxExtent),
                                world.mapY(vMaxExtent), 0.0, color, 1,
                                IGraphicsTarget.LineStyle.DASHED);
                        // label the contours
                        if ((srh % labelInterval) == 0) {
                            scratch = String.format("%d", srh);
                            target.drawString(smallFont, scratch, world
                                    .mapX(uMinExtent - labelOffset), world
                                    .mapY(vMinExtent), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                            target.drawString(smallFont, scratch, world
                                    .mapX(uMaxExtent + labelOffset), world
                                    .mapY(vMaxExtent), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                        }
                    } catch (VizException e) {
                        e.printStackTrace();
                    }
                }
            }
        } else {
            vMinExtent = newVmin;
            vMaxExtent = newVmax;
            for (srh = startContour; srh <= endContour; srh += contourInterval) {
                uMinExtent = ((srh - _helicity) - (_ghy * vMinExtent)) / _ghx;
                uMaxExtent = ((srh - _helicity) - (_ghy * vMaxExtent)) / _ghx;
                // check to see if lines run off of the bottom edge, if so clip
                // them
                // and adjust the labels
                if (uMinExtent < newUmin) {
                    vMaxExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((uMinExtent - newUmin) / (newUmax - newUmin)));
                    uMinExtent = newUmin;
                }
                // check to see if helicity lines go above the hodo, if so clip
                // them and adjust the labels
                if (uMaxExtent > newUmax) {
                    vMinExtent = vMinExtent
                            + ((newVmax - newVmin) * ((uMaxExtent - newUmin) / (newUmax - newUmin)));
                    uMaxExtent = newUmax;
                }
                // check to see if helicity lines go left of bottom left of the
                // hodo, if so clip
                // them and adjust the labels
                if (uMaxExtent < newUmin) {
                    vMaxExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((newUmin - uMinExtent) / (uMaxExtent - uMinExtent)));
                    uMaxExtent = newUmin;
                }
                // check to see if lines run off of top right edge, if so clip
                // them
                // and adjust the labels
                if (uMinExtent > newUmax) {
                    vMinExtent = vMinExtent
                            + ((vMaxExtent - vMinExtent) * ((newUmax - uMinExtent) / (uMaxExtent - uMinExtent)));
                    uMinExtent = newUmax;
                }

                if (vMaxExtent <= newVmax && uMaxExtent <= newUmax
                        && uMinExtent >= newUmin && uMinExtent <= newUmax) {
                    try {
                        target.drawLine(world.mapX(uMinExtent), world
                                .mapY(vMinExtent), 0.0, world.mapX(uMaxExtent),
                                world.mapY(vMaxExtent), 0.0, color, 1,
                                IGraphicsTarget.LineStyle.DASHED);
                        // label the contours
                        if ((srh % labelInterval) == 0) {
                            scratch = String.format("%d", srh);
                            target.drawString(smallFont, scratch, world
                                    .mapX(uMinExtent), world.mapY(vMinExtent
                                    - labelOffset), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                            target.drawString(smallFont, scratch, world
                                    .mapX(uMaxExtent), world.mapY(vMaxExtent
                                    + labelOffset), 0.0,
                                    IGraphicsTarget.TextStyle.NORMAL, color,
                                    IGraphicsTarget.HorizontalAlignment.CENTER,
                                    0.0);
                        }
                    } catch (VizException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        if (_helicity == SoundingLayer.MISSING) {
            final String sthelicity = "STORM RELATIVE HELICITY=NA";
            final String stmo = "STORM MOTION=NA";
            try {
                target.drawString(normalFont, sthelicity, world.mapX(newUmax
                        - 3 * normalFont.getFontSize() * zoomLevel), world
                        .mapY(newVmax - 1 * normalFont.getFontSize()
                                * zoomLevel), 0.0,
                        IGraphicsTarget.TextStyle.NORMAL, color,
                        IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);

                target
                        .drawString(
                                normalFont,
                                stmo,
                                world.mapX(newUmax - 3
                                        * normalFont.getFontSize() * zoomLevel),
                                world
                                        .mapY(newVmax
                                                - (1.5 * normalFont
                                                        .getFontSize() * zoomLevel)),
                                0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                                IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } else {
            String stRelHelicity = getStormRelativeHelicityString(sp);
            String stRelMotion = getStormRelativeMotionString(sp);
            final String hgt = "HEIGHTS ARE KM AGL";
            String helTop = String.format("TOP OF HELICITY LYR= %d km AGL",
                    (int) (_hgtHelLyr / 1000.0));
            final String stmInd = "*                                             ";
            final String stmPlot = "IS PLOTTED STORM MOTION";
            try {
                target.drawString(normalFont, hgt, world.mapX(newUmax - 3
                        * normalFont.getFontSize() * zoomLevel), world
                        .mapY(newVmax - 1 * normalFont.getFontSize()
                                * zoomLevel), 0.0,
                        IGraphicsTarget.TextStyle.NORMAL, color,
                        IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);

                target
                        .drawString(
                                normalFont,
                                stRelHelicity,
                                world.mapX(newUmax - 3
                                        * normalFont.getFontSize() * zoomLevel),
                                world
                                        .mapY(newVmax
                                                - (1.5 * normalFont
                                                        .getFontSize() * zoomLevel)),
                                0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                                IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
                target.drawString(normalFont, stRelMotion, world.mapX(newUmax
                        - 3 * normalFont.getFontSize() * zoomLevel), world
                        .mapY(newVmax
                                - (2 * normalFont.getFontSize() * zoomLevel)),
                        0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                        IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
                target
                        .drawString(
                                normalFont,
                                helTop,
                                world.mapX(newUmax - 3
                                        * normalFont.getFontSize() * zoomLevel),
                                world
                                        .mapY(newVmax
                                                - (2.5 * normalFont
                                                        .getFontSize() * zoomLevel)),
                                0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                                IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
                target.drawString(normalFont, stmInd, world.mapX(newUmax - 3
                        * normalFont.getFontSize() * zoomLevel), world
                        .mapY(newVmax
                                - (3 * normalFont.getFontSize() * zoomLevel)),
                        0.0, IGraphicsTarget.TextStyle.NORMAL,
                        SkewTConstants.pointEditColor,
                        IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
                target.drawString(normalFont, stmPlot, world.mapX(newUmax - 3
                        * normalFont.getFontSize() * zoomLevel), world
                        .mapY(newVmax
                                - (3 * normalFont.getFontSize() * zoomLevel)),
                        0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                        IGraphicsTarget.HorizontalAlignment.RIGHT, 0.0);
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    protected void draw24HrTempChange(IGraphicsTarget target, WGraphics world,
            SoundingParams sp, RGB color) throws VizException {
        Calendar curCal = sp.getInterleavedData().getDataTime().getValidTime();
        curCal.add(Calendar.DAY_OF_MONTH, -1);

        SoundingParams sp24 = getSoundingByTimeSpan(new DataTime(curCal),
                TEMP_CHANGE_WINDOW);

        drawVerticalVelocityLine(target, world, sp.getInterleavedData(), color);

        PixelExtent extent = new PixelExtent(getDisplay()
                .getTempChangeBackground().getRectangle());
        target.setupClippingPlane(extent);

        // draw something on the temp change window
        drawTempChangeLine(target, world, sp.getAnalysisData(),
                (sp24 == null ? null : sp24.getAnalysisData()), color);
        target.clearClippingPlane();
    }

    /**
     * Draw the vertical velocity line
     * 
     * @param target
     * @param world
     * @param vs
     * @param color
     * @throws VizException
     */
    protected void drawVerticalVelocityLine(IGraphicsTarget target,
            WGraphics world, VerticalSounding vs, RGB color)
            throws VizException {

        // create some bogus omega for testing
        // Random rand = new Random();
        // float omega = 2 * rand.nextFloat() - 1;
        // for (SoundingLayer layer : vs) {
        // layer.setOmega(omega);
        // omega += 2 * rand.nextDouble() - 1;
        // if (Math.abs(omega) > 5) {
        // omega = 5 * Math.signum(omega);
        // }
        // }

        // get the depictable range variables
        double pMax = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double pMin = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;

        // First we are going to plot the vertical velocity on the temp change
        // background.
        LineStyle lineStyle = LineStyle.DASHED;
        Coordinate c0 = null;
        double opmin = pMin /*- 50 */;
        boolean found = false;
        for (SoundingLayer layer : vs) {
            // only include data up to top of 24Hr pressure hgt (e.g., 300 mb)
            if (layer.getPressure() < opmin || layer.getPressure() > pMax) {
                continue;
            }
            if (layer.getOmega() == SoundingLayer.MISSING) {
                continue;
            }
            double ubarps = -layer.getOmega() * 10;
            double otrns = ubarps > 0 ? ubarps : -ubarps;
            if (otrns > 50) {
                otrns = 15;
            } else if (otrns > 15) {
                otrns = 10.0 + 5.0 * (otrns - 15) / 35;
            } else if (otrns > 5) {
                otrns = 5.0 + 5.0 * (otrns - 5) / 10;
            }
            if (ubarps < 0) {
                otrns = -otrns;
            }

            Coordinate c1 = new Coordinate(world.mapX(otrns), world.mapY(WxMath
                    .getSkewTXY(layer.getPressure(), 0).y));

            if (c0 != null) {
                target.drawLine(c0.x, c0.y, 0, c1.x, c1.y, 0, color, 1,
                        lineStyle);
            }
            c0 = c1;

            found = true;
        }

        if (found) {
            target.clearClippingPlane();
            double y = world.mapY(WxMath.getSkewTXY(opmin, 0).y);

            RGB clr = SkewTConstants.backgroundColor;
            double x = world.mapX(-15);
            target.drawString(smallFont, "-50", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);

            x = world.mapX(-10);
            target.drawString(smallFont, "-15", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);
            x = world.mapX(-5);
            target.drawString(smallFont, "-5", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);
            x = world.mapX(15);
            target.drawString(smallFont, "+50", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);
            x = world.mapX(10.0);
            target.drawString(smallFont, "+15", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);
            x = world.mapX(5);
            target.drawString(smallFont, "+5", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);
            x = world.mapX(0.0);
            target.drawString(smallFont, "0", x, y, 0.0, TextStyle.BLANKED,
                    clr, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                    0.0);

            y -= 2 * target.getStringBounds(smallFont, "H").getHeight();
            target.drawString(smallFont, "Omega (-ubars/s)", x, y, 0.0,
                    TextStyle.BLANKED, clr, HorizontalAlignment.CENTER,
                    VerticalAlignment.BOTTOM, 0.0);
        }

    }

    /**
     * Draw the 24 hour temp change line
     * 
     * @param points
     * @throws VizException
     */
    protected void drawTempChangeLine(IGraphicsTarget target, WGraphics world,
            VerticalSounding vs, VerticalSounding vs24, RGB color)
            throws VizException {

        // get the depictable range variables
        double pMax = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmax())).y;
        double pMin = WxMath.reverseSkewTXY(new Coordinate(0, world
                .getWorldYmin())).y;
        double deltaT = world.getWorldXmax();

        // don't draw the temp profile if no data or only data is above 100 mb
        if (vs.size() <= 1 || vs24 == null || vs24.size() <= 1) {
            return;
        }
        if ((vs.getMaxPressurelayer().getPressure() < pMin)
                || (vs24.getMaxPressurelayer().getPressure() < pMin)) {
            return;
        }

        // build the sequence of delta T's through linear interpolation of the
        // temperatures between the current and previous 24-hour temp profiles
        LineStyle lineStyle = LineStyle.SOLID;
        Coordinate c0 = null;
        double bestdz, bestdT;
        for (SoundingLayer layer : vs) {
            // only include data up to top of 24Hr pressure hgt (e.g., 300 mb)
            if (layer.getPressure() < pMin || layer.getPressure() > pMax) {
                continue;
            }

            // assume +/- 20m is good enough
            bestdz = 20;
            bestdT = 1e37;
            for (SoundingLayer oldLayer : vs24) {
                if (layer.getGeoHeight() == SoundingLayer.MISSING
                        || oldLayer.getGeoHeight() == SoundingLayer.MISSING
                        || layer.getTemperature() == SoundingLayer.MISSING
                        || oldLayer.getTemperature() == SoundingLayer.MISSING) {
                    continue;
                }
                double dz = oldLayer.getGeoHeight() - layer.getGeoHeight();
                if (dz > bestdz) {
                    break;
                }
                if (dz < 0) {
                    dz = -dz;
                }
                if (dz > bestdz) {
                    continue;
                }
                bestdz = dz;
                bestdT = layer.getTemperature() - oldLayer.getTemperature();
            }
            if (bestdT > 1e36) {
                continue;
            }

            // case where deltaT is out of bounds, neg or pos
            if (bestdT < -deltaT) {
                bestdT = -deltaT;
            } else if (bestdT > deltaT) {
                bestdT = deltaT;
            }

            Coordinate c1 = new Coordinate(world.mapX(bestdT), world
                    .mapY(WxMath.getSkewTXY(layer.getPressure(), 0).y));

            if (c0 != null) {
                target.drawLine(c0.x, c0.y, 0, c1.x, c1.y, 0, color, 1,
                        lineStyle);
            }
            c0 = c1;
        }

    }

    private String getStormRelativeMotionString(SoundingParams sp) {
        WindComp helicity = sp.helicity();

        if (helicity == null
                || helicity.getStormMotionDir() >= SoundingLayer.NODATA
                || helicity.getStormMotionSpd() >= SoundingLayer.NODATA) {
            return "N/A";
        }
        float stormDir = helicity.getStormMotionDir();
        float stormSpd = helicity.getStormMotionSpd();
        return String.format("STORM MOTION %d%c AT %d kts", (int) stormDir,
                degree, (int) stormSpd);
    }

    private String getStormRelativeHelicityString(SoundingParams sp) {
        WindComp helicity = sp.helicity();

        if (helicity == null
                || helicity.getStormRelativeHelicity() >= SoundingLayer.NODATA) {
            return "N/A";
        }

        float _SRelHelicity = helicity.getStormRelativeHelicity();
        return String.format("STORM RELATIVE HELICITY= %d m%c/s%c",
                (int) _SRelHelicity, squared, squared);
    }

    @Override
    public String inspect(ReferencedCoordinate rCoord) throws VizException {
        String s = null;

        SoundingParams sp = getSoundingParameters();
        if (sp == null) {
            return s;
        }

        s = "NO DATA";
        VerticalSounding vs = sp.getAnalysisData();
        if (vs == null) {
            return s;
        }
        try {
            Coordinate c = rCoord.getObject();
            if (this.getDisplay().getSkewTBackground().contains(c)) {
                c = WxMath.reverseSkewTXY(getSkewTWorld().unMap(c.x, c.y));
            } else if (getDisplay().getTempChangeBackground().contains(c)) {
                c = getTempChangeWorld().unMap(c.x, c.y);
                c.y = WxMath.reverseSkewTXY(c).y;
            } else if (this.getDisplay().getHodoBackground().contains(c)) {
                c = this.getDisplay().getHodoBackground().getWorld().unMap(c.x,
                        c.y);
                Coordinate cc = getWindfromSounding(c);
                SoundingLayer sl = new SoundingLayer();
                if (cc != null) {
                    sl = windMap.get(cc);
                    c.y = sl.getPressure();
                } else {
                    return s;
                }
            } else {
                return s;
            }

            float p_mb = (float) c.y;
            float p_sfc = vs.getMaxPressurelayer().getPressure();

            if (p_mb > p_sfc) {
                return s;
            }

            double z_m = vs.interpolateValue(p_mb, DATA_TYPE.GEO_HEIGHT);
            double z_ft = (z_m == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : metersToFeet.convert(z_m);
            double t_C = (vs.interpolateValue(p_mb, DATA_TYPE.TEMPERATURE) == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : kelvinToCelsius.convert(vs.interpolateValue(p_mb,
                            DATA_TYPE.TEMPERATURE));
            double t_F = (t_C == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : celciusToFahrenheit.convert(t_C);
            double td_C = (vs.interpolateValue(p_mb, DATA_TYPE.DEWPOINT) == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : kelvinToCelsius.convert(vs.interpolateValue(p_mb,
                            DATA_TYPE.DEWPOINT));
            double td_F = (td_C == SoundingLayer.MISSING) ? SoundingLayer.MISSING
                    : celciusToFahrenheit.convert(td_C);
            float u_ms = vs.interpolateValue(p_mb, DATA_TYPE.WIND_U);
            float v_ms = vs.interpolateValue(p_mb, DATA_TYPE.WIND_V);
            Coordinate sd = new Coordinate(SoundingLayer.MISSING,
                    SoundingLayer.MISSING);
            double dir = SoundingLayer.MISSING;
            double kts = SoundingLayer.MISSING;
            if (u_ms != SoundingLayer.MISSING && v_ms != SoundingLayer.MISSING) {
                sd = WxMath.speedDir(u_ms, v_ms);
                dir = sd.y;
                kts = metersPerSecondToKnots.convert(sd.x);
            }
            double theta = SoundingLayer.MISSING;
            double thetaE = SoundingLayer.MISSING;
            double w = SoundingLayer.MISSING;
            if (td_C != SoundingLayer.MISSING) {
                theta = celciusToKelvin.convert(WxMath.theta(p_mb, t_C, 1000));
                thetaE = celciusToKelvin
                        .convert(WxMath.thetae(p_mb, t_C, td_C));
                w = WxMath.mixingRatio(p_mb, t_C);
            }
            s = String.format("P=%.0fmb z=%.0fm/%.0fft\n", p_mb, z_m, z_ft);
            s += String.format(
                    "T=%.0f%cC/%.0f%cF Td=%.0f%cC/%.0f%cF %03.0f@%.0fkts\n",
                    t_C, SkewTConstants.DEGREE_SYMBOL, t_F,
                    SkewTConstants.DEGREE_SYMBOL, td_C,
                    SkewTConstants.DEGREE_SYMBOL, td_F,
                    SkewTConstants.DEGREE_SYMBOL, dir, kts);
            s += String.format(
                    "u=%.0fm/s v=%.0fm/s Theta=%.0fK Theta-e=%.0fK w=%.1f",
                    u_ms, v_ms, theta, thetaE, w);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception translating coordinate", e);
        }

        return s;
    }

    /**
     * Draw an edit handle
     * 
     * @param target
     * @param world
     * @param zoomLevel
     * @param x
     * @param y
     * @param color
     * @throws VizException
     */
    public void drawEditHandle(IGraphicsTarget target, WGraphics world,
            double zoomLevel, double x, double y, RGB color)
            throws VizException {
        double radius = SkewTConstants.endpointRadius * zoomLevel;

        target.drawShadedRect(new PixelExtent(x - radius, x + radius, y
                - radius, y + radius), color, 1.0, null);
    }

    public void addListener(ISkewTDataChangedListener listener) {
        listenerList.add(listener);
    }

    public void removeListener(ISkewTDataChangedListener listener) {
        listenerList.remove(listener);
    }

    private void fireListeners() {
        for (Object listener : listenerList.getListeners()) {
            ((ISkewTDataChangedListener) listener).skewTResourceChanged(this);
        }
    }

    public static interface ISkewTDataChangedListener {
        public abstract void skewTResourceChanged(SkewTResource rsc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource#getInsetMapLocation()
     */
    @Override
    public Geometry getInsetMapLocation() {
        if (getLocation() != null) {
            return IInsetMapResource.factory.createPoint(getLocation());
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource#paintInsetMap(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * com.raytheon.uf.viz.core.map.MapDescriptor)
     */
    @Override
    public void paintInsetMap(IGraphicsTarget target,
            PaintProperties paintProps, MapDescriptor insetMapDescriptor)
            throws VizException {
        // paint a point
        if (point == null) {
            Coordinate lp = getLocation();
            if (lp != null) {
                point = new PointRenderable(getLocation(), getCapability(
                        ColorableCapability.class).getColor(),
                        insetMapDescriptor);
            } else {
                return;
            }
        } else {
            point.setColor(getCapability(ColorableCapability.class).getColor());
        }
        point.paint(target, paintProps);
    }
}
