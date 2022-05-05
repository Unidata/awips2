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
package com.raytheon.uf.viz.d2d.gfe.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.gfe.colortable.ColorEntry;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.colortable.DiscreteColorTable;
import com.raytheon.viz.gfe.colortable.WeatherColorTable;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.rsc.DiscreteDisplayUtil;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.rsc.colorbar.IColorBarDisplay;
import com.raytheon.viz.gfe.ui.GfeUiUtil;

/**
 *
 * D2D version of GFEColorbarResource used for displaying discrete and weather
 * products in D2D.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 02, 2019  71896    tjensen   Initial creation
 * Dec 13, 2019  72475    tjensen   Refactor to reduce dependency on GFE and
 *                                  closer to D2DColorbarResource
 *
 * </pre>
 *
 * @author tjensen
 */
public class D2DDiscreteColorbarResource
        extends AbstractVizResource<GFEGridResourceData, IMapDescriptor> {

    private static final double HEIGHT = 25.0;

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DDiscreteColorbarResource.class);

    private static final RGB COLORBAR_GRAY = new RGB(192, 192, 192);

    private PixelExtent lastExtent;

    // the currently displayed parm
    protected Parm currentParm;

    private IFont colorbarScaleFont;

    private IFont colorbarWxLabelFont;

    private IFont pickupFont;

    protected IColorBarDisplay colorbarDisplay;

    protected RGB seColorBarTickColor;

    protected RGB seColorBarTextColor;

    protected RGB seColorBarFgPickupColor;

    protected RGB seColorBarBgPickupColor;

    protected RGB seColorBarFgWxPickupColor;

    protected RGB seColorBarBgWxPickupColor;

    protected Set<ParmID> fittedParms;

    private DataTime lastTime;

    private List<ColorEntry> colorTable;

    private final List<WxValue> gridKeys;

    private Parm parm;

    private DiscreteColorTable discreteColorTable;

    private final WeatherColorTable weatherColorTable = new WeatherColorTable();

    public D2DDiscreteColorbarResource(
            GFEGridResourceData gfeGridResourceData) {

        super(gfeGridResourceData, new LoadProperties());
        fittedParms = new HashSet<>();
        this.gridKeys = new ArrayList<>();

        seColorBarTickColor = RGBColors.getRGBColor("white");
        seColorBarTextColor = RGBColors.getRGBColor("white");
        seColorBarFgPickupColor = RGBColors.getRGBColor("white");
        seColorBarBgPickupColor = RGBColors.getRGBColor("black");
        seColorBarFgWxPickupColor = RGBColors.getRGBColor("white");
        seColorBarBgWxPickupColor = RGBColors.getRGBColor("purple");
    }

    @Override
    protected void disposeInternal() {

        if (colorbarScaleFont != null) {
            colorbarScaleFont.dispose();
        }

        if (colorbarWxLabelFont != null) {
            colorbarWxLabelFont.dispose();
        }

        if (pickupFont != null) {
            pickupFont.dispose();
        }

        if (colorbarDisplay != null) {
            colorbarDisplay.dispose();
        }
    }

    @Override
    public String getName() {
        return "D2D Discrete Colorbar Resource";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        colorbarScaleFont = GFEFonts.makeGFEIFont(target, "ColorBarScale_font",
                1);
        colorbarWxLabelFont = GFEFonts.makeGFEIFont(target,
                "ColorBarWxLabel_font", 2);
        pickupFont = GFEFonts.makeGFEIFont(target, "ColorBarPickUp_font", 3);
    }

    public void initExtent(PaintProperties paintProps) {
        IExtent screenExtent = paintProps.getView().getExtent();
        // Construct a bar that is HEIGHT pixels high
        double height = (HEIGHT * screenExtent.getHeight())
                / paintProps.getCanvasBounds().height;
        PixelExtent pe = new PixelExtent(screenExtent.getMinX(),
                screenExtent.getMaxX(), screenExtent.getMinY(),
                screenExtent.getMinY() + height);

        // Set the extent of the colorbar back on the parent resource so it
        // knows the size for things like mouse actions
        lastExtent = pe;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        initExtent(paintProps);

        target.clearClippingPlane();
        ResourceList rl = descriptor.getResourceList();
        for (ResourcePair rp : rl) {
            paintResource(target, paintProps, rp);
        }
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    private void paintResource(IGraphicsTarget target,
            PaintProperties paintProps, ResourcePair rp) throws VizException {
        ResourceProperties props = rp.getProperties();
        AbstractVizResource<?, ?> rsc = rp.getResource();

        if (props.isSystemResource() || !props.isVisible() || rsc == null) {
            return;
        }

        if (rsc instanceof GFEGridResource) {
            GFEGridResource gfeGridRsc = (GFEGridResource) rsc;
            if (gfeGridRsc.isDiscreteColorbarUser()) {
                paintColorBar(target, paintProps, gfeGridRsc);
            }
        }
    }

    public void paintColorBar(IGraphicsTarget target,
            PaintProperties paintProps, GFEGridResource gfeGridRsc)
            throws VizException {
        parm = gfeGridRsc.getParm();

        DataTime currentTime = paintProps.getFramesInfo()
                .getTimeForResource(gfeGridRsc);
        if ((parm == null) || (currentTime == null)) {
            return;
        }

        IGridData gridData = parm.overlappingGrid(currentTime.getRefTime());
        GridType gridType = parm.getGridInfo().getGridType();
        boolean isWeather = false;
        if (gridType.equals(GridType.WEATHER)) {
            isWeather = true;
        } else if (!gridType.equals(GridType.DISCRETE)) {
            statusHandler.error(
                    "Unsupported grid data type using D2DDiscreteColorBarResource: "
                            + gridType);
            return;
        }

        if (!currentTime.equals(lastTime)) {
            lastTime = currentTime;

            if (!isWeather) {
                ColorMapParameters params = DiscreteDisplayUtil
                        .buildColorMapParameters(parm);
                IColorMap colorMap = params.getColorMap();
                discreteColorTable = new DiscreteColorTable(parm, colorMap);
            }

            // compute the color table for the displayed grid
            colorTable = gridColorTable(gridData, isWeather);

            gridKeys.clear();
            for (ColorEntry entry : colorTable) {
                gridKeys.add(entry.getValue());
            }
        }

        drawDiscreteBar(target);

        double ratio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        drawLabelStrings(target, ratio);

    }

    private List<ColorEntry> gridColorTable(IGridData gridData,
            boolean isWeather) {
        return calcGridColorTable(gridData, isWeather);
    }

    private List<ColorEntry> calcGridColorTable(IGridData gridData,
            boolean isWeather) {
        List<ColorEntry> cEntries = new ArrayList<>();
        if (gridData == null) {
            return cEntries;
        }

        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();
        String compName = parmId.getCompositeName();

        // special case: discrete non-overlapping, use all keys
        if (!isWeather
                && !DiscreteKey.discreteDefinition(siteId).overlaps(compName)) {
            List<String> dkeys = DiscreteKey.discreteDefinition(siteId)
                    .symbols(compName);
            for (String key : dkeys) {
                DiscreteKey dk = new DiscreteKey(siteId, key, parmId);
                WxValue v = new DiscreteWxValue(dk, parm);
                List<ImageAttr> attrs = getFillAttributes(v);
                cEntries.add(new ColorEntry(v, attrs));
            }
            return cEntries;
        }

        // get the grid for this gridSlice
        Grid2DByte grid = (isWeather
                ? ((WeatherGridData) gridData).getDataObject().getWeatherGrid()
                : ((DiscreteGridData) gridData).getDataObject()
                        .getDiscreteGrid());

        // go through the Grid2D and make a byte array[256] to indicate which
        // point are on
        boolean[] cArray = new boolean[256];

        // initialize the array to all false
        Arrays.fill(cArray, false);

        Grid2DBit dspMask = parm.getDisplayAttributes().getDisplayMask();

        for (int i = 0; i < grid.getXdim(); i++) {
            for (int j = 0; j < grid.getYdim(); j++) {
                if (dspMask.getAsBoolean(i, j)) {
                    cArray[0xFF & grid.get(i, j)] = true;
                }
            }
        }

        // make a seq of WxValue using the values set as on
        List<WxValue> gridWValues = new ArrayList<>();

        // for WEATHER
        if (isWeather) {
            WeatherKey[] wKeys = ((WeatherGridData) gridData).getDataObject()
                    .getKeys();
            for (int i = 0; i < 256; i++) {
                /*
                 * extract the WxValue corresponding to the bytes which are on
                 * (this extraction is done since not all the keys for a
                 * GridSlice are displayed on the ColorBar - only the ones which
                 * are used)
                 */
                if (cArray[i]) {
                    gridWValues.add(new WeatherWxValue(wKeys[i], parm));
                }
            }
        }
        // for DISCRETE
        else {
            DiscreteKey[] dKeys = ((DiscreteGridData) gridData).getDataObject()
                    .getKeys();
            for (int i = 0; i < 256; i++) {
                if (cArray[i]) {
                    gridWValues.add(new DiscreteWxValue(dKeys[i], parm));
                }
            }
        }

        // map each WxValue and append ColorEntry to list
        for (WxValue wxValue : gridWValues) {
            List<ImageAttr> attrs = getFillAttributes(wxValue);
            cEntries.add(new ColorEntry(wxValue, attrs));
        }

        return cEntries;
    }

    /**
     * Draws all of the labels within colorTable to the Discrete Colorbar.
     * Labels that do not fit their designated band on the bar will be
     * truncated. Pickup value text will always be displayed in full, so any
     * text it overlaps will not be drawn.
     *
     * @param target
     *            The graphics target on which to draw
     * @param colorTable
     *            list of color entries to label
     * @param pe
     *            The pixel space covered by the colorbar
     * @param xScaleFactor
     *            The conversion factor from drawing units to absolute pixels
     *            for the specified drawing target.
     * @throws VizException
     *             if target.drawLine() or target.drawString() fail
     */
    private void drawLabelStrings(IGraphicsTarget target, double xScaleFactor)
            throws VizException {
        double labelLoc = 0.0f;

        // Draw label strings on the bars
        double maxX = lastExtent.getMaxX();
        double minX = lastExtent.getMinX();
        double maxY = lastExtent.getMaxY();
        double minY = lastExtent.getMinY();
        double keywidth = (maxX - minX) / colorTable.size();

        WxValue pickupValue = parm.getParmState().getPickUpValue();

        /*
         * Going to draw our pickup label first. Once we know where it belongs,
         * we can prevent any labels that would be overlapped by it from
         * drawing.
         */
        int pickupIndex = -1;
        int i = 0;
        for (ColorEntry colorEntry : colorTable) {
            if (colorEntry.getValue().equals(pickupValue)) {
                pickupIndex = i;
                break;
            }
            i++;
        }

        double pickupLabelMinX = 0;
        double pickupLabelMaxX = 0;
        double center = (minY + maxY) / 2.0;

        if (pickupIndex != -1) {
            RGB pickupColor = null;
            if (parm.getGridInfo().getGridType() == GridType.WEATHER) {
                pickupColor = seColorBarFgWxPickupColor;
            } else {
                pickupColor = seColorBarFgPickupColor;
            }
            DrawableString dstring = new DrawableString(pickupValue.toString(),
                    pickupColor);
            dstring.font = pickupFont;

            double pickupLabelSize = target.getStringsBounds(dstring)
                    .getWidth();
            double pickupLabelDrawPoint = (minX + (pickupIndex * keywidth)
                    + (keywidth / 2)) * xScaleFactor;
            pickupLabelMinX = pickupLabelDrawPoint - (pickupLabelSize / 2);
            pickupLabelMaxX = pickupLabelDrawPoint + (pickupLabelSize / 2);
            HorizontalAlignment pickupValueAlignment = HorizontalAlignment.CENTER;

            /*
             * Ensure we have not run off the edge of our screen. If so, slide
             * the drawing point for our pickup label accordingly.
             */
            if (pickupLabelMinX < (minX * xScaleFactor)) {
                pickupLabelDrawPoint = (minX + (pickupIndex * keywidth))
                        * xScaleFactor;
                pickupLabelMinX = pickupLabelDrawPoint;
                pickupLabelMaxX = pickupLabelDrawPoint + pickupLabelSize;
                pickupValueAlignment = HorizontalAlignment.LEFT;
            } else if (pickupLabelMaxX > (maxX * xScaleFactor)) {
                pickupLabelDrawPoint = (minX + (pickupIndex * keywidth)
                        + keywidth) * xScaleFactor;
                pickupLabelMinX = pickupLabelDrawPoint - pickupLabelSize;
                pickupLabelMaxX = pickupLabelDrawPoint;
                pickupValueAlignment = HorizontalAlignment.RIGHT;
            }

            dstring.setCoordinates(pickupLabelDrawPoint / xScaleFactor, center);
            dstring.horizontalAlignment = pickupValueAlignment;
            dstring.verticallAlignment = VerticalAlignment.MIDDLE;
            dstring.addTextStyle(TextStyle.BLANKED);
            dstring.addTextStyle(TextStyle.BOXED);
            dstring.addTextStyle(TextStyle.DROP_SHADOW,
                    seColorBarBgWxPickupColor);
            target.drawStrings(dstring);
        } else {
            pickupLabelMinX = Double.NEGATIVE_INFINITY;
            pickupLabelMaxX = Double.NEGATIVE_INFINITY;
        }

        DrawableLine[] lines = new DrawableLine[colorTable.size()];
        List<DrawableString> strings = new ArrayList<>();
        i = 0;
        for (ColorEntry colorEntry : colorTable) {
            double ikeywidth = i * keywidth;
            lines[i] = new DrawableLine();
            lines[i].setCoordinates(minX + ikeywidth, minY);
            lines[i].addPoint(minX + ikeywidth, maxY);
            lines[i].basics.color = seColorBarTickColor;

            String keyName = colorEntry.getValue().toString();
            labelLoc = (float) (minX + ikeywidth) + ((float) keywidth / 2);

            double boxMinXCoord = (minX + ikeywidth) * xScaleFactor;
            double boxMaxXCoord = boxMinXCoord + (keywidth * xScaleFactor);

            if (i != pickupIndex) {
                if ((pickupLabelMinX > boxMaxXCoord)
                        || (pickupLabelMaxX < boxMinXCoord)) {
                    String truncatedLabel = GfeUiUtil.truncateLabelToFit(target,
                            colorbarWxLabelFont, keyName,
                            (int) Math.floor(keywidth * xScaleFactor), true);
                    DrawableString dstring = new DrawableString(truncatedLabel,
                            seColorBarTextColor);
                    dstring.setCoordinates(labelLoc, center);
                    dstring.font = colorbarWxLabelFont;
                    dstring.horizontalAlignment = HorizontalAlignment.CENTER;
                    dstring.verticallAlignment = VerticalAlignment.MIDDLE;
                    strings.add(dstring);
                }
            }
            i++;
        }
        target.drawLine(lines);
        target.drawStrings(strings);
    }

    /**
     * Draws the colorbar once colors and patterns have been decided.
     *
     * @param target
     *            The graphics target on which to draw.
     * @param lastExtent
     *            The extent (in pixel space) of the colorbar.
     * @param descriptor
     *            The current map descriptor.
     * @param discreteKeys
     *            The discrete keys that are actually used in the map.
     * @param keyColors
     *            A map from discrete keys to the colors that should be used to
     *            draw the key. Overlapping hazards will have more than one
     *            color.
     * @param keyPatterns
     *            A map of discrete keys to the patterns that should be used to
     *            draw the key. Overlapping hazards will have more than one
     *            pattern.
     */
    private void drawDiscreteBar(IGraphicsTarget target) throws VizException {

        double x1 = lastExtent.getMinX();
        double x2 = lastExtent.getMaxX();
        double y1 = lastExtent.getMinY();
        double y2 = lastExtent.getMaxY();

        double keywidth = (x2 - x1) / colorTable.size();

        int x = 0;
        for (ColorEntry colorEntry : colorTable) {
            // calculate the bounding box from x
            PixelExtent pe = new PixelExtent(x1 + (x * keywidth),
                    x1 + ((x + 1) * keywidth), y1, y2);

            Coordinate[] coordinates = new Coordinate[5];
            coordinates[0] = new Coordinate(x1 + (x * keywidth), y1);
            coordinates[1] = new Coordinate(x1 + ((x + 1) * keywidth), y1);
            coordinates[2] = new Coordinate(x1 + ((x + 1) * keywidth), y2);
            coordinates[3] = new Coordinate(x1 + (x * keywidth), y2);
            coordinates[4] = new Coordinate(x1 + (x * keywidth), y1);

            // draw the shaded rectangle with the correct color(s) and
            // pattern(s)
            List<ImageAttr> attrs = colorEntry.getAttributes();
            for (ImageAttr attr : attrs) {
                RGB color = RGBColors.getRGBColor(attr.getColorName());
                byte[] pattern = FillPatterns
                        .getGLPattern(attr.getFillPatternName());
                target.drawShadedRect(pe, color, 1.0, pattern);
            }

            // draw the outline
            target.drawRect(pe, COLORBAR_GRAY, 1, 1.0);
            x++;
        }
    }

    private List<ImageAttr> getFillAttributes(WxValue wxValue) {
        if (wxValue instanceof WeatherWxValue) {
            return weatherColorTable.map(wxValue);
        }

        if (wxValue instanceof DiscreteWxValue) {
            return discreteColorTable.map(wxValue);
        }

        throw new IllegalArgumentException(
                "WeatherWxValue or DiscreteWxValue expected, received: "
                        + wxValue.getClass().getSimpleName());
    }
}
