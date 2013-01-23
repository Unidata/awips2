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

/**
 * This package is associated with the classes that describe the Color bar at the
 * top of the Spatial Editor window.
 */
package com.raytheon.viz.gfe.rsc.colorbar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IColorMapParametersListener;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.colortable.ColorEntry;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.DiscreteDisplayUtil;
import com.raytheon.viz.gfe.ui.GfeUiUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a colorbar for continuous (scalar and vector) elements
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/23/2008              dfitch      Initial Creation.
 * Aug 20, 2008            dglazesk    Updated for the new ColorMap interface
 * Aug 20, 2012      1079  randerso    Changed to display all discrete values for
 *                                     non-overlapping discretes
 * Jan 9, 2013  15661      ryu         Set font for drawing regular Wx/discrete parm labels.
 * Jan 10, 2013  15548     ryu         Update colorbar when new discrete colormap is selected
 *
 * </pre>
 *
 * @author chammack
 * @version 1.0
 */
public class DiscreteColorbar implements IColorBarDisplay,
        IGridDataChangedListener, IColorMapParametersListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteColorbar.class);

    private final Parm parm;

    private final GFEColorbarResource colorbarResource;

    private static float red[] = { 255, 200, 100, 0, 0, 0, 0, 0, 0 };

    private static float green[] = { 0, 0, 0, 100, 200, 255, 0, 0, 0 };

    private static float blue[] = { 0, 0, 0, 0, 0, 0, 100, 200, 255 };

    private static ColorMap fallbackColorMap = new ColorMap("Discrete", red,
            green, blue);

    private DataTime lastTime;

    private RGB seColorBarTickColor = new RGB(255, 255, 255);

    private RGB seColorBarTextColor = new RGB(255, 255, 255);

    private RGB seColorBarFgPickupColor = new RGB(255, 255, 255);

    private RGB seColorBarFgWxPickupColor = new RGB(255, 255, 255);

    private RGB seColorBarBgWxPickupColor;

    private List<WxValue> gridKeys;

    private List<ColorEntry> colorTable;

    private boolean lastIscMode;

    private ColorMapParameters cmParams;

    /**
     * Constructor for the Discrete Color Bar
     *
     * @param parm
     *            The parm
     * @param colorbarResource
     *            The colorbar resource
     */
    public DiscreteColorbar(Parm parm, GFEColorbarResource colorbarResource) {
        this.parm = parm;
        this.colorbarResource = colorbarResource;

        this.colorTable = Collections.emptyList();
        this.gridKeys = new ArrayList<WxValue>();
        this.lastIscMode = parm.getDataManager().getParmManager().iscMode();

        parm.getListeners().addGridChangedListener(this);
        cmParams = getColorMapParameters();
        cmParams.addListener(this);
    }

    private ColorMapParameters getColorMapParameters() {
        DataManager dataManager = parm.getDataManager();
        ISpatialDisplayManager spatialDisplayManager = dataManager
                .getSpatialDisplayManager();
        ResourcePair resourcePair = spatialDisplayManager
                .getResourcePair(parm);
        AbstractVizResource<?, ?> resource = resourcePair.getResource();
        ColorMapParameters params = resource.getCapability(
                ColorMapCapability.class).getColorMapParameters();
        return params;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.gfe.rsc.colorbar.IColorBarDisplay#dispose()
     */
    @Override
    public void dispose() {
        parm.getListeners().removeGridChangedListener(this);
        cmParams.removeListener(this);
    }

    @Override
    public void gridDataChanged(ParmID parm, TimeRange validTime) {
        lastTime = null;
    }

    @Override
    public void colorMapChanged() {
        lastTime = null;
    }

    /**
     * Gets the Discrete Color map.
     *
     * @return Returns the color map used for the discrete data.
     */
    public static ColorMap getFallbackColorMap() {
        return fallbackColorMap;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (parm == null) {
            return;
        }

        DataTime currentTime = paintProps.getDataTime();
        IGridData gridData = parm.overlappingGrid(currentTime.getRefTime());
        boolean currentIscMode = parm.getDataManager().getParmManager()
                .iscMode();

        if ((!currentTime.equals(lastTime)) || (lastIscMode != currentIscMode)) {
            lastTime = currentTime;
            lastIscMode = currentIscMode;

            IGridData iscGridData = null;
            if (parm.getDataManager().getParmManager().iscMode()) {
                GridID gid = new GridID(parm, currentTime.getRefTime());
                if (parm.getGridInfo().getGridType().equals(GridType.WEATHER)) {
                    WeatherGridSlice slice = new WeatherGridSlice();
                    parm.getDataManager().getIscDataAccess()
                            .getCompositeGrid(gid, true, slice);
                    if (slice.getKeys().length == 0) {
                        return;
                    }
                    iscGridData = new WeatherGridData(this.parm, slice);
                } else if (parm.getGridInfo().getGridType()
                        .equals(GridType.DISCRETE)) {
                    DiscreteGridSlice slice = new DiscreteGridSlice();
                    parm.getDataManager().getIscDataAccess()
                            .getCompositeGrid(gid, true, slice);
                    if (slice.getKey().length == 0) {
                        return;
                    }
                    iscGridData = new DiscreteGridData(this.parm, slice);
                } else {
                    return;
                }
            }

            // compute the color table for the displayed grid
            colorTable = gridColorTable(gridData, iscGridData);

            gridKeys.clear();
            for (ColorEntry entry : colorTable) {
                gridKeys.add(entry.getValue());
            }
        }

        IMapDescriptor descriptor = colorbarResource.getMapDescriptor();
        drawDiscreteBar(target, colorbarResource.getExtent(), descriptor,
                colorTable);

        double ratio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        drawLabelStrings(target, colorTable, colorbarResource.getExtent(),
                ratio);

    }

    private List<ColorEntry> gridColorTable(IGridData gridData,
            IGridData iscGridData) {
        List<ColorEntry> primaryValues = calcGridColorTable(gridData);
        if (iscGridData == null) {
            return primaryValues;
        }

        List<ColorEntry> iscValues = calcGridColorTable(iscGridData);

        // add unique entries in iscValues to primaryValues
        for (int i = 0; i < iscValues.size(); i++) {
            boolean found = false;
            WxValue key = iscValues.get(i).getValue();
            for (int j = 0; j < primaryValues.size(); j++) {
                if (primaryValues.get(j).getValue().toString()
                        .equals(key.toString())) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                primaryValues.add(new ColorEntry(key, iscValues.get(i)
                        .getAttributes()));
            }
        }
        return primaryValues;
    }

    private List<ColorEntry> calcGridColorTable(IGridData gridData) {
        List<ColorEntry> cEntries = new ArrayList<ColorEntry>();
        if (gridData == null) {
            return cEntries;
        }

        GridType gridType = parm.getGridInfo().getGridType();
        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();
        String compName = parmId.getCompositeName();

        // special case: discrete non-overlapping, use all keys
        if (gridType.equals(GridType.DISCRETE)
                && !DiscreteKey.discreteDefinition(siteId).overlaps(compName)) {
            List<String> dkeys = DiscreteKey.discreteDefinition(siteId)
                    .symbols(compName);
            for (String key : dkeys) {
                DiscreteKey dk = new DiscreteKey(siteId, key, parmId);
                WxValue v = new DiscreteWxValue(dk, parm);
                List<ImageAttr> attrs = DiscreteDisplayUtil
                        .getFillAttributes(v);
                cEntries.add(new ColorEntry(v, attrs));
            }
            return cEntries;
        }

        // get the grid slice for the grid data
        IGridSlice gs = gridData.getGridSlice();

        // get the grid for this gridSlice
        Grid2DByte grid = (gridType.equals(GridType.WEATHER) ? ((WeatherGridSlice) gs)
                .getWeatherGrid() : ((DiscreteGridSlice) gs).getDiscreteGrid());

        // go through the Grid2D and make a byte array[256] to indicate which
        // point are on
        boolean[] cArray = new boolean[256];
        // initialize the array to all false
        Arrays.fill(cArray, false);

        Grid2DBit dspMask = parm.getDisplayAttributes().getDisplayMask();

        for (int i = 0; i < grid.getXdim(); i++) {
            for (int j = 0; j < grid.getYdim(); j++) {
                if (dspMask.getAsBoolean(i, j)) {
                    cArray[grid.get(i, j)] = true;
                }
            }
        }

        // make a seq of WxValue using the values set as on
        List<WxValue> gridWValues = new ArrayList<WxValue>();

        // for WEATHER
        if (gridType.equals(GridType.WEATHER)) {
            WeatherKey[] wKeys = ((WeatherGridSlice) gs).getKeys();
            for (int i = 0; i < 256; i++) {
                // extract the WxValue corresponding to the bytes which are on
                // (this extraction is done since not all the keys for a
                // GridSlice are displayed on the ColorBar - only the ones which
                // are used)
                if (cArray[i]) {
                    gridWValues.add(new WeatherWxValue(wKeys[i], parm));
                }
            }
        }
        // for DISCRETE
        else {
            DiscreteKey[] dKeys = ((DiscreteGridSlice) gs).getKey();
            for (int i = 0; i < 256; i++) {
                if (cArray[i]) {
                    gridWValues.add(new DiscreteWxValue(dKeys[i], parm));
                }
            }
        }

        // map each WxValue and append ColorEntry to list
        for (WxValue wxValue : gridWValues) {
            List<ImageAttr> attrs = DiscreteDisplayUtil
                    .getFillAttributes(wxValue);
            cEntries.add(new ColorEntry(wxValue, attrs));
        }

        // add in any additional required color bar values, format of config
        // entry is parmNameAndLevel_AdditionalColorBatLabels.
        String cn = parmId.compositeNameUI() + "_AdditionalColorBarLabels";
        String[] additional = Activator.getDefault().getPreferenceStore()
                .getStringArray(cn);
        for (int i = 0; i < additional.length; i++) {
            WxValue wx;
            if (gridType.equals(GridType.WEATHER)) {
                WeatherKey key = new WeatherKey(siteId, additional[i]);
                if (!key.isValid()) {
                    statusHandler.handle(Priority.PROBLEM, "Key ["
                            + additional[i] + "] not valid in " + cn
                            + " configuration");
                    continue;
                }
                wx = new WeatherWxValue(key, parm);
            } else {
                DiscreteKey key = new DiscreteKey(siteId, additional[i], parmId);
                if (!key.isValid()) {
                    statusHandler.handle(Priority.PROBLEM, "Key ["
                            + additional[i] + "] not valid in " + cn
                            + " configuration");
                    continue;
                }
                wx = new DiscreteWxValue(key, parm);
            }
            boolean found = false;
            for (ColorEntry cEntry : cEntries) {
                if (cEntry.getValue().equals(wx)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                List<ImageAttr> attrs = DiscreteDisplayUtil
                        .getFillAttributes(wx);
                cEntries.add(new ColorEntry(wx, attrs));
            }
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
    private void drawLabelStrings(IGraphicsTarget target,
            List<ColorEntry> colorTable, PixelExtent pe, double xScaleFactor)
            throws VizException {
        double labelLoc = 0.0f;

        // Draw label strings on the bars
        double maxX = pe.getMaxX();
        double minX = pe.getMinX();
        double maxY = pe.getMaxY();
        double minY = pe.getMinY();
        double keywidth = (maxX - minX) / colorTable.size();

        WxValue pickupValue = parm.getParmState().getPickUpValue();

        // Going to draw our pickup label first. Once we know where it belongs,
        // we can prevent any labels that would be overlapped by it from
        // drawing.
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
            dstring.font = colorbarResource.getPickupValueFont();

            double pickupLabelSize = target.getStringsBounds(dstring)
                    .getWidth();
            double pickupLabelDrawPoint = (minX + (pickupIndex * keywidth) + keywidth / 2)
                    * xScaleFactor;
            pickupLabelMinX = pickupLabelDrawPoint - (pickupLabelSize / 2);
            pickupLabelMaxX = pickupLabelDrawPoint + (pickupLabelSize / 2);
            HorizontalAlignment pickupValueAlignment = HorizontalAlignment.CENTER;

            // Ensure we have not run off the edge of our screen. If so, slide
            // the drawing point for our pickup label accordingly.
            if (pickupLabelMinX < (minX * xScaleFactor)) {
                pickupLabelDrawPoint = (minX + (pickupIndex * keywidth))
                        * xScaleFactor;
                pickupLabelMinX = pickupLabelDrawPoint;
                pickupLabelMaxX = pickupLabelDrawPoint + pickupLabelSize;
                pickupValueAlignment = HorizontalAlignment.LEFT;
            } else if (pickupLabelMaxX > (maxX * xScaleFactor)) {
                pickupLabelDrawPoint = (minX + (pickupIndex * keywidth) + keywidth)
                        * xScaleFactor;
                pickupLabelMinX = pickupLabelDrawPoint - pickupLabelSize;
                pickupLabelMaxX = pickupLabelDrawPoint;
                pickupValueAlignment = HorizontalAlignment.RIGHT;
            }

            dstring.setCoordinates(pickupLabelDrawPoint / xScaleFactor, center);
            dstring.textStyle = TextStyle.BOXED;
            dstring.horizontalAlignment = pickupValueAlignment;
            dstring.verticallAlignment = VerticalAlignment.MIDDLE;

            // draw once for the box
            target.drawStrings(dstring);
            // and again for shadowed text in the box
            dstring.shadowColor = seColorBarBgWxPickupColor;
            dstring.textStyle = TextStyle.DROP_SHADOW;
            target.drawStrings(dstring);
        } else {
            pickupLabelMinX = Double.NEGATIVE_INFINITY;
            pickupLabelMaxX = Double.NEGATIVE_INFINITY;
        }

        DrawableString dstring = new DrawableString("", seColorBarTextColor);
        dstring.textStyle = TextStyle.NORMAL;
        dstring.horizontalAlignment = HorizontalAlignment.CENTER;
        dstring.verticallAlignment = VerticalAlignment.MIDDLE;

        i = 0;
        for (ColorEntry colorEntry : colorTable) {
            double ikeywidth = i * keywidth;
            target.drawLine(minX + ikeywidth, minY, 0.0, minX + ikeywidth,
                    maxY, 0.0, seColorBarTickColor, 1.0f);

            String keyName = colorEntry.getValue().toString();
            labelLoc = (float) (minX + ikeywidth) + (float) keywidth / 2;

            double boxMinXCoord = (minX + ikeywidth) * xScaleFactor;
            double boxMaxXCoord = boxMinXCoord + (keywidth * xScaleFactor);

            if (i != pickupIndex) {
                if (!GFEColorbarResource.isWithin(boxMinXCoord, boxMaxXCoord,
                        pickupLabelMinX, pickupLabelMaxX)) {
                    String truncatedLabel = GfeUiUtil.truncateLabelToFit(
                            target, colorbarResource.getColorbarWxLabelFont(),
                            keyName, (int) Math.floor(keywidth * xScaleFactor),
                            true);
                    dstring.setText(truncatedLabel, seColorBarTextColor);
                    dstring.setCoordinates(labelLoc, center);
                    dstring.font = colorbarResource.getColorbarWxLabelFont();
                    target.drawStrings(dstring);
                }
            }
            i++;
        }
    }

    /**
     * Draws the colorbar once colors and patterns have been decided.
     *
     * @param target
     *            The graphics target on which to draw.
     * @param pixelExtent
     *            The extent (in pixel space) of the colorbar.
     * @param mapDescriptor
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
    private void drawDiscreteBar(IGraphicsTarget target,
            PixelExtent pixelExtent, IMapDescriptor mapDescriptor,
            List<ColorEntry> colorTable) throws VizException {

        double x1 = pixelExtent.getMinX();
        double x2 = pixelExtent.getMaxX();
        double y1 = pixelExtent.getMinY();
        double y2 = pixelExtent.getMaxY();

        double keywidth = (x2 - x1) / colorTable.size();

        int x = 0;
        for (ColorEntry colorEntry : colorTable) {
            // calculate the bounding box from x
            PixelExtent pe = new PixelExtent(x1 + x * keywidth, x1 + (x + 1)
                    * keywidth, y1, y2);

            Coordinate[] coordinates = new Coordinate[5];
            coordinates[0] = new Coordinate(x1 + x * keywidth, y1);
            coordinates[1] = new Coordinate(x1 + (x + 1) * keywidth, y1);
            coordinates[2] = new Coordinate(x1 + (x + 1) * keywidth, y2);
            coordinates[3] = new Coordinate(x1 + x * keywidth, y2);
            coordinates[4] = new Coordinate(x1 + x * keywidth, y1);

            // draw the shaded rectangle with the correct color(s) and
            // pattern(s)
            List<ImageAttr> attrs = colorEntry.getAttributes();
            for (ImageAttr attr : attrs) {
                RGB color = RGBColors.getRGBColor(attr.getColorName());
                byte[] pattern = FillPatterns.getGLPattern(attr
                        .getFillPatternName());
                target.drawShadedRect(pe, color, 1.0, pattern);
            }

            // draw the outline
            target.drawRect(pe, GFEColorbarResource.COLORBAR_GRAY, 1, 1.0);
            x++;
        }
    }

    /**
     * @param seColorBarTickColor
     *            the seColorBarTickColor to set
     */
    public void setSeColorBarTickColor(RGB seColorBarTickColor) {
        this.seColorBarTickColor = seColorBarTickColor;
    }

    /**
     * @param seColorBarTextColor
     *            the seColorBarTextColor to set
     */
    public void setSeColorBarTextColor(RGB seColorBarTextColor) {
        this.seColorBarTextColor = seColorBarTextColor;
    }

    /**
     * @param seColorBarFgPickupColor
     *            the seColorBarFgPickupColor to set
     */
    public void setSeColorBarFgPickupColor(RGB seColorBarFgPickupColor) {
        this.seColorBarFgPickupColor = seColorBarFgPickupColor;
    }

    /**
     * @param seColorBarFgWxPickupColor
     *            the seColorBarFgWxPickupColor to set
     */
    public void setSeColorBarFgWxPickupColor(RGB seColorBarFgWxPickupColor) {
        this.seColorBarFgWxPickupColor = seColorBarFgWxPickupColor;
    }

    /**
     * @param seColorBarBgWxPickupColor
     *            the seColorBarBgWxPickupColor to set
     */
    public void setSeColorBarBgWxPickupColor(RGB seColorBarBgWxPickupColor) {
        this.seColorBarBgWxPickupColor = seColorBarBgWxPickupColor;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.gfe.rsc.colorbar.IColorBarDisplay#getValueAt(double[],
     * int)
     */
    @Override
    public WxValue getValueAt(double[] coord, int mouseButton) {
        PixelExtent lastExtent = colorbarResource.getExtent();
        float fractionX = (float) ((coord[0] - lastExtent.getMinX()) / (lastExtent
                .getMaxX() - lastExtent.getMinX()));
        int index = (int) (gridKeys.size() * fractionX);
        if (index >= gridKeys.size()) {
            index = gridKeys.size() - 1;
        }

        switch (parm.getGridInfo().getGridType()) {
        case DISCRETE: {
            DiscreteWxValue castedVal = (DiscreteWxValue) gridKeys.get(index);
            return new DiscreteWxValue(castedVal.getDiscreteKey(), parm);
        }
        case WEATHER: {
            WeatherWxValue castedVal = (WeatherWxValue) gridKeys.get(index);
            return new WeatherWxValue(castedVal.getWeatherKey(), parm);
        }
        default:
            throw new IllegalArgumentException(
                    "getValueAt does not support type: "
                            + parm.getGridInfo().getGridType());
        }
    }
}
