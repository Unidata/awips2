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
package com.raytheon.viz.mpe.ui.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.LabelEntry;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;
import com.raytheon.uf.viz.core.sampling.ISamplingResource;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.MPEInterrogationConstants;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.AvailableRadarGridType;
import com.raytheon.viz.mpe.ui.MPEFontFactory;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.ui.actions.MPELegendOverride;
import com.raytheon.viz.mpe.ui.actions.MPELegendOverride.OverrideType;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.InputAdapter;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * The MPE Legend Resource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 19, 2008           randerso  Initial creation
 * Dec 02, 2009  3237     snaples   Added options for 6/24 hour display
 *                                  filtering.
 * Feb 14, 2013  1616     bsteffen  Add option for interpolation of colormap
 *                                  parameters, disable colormap interpolation
 *                                  by default.
 * Feb 28, 2017  6157     bkowal    Override legend colors when filtering is
 *                                  initiated by the user.
 * Mar 20, 2017  6157     bkowal    No longer cast the temperature value to an
 *                                  Integer.
 * Mar 21, 2017  6157     bkowal    Always display the filtering menu items even
 *                                  if there are other items in the context
 *                                  menu.
 * Dec 13, 2018  6790     dgilling  Display all loaded resources in the legend.
 * Feb 27, 2019  7731     smanoj    Added null check to avoid paint error with
 *                                  MPE legends on the lower part of the screen.
 * Mar 11, 2019  7761     smanoj    Adding null pointer check to avoid UELE
 *                                  error.
 * Mar 27, 2019  7753     dgilling  Correct duplicated legend text from previous
 *                                  DR.
 * Jul 16, 2019  7103     randerso  Remove invalid setting of legend extent.
 *
 * </pre>
 *
 * @author randerso
 */
public class MPELegendResource
        extends AbstractLegendResource<GenericResourceData>
        implements IMpeResource, IContextMenuContributor, ISamplingResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPELegendResource.class);

    private static double eVal = 0;

    private ReferencedCoordinate sampleCoord = null;

    private final IInputHandler inspectHandler = new InputAdapter() {

        @Override
        public boolean handleMouseMove(int x, int y) {
            IDisplayPaneContainer container = getResourceContainer();
            Coordinate c = container.translateClick(x, y);
            if (c != null) {
                sampleCoord = new ReferencedCoordinate(c);
            } else {
                sampleCoord = null;
            }
            if (isSampling()) {
                issueRefresh();
            }
            return false;
        }

        @Override
        public boolean handleMouseExit(Event event) {
            sampleCoord = null;
            if (isSampling()) {
                issueRefresh();
            }
            return false;
        }
    };

    private static final String LAT_LON_FORMAT_STR = "%5.2f";

    private MPEDisplayManager displayMgr;

    private double scale;

    private PixelExtent lastExtent;

    private double textSpace;

    private AbstractVizResource<?, ?> rsc;

    private final String rfc = MPEDataManager.getInstance().getRFC();

    private double width;

    private double cell;

    private String mval = "";

    private double scaleVal;

    private boolean displayInfo = false;

    private MPEFontFactory fontFactory;

    private IFont font;

    private SimpleDateFormat legendFormat = MPEDateFormatter
            .createSimpleDateFormat(MPEDateFormatter.MMM_dd_yyyy_HH);

    public MPELegendResource(GenericResourceData rscData,
            LoadProperties loadProps) {
        super(rscData, loadProps);
    }

    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inspectHandler);
        }
        fontFactory.dispose();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        displayMgr = MPEDisplayManager
                .getInstance(descriptor.getRenderableDisplay());
        fontFactory = new MPEFontFactory(target, this);
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inspectHandler);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Fonts are shared and cached, no need to init or dispose
        font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());
        IExtent screenExtent = paintProps.getView().getExtent();

        scale = (screenExtent.getHeight()
                / paintProps.getCanvasBounds().height);

        DrawableString pa = new DrawableString("0", new RGB(255, 255, 255));
        pa.font = font;
        double textHeight = target.getStringsBounds(pa).getHeight() * scale;
        double padding = 3 * scale;
        textSpace = textHeight + padding;
        double yMax = screenExtent.getMaxY();
        target.clearClippingPlane();

        yMax = drawGageQCLegend(target, paintProps.getAlpha(),
                screenExtent.getMinX(), screenExtent.getMaxX(), yMax);

        yMax = drawMpeInfo(target, paintProps.getAlpha(),
                screenExtent.getMinX(), screenExtent.getMaxX(), yMax, padding);

        if (displayMgr.isQpf() || displayMgr.isMaxmin()
                || displayMgr.isZflag()) {
            yMax = drawDqcLegend(target, paintProps.getAlpha(),
                    screenExtent.getMinX(), screenExtent.getMaxX(), yMax,
                    textHeight, padding);
        } else {
            yMax = drawMpeLegend(target, paintProps, screenExtent.getMinX(),
                    screenExtent.getMaxX(), yMax, textHeight, padding);
        }

        target.setupClippingPlane(screenExtent);
        super.paintInternal(target, paintProps);
    }

    private double drawMpeLegend(IGraphicsTarget target,
            PaintProperties paintProps, double xMin, double xMax, double yMax,
            double textHeight, double padding) throws VizException {
        // TODO: Pass in expected resource
        float alpha = paintProps.getAlpha();
        double legendHeight = 0;
        rsc = displayMgr.getDisplayedFieldResource();

        if (rsc != null) {
            if (rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {

                double cmapHeight = textHeight * 1.25;

                legendHeight = cmapHeight + 2.0 * textSpace + 2.0 * padding;
                double y1 = yMax - legendHeight;

                DrawableColorMap cmap = new DrawableColorMap(
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
                cmap.alpha = alpha;
                IColorMap cm = cmap.getColorMapParams().getColorMap();

                width = (xMax - xMin) / 30 * cm.getSize();
                PixelExtent legendExtent = new PixelExtent(xMin, xMin + width,
                        y1, yMax);
                target.drawShadedRect(legendExtent, new RGB(0, 0, 0), alpha,
                        null);
                int offset = 0;
                RGB textColor = new RGB(255, 255, 255);
                RGB productTypeTextColor = new RGB(0, 0, 0);
                DrawableString strings = new DrawableString("", textColor);
                strings.font = font;

                // Radar Coverage Map's type is called "Index"

                if (displayMgr.getDisplayFieldType()
                        .equals(DisplayFieldData.Index)) {
                    offset = (int) (rsc.getCapability(ColorMapCapability.class)
                            .getColorMapParameters().getLabels().get(1)
                            .getLocation() * width / 2);
                }

                int i = 0;

                List<LabelEntry> labelEntryList = rsc
                        .getCapability(ColorMapCapability.class)
                        .getColorMapParameters().getLabels();

                for (LabelEntry entry : labelEntryList) {

                    if (entry.getText().length() > 10) {
                        break;
                    } else {
                        double cbarSize = (width / cm.getSize());
                        double xLoc = xMin + offset + (cbarSize * i);
                        strings.setText(entry.getText(), textColor);
                        strings.horizontalAlignment = HorizontalAlignment.CENTER;
                        strings.verticallAlignment = VerticalAlignment.TOP;
                        strings.setCoordinates(xLoc, y1);
                        target.drawStrings(strings);
                    }
                    i++;
                }

                // draw color bars
                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1,
                        y1 + cmapHeight);
                target.drawColorRamp(cmap);

                if (displayMgr.getDisplayFieldType()
                        .equals(DisplayFieldData.Index)) {
                    // draw radar product type indicator (S,D, M) for Single
                    // Pol, Dual Pol, or Missing
                    // this is drawn on top of the color bars
                    AvailableRadarGridType availableRadarGridType = AvailableRadarGridType.MISSING;
                    String typeString = null;

                    int offsetRadarList = 2;
                    for (int index = offsetRadarList; index < labelEntryList
                            .size(); index++) {
                        LabelEntry entry = labelEntryList.get(index);
                        String radarId = entry.getText();

                        if (!radarId.isEmpty()) {
                            availableRadarGridType = displayMgr
                                    .getAvailableRadarType(radarId);

                            if (availableRadarGridType.equals(
                                    AvailableRadarGridType.SINGLE_AND_DUAL_POL)) {
                                typeString = "SD";
                            } else if (availableRadarGridType
                                    .equals(AvailableRadarGridType.DUAL_POL)) {
                                typeString = "D";
                            } else if (availableRadarGridType.equals(
                                    AvailableRadarGridType.SINGLE_POL)) {
                                typeString = "S";
                            } else {
                                // missing
                                typeString = "M";
                            }

                            double cbarSize = (width / cm.getSize());
                            int offsetForTypeString = offset
                                    + (int) (2 * cbarSize);
                            double xLoc = xMin + offsetForTypeString
                                    + (cbarSize * (index - 2));
                            strings.setText(typeString, productTypeTextColor);
                            strings.horizontalAlignment = HorizontalAlignment.CENTER;
                            strings.verticallAlignment = VerticalAlignment.TOP;
                            strings.setCoordinates(xLoc, y1);
                            target.drawStrings(strings);
                        }
                    }

                }

                y1 += cmapHeight;

                int accum = 0;
                if (rsc instanceof MPEFieldResource) {
                    // IMPEResource.getAccumulationInterval()?
                    accum = ((MPEFieldResource) rsc).getResourceData()
                            .getAccumulationInterval();
                }

                String dateStr = legendFormat.format(paintProps.getFramesInfo()
                        .getTimeForResource(rsc).getRefTime()) + "Z";

                if (accum > 0) {
                    String qpeString = String.format(
                            "%d hr Accumulated %s For %s Ending %s (in)", accum,
                            rsc.getName(), rfc, dateStr);

                    double xLoc = xMin + padding;
                    strings.setText(qpeString, textColor);
                    strings.horizontalAlignment = HorizontalAlignment.LEFT;
                    strings.verticallAlignment = VerticalAlignment.TOP;
                    strings.setCoordinates(xLoc, y1);
                    target.drawStrings(strings);
                } else {
                    String fieldString = String.format("%s site=%s %s", dateStr,
                            rfc, rsc.getName());
                    strings.setText(fieldString, textColor);
                    double xLoc = xMin + padding;
                    strings.horizontalAlignment = HorizontalAlignment.LEFT;
                    strings.verticallAlignment = VerticalAlignment.TOP;
                    strings.setCoordinates(xLoc, y1);
                    target.drawStrings(strings);
                }
                textColor = null;
            }
        }
        return yMax - legendHeight;
    }

    private double drawDqcLegend(IGraphicsTarget target, float alpha,
            double xMin, double xMax, double yMax, double textHeight,
            double padding) throws VizException {
        double legendHeight = 0;

        // TODO: pass in expected resource type
        rsc = displayMgr.getDisplayedResource();

        double cmapHeight = textHeight * 1.25;

        legendHeight = cmapHeight + 2.0 * textSpace + 2.0 * padding;
        double y1 = yMax - legendHeight;
        int cmapSize = 1;
        DrawableColorMap cmap = null;
        RGB textColor = new RGB(255, 255, 255);
        DrawableString strings = new DrawableString("", textColor);
        strings.font = font;
        if (rsc != null) {
            if (rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {
                cmap = new DrawableColorMap(
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
                cmap.alpha = alpha;

                if (cmap.getColorMapParams() == null) {
                    statusHandler
                            .debug("Can't access Color Map Parameters when loading "
                                    + DrawDQCStations.qcmode);
                    y1 += textSpace;
                    y1 += cmapHeight;
                    strings.setText(DrawDQCStations.qcmode, textColor);
                    double xLoc = xMin + padding;
                    strings.horizontalAlignment = HorizontalAlignment.LEFT;
                    strings.verticallAlignment = VerticalAlignment.TOP;
                    strings.setCoordinates(xLoc, y1);
                    target.drawStrings(strings);
                    return yMax - legendHeight;
                }

                cmapSize = cmap.getColorMapParams().getColorMap().getSize();
                width = (xMax - xMin) / 30 * cmapSize;
            }
            PixelExtent legendExtent = new PixelExtent(xMin, xMin + width, y1,
                    yMax);
            this.setExtent(legendExtent);
            cell = width / cmapSize;
            if (rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {
                target.drawShadedRect(legendExtent, new RGB(0, 0, 0), alpha,
                        null);
                int offset = 0;
                int i = 0;
                for (LabelEntry entry : cmap.getColorMapParams().getLabels()) {
                    if (entry.getText().length() > 10) {
                        break;
                    } else {
                        double cbarSize = (width / cmapSize);
                        double xLoc = xMin + offset + (cbarSize * i);
                        strings.setText(entry.getText(), textColor);
                        strings.horizontalAlignment = HorizontalAlignment.CENTER;
                        strings.verticallAlignment = VerticalAlignment.TOP;
                        strings.setCoordinates(xLoc, y1);
                        target.drawStrings(strings);
                    }
                    i++;
                }

                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1,
                        y1 + cmapHeight);
                target.drawColorRamp(cmap);
                y1 += cmapHeight;
                strings.setText(rsc.getName(), textColor);
                double xLoc = xMin + padding;
                strings.horizontalAlignment = HorizontalAlignment.LEFT;
                strings.verticallAlignment = VerticalAlignment.TOP;
                strings.setCoordinates(xLoc, y1);
                target.drawStrings(strings);

                /*
                 * These instanceof checks prevent us from up duplicating the
                 * DailyQC legend text when drawing the points in combination
                 * with grids or the mean area grid types.
                 */
                if ((!(rsc instanceof PlotGriddedPrecipResource))
                        && (!(rsc instanceof PlotMeanAreaPrecipResource))
                        && (!(rsc instanceof PlotGriddedTempResource))
                        && (!(rsc instanceof PlotMeanAreaTempResource))
                        && (!(rsc instanceof PlotGriddedFreezeResource))
                        && (!(rsc instanceof PlotMeanAreaFreezeResource))) {
                    /*
                     * we draw "DailyQC" legend text above the colorbar and
                     * other associated text, so move the height pointer in the
                     * opposite direction.
                     */
                    y1 -= textSpace * 2;
                    y1 -= cmapHeight;

                    strings.setText(DrawDQCStations.qcmode, textColor);
                    xLoc = xMin + padding;
                    strings.horizontalAlignment = HorizontalAlignment.LEFT;
                    strings.verticallAlignment = VerticalAlignment.TOP;
                    strings.setCoordinates(xLoc, y1);
                    target.drawStrings(strings);
                }
            } else {
                cmap = new DrawableColorMap(
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
                // We are in Point mode of DQC
                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1,
                        y1 + cmapHeight);
                y1 += cmapHeight;

                strings.setText(DrawDQCStations.qcmode, textColor);
                double xLoc = xMin + padding;
                strings.horizontalAlignment = HorizontalAlignment.LEFT;
                strings.verticallAlignment = VerticalAlignment.TOP;
                strings.setCoordinates(xLoc, y1);
                target.drawStrings(strings);
            }
        } else {
            // No xmrg resource has been loaded, point data only.
            y1 += textSpace;
            y1 += cmapHeight;

            strings.setText(DrawDQCStations.qcmode, textColor);
            double xLoc = xMin + padding;
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.TOP;
            strings.setCoordinates(xLoc, y1);
            target.drawStrings(strings);
        }

        return yMax - legendHeight;
    }

    private double drawMpeInfo(IGraphicsTarget target, float alpha, double xMin,
            double xMax, double yMax, double padding) throws VizException {
        double legendHeight = 0;
        RGB textColor = new RGB(255, 255, 255);
        DrawableString strings = new DrawableString("", textColor);
        strings.font = font;
        if (isSampling() && rsc != null
                && rsc.getStatus() == ResourceStatus.INITIALIZED) {
            legendHeight = textSpace + 2 * padding;
            width = (xMax - xMin) / 25
                    * rsc.getCapability(ColorMapCapability.class)
                            .getColorMapParameters().getColorMap().getSize();
            PixelExtent legendExtent = new PixelExtent(xMin, xMin + width,
                    yMax - legendHeight, yMax);
            target.drawShadedRect(legendExtent, new RGB(0, 0, 0), alpha, null);

            String hrapX = "9999";
            String hrapY = "9999";
            String value = "-----";
            String county = "Xxxxxxxxxxxx ";
            String basin = "Xxxxxxxx ";
            String lon = "99.99";
            String lat = "9999.99";

            Map<String, Object> values = getMpeInfo(rsc);
            if (values != null) {
                String gridValue = (String) values
                        .get(MPEInterrogationConstants.INTERROGATE_VALUE);
                if (gridValue != null) {
                    value = gridValue;
                }
            }

            values = getMpeInfo(this);
            if (values != null) {
                Coordinate gridCell = (Coordinate) values
                        .get(MPEInterrogationConstants.INTERROGATE_GRID_CELL);
                if (gridCell != null) {
                    hrapX = Integer.toString((int) gridCell.x);
                    hrapY = Integer.toString((int) gridCell.y);
                }

                Coordinate latLon = (Coordinate) values
                        .get(MPEInterrogationConstants.INTERROGATE_LAT_LON);
                if (latLon != null) {
                    lon = String.format(LAT_LON_FORMAT_STR, latLon.x);
                    lat = String.format(LAT_LON_FORMAT_STR, latLon.y);
                }

                String countyValue = (String) values
                        .get(MPEInterrogationConstants.INTERROGATE_COUNTY);
                if (countyValue != null) {
                    county = countyValue;
                }
                String basinValue = (String) values
                        .get(MPEInterrogationConstants.INTERROGATE_BASIN);
                if (basinValue != null) {
                    basin = basinValue;
                }
            }

            double x = xMin + padding;
            double y = yMax - 2 * padding;

            strings.setText("Hrap x, y:  ", textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            String hrap = String.format("%4s, %4s", hrapX, hrapY);
            strings.setText(hrap, textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(" Lat, Lon:  ", textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            String latlon = String.format("%5s, %7s", lat, lon);
            strings.setText(latlon, textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(" Value: ", textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(value, textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(" County: ", textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(county, textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(" Basin: ", textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
            x += target.getStringsBounds(strings).getWidth() * scale;

            strings.setText(basin, textColor);
            strings.horizontalAlignment = HorizontalAlignment.LEFT;
            strings.verticallAlignment = VerticalAlignment.BOTTOM;
            strings.setCoordinates(x, y);
            target.drawStrings(strings);
        }
        textColor = null;
        return yMax - legendHeight;
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            Map<String, Object> values = new HashMap<>();
            Coordinate latLon = coord.asLatLon();

            // Get hrap grid cell
            Coordinate hrapCell = coord.asGridCell(
                    HRAP.getInstance().getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            values.put(MPEInterrogationConstants.INTERROGATE_GRID_CELL,
                    hrapCell);

            values.put(MPEInterrogationConstants.INTERROGATE_LAT_LON, latLon);

            ISpatialQuery query = SpatialQueryFactory.create();

            org.locationtech.jts.geom.Point point = new GeometryFactory()
                    .createPoint(latLon);

            SpatialQueryResult[] results = query.query("county",
                    new String[] { "countyname" }, point, null, false,
                    SearchMode.WITHIN);

            String county = "Not Defined";
            if ((results != null) && (results.length > 0)) {
                county = (String) results[0].attributes.get("countyname");
            }
            values.put(MPEInterrogationConstants.INTERROGATE_COUNTY, county);

            results = query.query("basins", new String[] { "name" }, point,
                    null, false, SearchMode.WITHIN);
            String basin = "Not Defined";
            if ((results != null) && (results.length > 0)) {
                basin = (String) results[0].attributes.get("name");
            }
            values.put(MPEInterrogationConstants.INTERROGATE_BASIN, basin);

            return values;
        } catch (Exception e) {
            throw new VizException("Error interrogating MPE legend resource",
                    e);
        }
    }

    private Map<String, Object> getMpeInfo(AbstractVizResource<?, ?> resource) {
        if (sampleCoord != null && resource != null) {
            try {
                return resource.interrogate(sampleCoord);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error inspecting xmrg resource", e);
            }
        }
        return null;
    }

    private double drawGageQCLegend(IGraphicsTarget target, float alpha,
            double xMin, double xMax, double yMax) throws VizException {
        double legendHeight = 0;

        return yMax - legendHeight;
    }

    @Override
    public String getName() {
        String legName = "";
        return legName;
    }

    protected void setExtent(PixelExtent pixelExtent) {
        lastExtent = pixelExtent;
    }

    /**
     * Get the extent
     *
     * @return the extent
     */
    public PixelExtent getExtent() {
        return lastExtent;
    }

    /**
     * Get the value at a certain coordinate
     *
     * @param coord
     * @return
     */
    public double getValueAt(double[] coord) {
        scaleVal = 0.0;
        if (rsc != null) {
            if (rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {
                ColorMapParameters params = rsc
                        .getCapability(ColorMapCapability.class)
                        .getColorMapParameters();
                if (params != null) {
                    float max = params.getDataMax();
                    double maxx = lastExtent.getMinX() + (cell * (max));
                    float fractionX = (float) ((coord[0] - lastExtent.getMinX())
                            / (maxx - lastExtent.getMinX()));
                    double val = (max) * fractionX;
                    UnitConverter cvt = params.getImageToDataConverter();
                    eVal = (int) val;
                    scaleVal = cvt.convert((int) val);
                    scaleVal = scaleVal / 100;
                }
            }
        }
        return scaleVal;

    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (displayMgr.isQpf() || displayMgr.isMaxmin()
                || displayMgr.isZflag()) {
            if (DrawDQCStations.grids_flag == 1
                    || DrawDQCStations.map_flag == 1) {
                if (lastExtent != null) {
                    if (!menuManager.isEmpty()) {
                        menuManager.add(new Separator());
                    }
                    if (displayMgr.isQpf()) {
                        mval = String.format("%2.1f in.", scaleVal);
                    } else if (displayMgr.isMaxmin()) {
                        mval = String.format("%3d F", Math.round(scaleVal));
                    } else {
                        mval = String.format("%5.2f ft.", scaleVal);
                    }
                    menuManager.add(new SetUPAction());
                    menuManager.add(new SetDownAction());
                    menuManager.add(new SetOffAction());
                }
            }
        }
    }

    private class SetUPAction extends AbstractRightClickAction {

        public SetUPAction() {
            super("Filter UP on Value: " + mval);

        }

        @Override
        public void run() {
            DrawDQCStations.getInstance()
                    .reloadDQC(new MPELegendOverride(OverrideType.UP, eVal));
        }
    }

    private class SetDownAction extends AbstractRightClickAction {

        public SetDownAction() {
            super("Filter Down on Value: " + mval);
        }

        @Override
        public void run() {
            DrawDQCStations.getInstance()
                    .reloadDQC(new MPELegendOverride(OverrideType.DOWN, eVal));
        }
    }

    private class SetOffAction extends AbstractRightClickAction {

        public SetOffAction() {
            super("Turn Filtering Off");
        }

        @Override
        public void run() {
            ColorMapParameters cmc = rsc.getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            cmc.setDataMax(cmc.getColorMap().getSize() - 1);
            DrawDQCStations.getInstance().reloadDQC(new MPELegendOverride());

        }
    }

    @Override
    public boolean isSampling() {
        return displayInfo;
    }

    @Override
    public void setSampling(boolean sampling) {
        displayInfo = sampling;
        issueRefresh();
    }

    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {
        FramesInfo frameInfo = descriptor.getFramesInfo();
        List<LegendData> labels = new ArrayList<>();
        ResourceList resourceList = descriptor.getResourceList();
        if (resourceList != null) {
            for (int i = 0; i < resourceList.size(); i++) {
                ResourcePair resourcePair = resourceList.get(i);
                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                // See if resource is visible
                boolean vis = resourcePair.getProperties().isVisible();
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (system) {
                    continue;
                } else {
                    LegendData legend = new LegendData();
                    if (rsc == null) {
                        continue;
                    } else if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                        continue;
                    } else {
                        legend.label = rsc.getName();
                        legend.resource = resourcePair;
                        if (!rsc.isTimeAgnostic()) {
                            DataTime date = frameInfo.getTimeForResource(rsc);
                            String time = " No Data Available";
                            if (date != null) {
                                time = " - " + date.getLegendString();
                            }
                            legend.label += time;
                        }
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        legend.color = rsc
                                .getCapability(ColorableCapability.class)
                                .getColor();
                    }
                    labels.add(legend);
                }
            }
        }

        LegendEntry[] entries = new LegendEntry[labels.size()];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].legendParts = new LegendData[] { labels.get(i) };
        }
        return entries;
    }
}
