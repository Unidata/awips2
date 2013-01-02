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

import javax.measure.converter.UnitConverter;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters.LabelEntry;
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
import com.raytheon.viz.mpe.ui.MPEFontFactory;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * The MPE Legend Resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2008            randerso     Initial creation
 * Dec 02, 2009  3237      snaples      Added options for 6/24 hour
 *                                      display filtering.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class MPELegendResource extends
        AbstractLegendResource<GenericResourceData> implements IMpeResource,
        IContextMenuContributor, ISamplingResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPELegendResource.class);

    public static double eVal = 0;

    public static double dVal = 0;

    public static boolean up = false;

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inspectHandler);
        }
        fontFactory.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        displayMgr = MPEDisplayManager.getInstance(descriptor
                .getRenderableDisplay());
        fontFactory = new MPEFontFactory(target, this);
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inspectHandler);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Fonts are shared and cached, no need to init or dispose
        font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());
        IExtent screenExtent = paintProps.getView().getExtent();

        scale = (screenExtent.getHeight() / paintProps.getCanvasBounds().height);

        DrawableString pa = new DrawableString("0", new RGB(255, 255, 255));
        pa.font = font;
        double textHeight = target.getStringsBounds(pa).getHeight() * scale;
        double padding = 3 * scale;
        textSpace = textHeight + padding;
        double yMax = screenExtent.getMaxY();
        PixelExtent pe = new PixelExtent(screenExtent.getMinX(),
                screenExtent.getMaxX(), screenExtent.getMinY(),
                screenExtent.getMinY() + screenExtent.getMaxY());
        this.setExtent(pe);
        target.clearClippingPlane();

        yMax = drawGageQCLegend(target, paintProps.getAlpha(),
                screenExtent.getMinX(), screenExtent.getMaxX(), yMax);

        yMax = drawMpeInfo(target, paintProps.getAlpha(),
                screenExtent.getMinX(), screenExtent.getMaxX(), yMax, padding);

        if (displayMgr.isQpf() == true || displayMgr.isMaxmin() == true
                || displayMgr.isZflag() == true) {
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

                DrawableColorMap cmap = new DrawableColorMap(rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters());
                cmap.interpolate = false;
                cmap.alpha = alpha;
                IColorMap cm = cmap.getColorMapParams().getColorMap();

                width = (xMax - xMin) / 30 * cm.getSize();
                PixelExtent legendExtent = new PixelExtent(xMin, xMin + width,
                        y1, yMax);
                target.drawShadedRect(legendExtent, new RGB(0, 0, 0), alpha,
                        null);
                int offset = 0;
                RGB textColor = new RGB(255, 255, 255);
                DrawableString strings = new DrawableString("", textColor);
                strings.font = font;
                if (displayMgr.getDisplayFieldType().equals(
                        DisplayFieldData.Index)) {
                    offset = (int) (rsc.getCapability(ColorMapCapability.class)
                            .getColorMapParameters().getLabels().get(1)
                            .getLocation()
                            * width / 2);
                }

                int i = 0;
                for (LabelEntry entry : rsc
                        .getCapability(ColorMapCapability.class)
                        .getColorMapParameters().getLabels()) {

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

                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1, y1
                        + cmapHeight);
                target.drawColorRamp(cmap);
                y1 += cmapHeight;

                int accum = 0;
                if (rsc instanceof MPEFieldResource) {
                    // IMPEResource.getAccumulationInterval()?
                    accum = ((MPEFieldResource) rsc).getResourceData()
                            .getAccumulationInterval();
                }

                String dateStr = legendFormat.format(paintProps.getFramesInfo()
                        .getTimeForResource(rsc).getRefTime())
                        + "z";
                if (accum > 0) {
                    String qpeString = String.format(
                            "%d hr Accumulated %s For %s Ending %sz (in)",
                            accum, rsc.getName(), rfc, dateStr);

                    double xLoc = xMin + padding;
                    strings.setText(qpeString, textColor);
                    strings.horizontalAlignment = HorizontalAlignment.LEFT;
                    strings.verticallAlignment = VerticalAlignment.TOP;
                    strings.setCoordinates(xLoc, y1);
                    target.drawStrings(strings);
                } else {
                    String fieldString = String.format("%s site=%s %s",
                            dateStr, rfc, rsc.getName());
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
                cmap = new DrawableColorMap(rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters());
                cmap.interpolate = false;
                cmap.alpha = alpha;

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
                cmap.extent = new PixelExtent(xMin, xMin + width, y1, y1
                        + cmapHeight);
                target.drawColorRamp(cmap);
                y1 += cmapHeight;
                strings.setText(rsc.getName(), textColor);
                double xLoc = xMin + padding;
                strings.horizontalAlignment = HorizontalAlignment.LEFT;
                strings.verticallAlignment = VerticalAlignment.TOP;
                strings.setCoordinates(xLoc, y1);
                target.drawStrings(strings);
            } else {
                cmap = new DrawableColorMap(rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters());
                // We are in Point mode of DQC
                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1, y1
                        + cmapHeight);
                y1 += cmapHeight;
                strings.setText(DrawDQCStations.qcmode, textColor);
                double xLoc = xMin + padding;
                strings.horizontalAlignment = HorizontalAlignment.LEFT;
                strings.verticallAlignment = VerticalAlignment.TOP;
                strings.setCoordinates(xLoc, y1);
                target.drawStrings(strings);
            }
            // textColor = null;
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

    private double drawMpeInfo(IGraphicsTarget target, float alpha,
            double xMin, double xMax, double yMax, double padding)
            throws VizException {
        double legendHeight = 0;
        RGB textColor = new RGB(255, 255, 255);
        DrawableString strings = new DrawableString("", textColor);
        strings.font = font;
        if (isSampling() && rsc != null
                && rsc.getStatus() == ResourceStatus.INITIALIZED) {
            legendHeight = textSpace + 2 * padding;
            width = (xMax - xMin)
                    / 25
                    * rsc.getCapability(ColorMapCapability.class)
                            .getColorMapParameters().getColorMap().getSize();
            PixelExtent legendExtent = new PixelExtent(xMin, xMin + width, yMax
                    - legendHeight, yMax);
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
                        .get(MPEInterrogationConstants.INTERROGATE_VALUE_LABEL);
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            Map<String, Object> values = new HashMap<String, Object>();
            Coordinate latLon = coord.asLatLon();

            // Get hrap grid cell
            Coordinate hrapCell = coord.asGridCell(HRAP.getInstance()
                    .getGridGeometry(), PixelInCell.CELL_CENTER);
            values.put(MPEInterrogationConstants.INTERROGATE_GRID_CELL,
                    hrapCell);

            values.put(MPEInterrogationConstants.INTERROGATE_LAT_LON, latLon);

            ISpatialQuery query = SpatialQueryFactory.create();

            com.vividsolutions.jts.geom.Point point = new GeometryFactory()
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
            throw new VizException("Error interrogating MPE legend resource", e);
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
        float max = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getDataMax();
        double maxx = lastExtent.getMinX() + (cell * (max));
        float fractionX = (float) ((coord[0] - lastExtent.getMinX()) / (maxx - lastExtent
                .getMinX()));
        double val = (max) * fractionX;
        ColorMapParameters params = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter cvt = params.getImageToDataConverter();
        eVal = (int) val;
        scaleVal = cvt.convert((int) val);
        scaleVal = scaleVal / 100;
        return scaleVal;

    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (displayMgr.isQpf() == true || displayMgr.isMaxmin() == true
                || displayMgr.isZflag() == true) {
            if (DrawDQCStations.grids_flag == 1
                    || DrawDQCStations.map_flag == 1) {
                if (lastExtent != null) {
                    if (menuManager.isEmpty()) {
                        if (displayMgr.isQpf() == true) {
                            mval = String.format("%2.1f in.", scaleVal);
                        } else if (displayMgr.isMaxmin()) {
                            mval = String.format("%3d F", (int) scaleVal);
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
    }

    private class SetUPAction extends AbstractRightClickAction {

        public SetUPAction() {
            super("Filter UP on Value: " + mval);

        }

        @Override
        public void run() {
            dVal = eVal;
            up = true;
            DrawDQCStations.getInstance().reloadDQC();

        }
    }

    private class SetDownAction extends AbstractRightClickAction {

        public SetDownAction() {
            super("Filter Down on Value: " + mval);

        }

        @Override
        public void run() {
            dVal = eVal;
            up = false;
            DrawDQCStations.getInstance().reloadDQC();
        }
    }

    private class SetOffAction extends AbstractRightClickAction {

        public SetOffAction() {
            super("Turn Filtering Off");

        }

        @Override
        public void run() {
            ColorMapParameters cmc = rsc
                    .getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            cmc.setDataMax(cmc.getColorMap().getSize() - 1);
            dVal = 0;
            up = false;
            DrawDQCStations.getInstance().reloadDQC();

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.sampling.ISamplingResource#isSampling()
     */
    @Override
    public boolean isSampling() {
        return displayInfo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.sampling.ISamplingResource#setSampling(boolean
     * )
     */
    @Override
    public void setSampling(boolean sampling) {
        displayInfo = sampling;
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {
        FramesInfo frameInfo = descriptor.getFramesInfo();
        List<LegendData> labels = new ArrayList<LegendData>();
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
                        if (rsc.isTimeAgnostic() == false) {
                            DataTime date = frameInfo.getTimeForResource(rsc);
                            String time = " No Data Available";
                            if (date != null) {
                                time = " - " + date.getLegendString() + "z";
                            }
                            legend.label += time;
                        }
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        legend.color = rsc.getCapability(
                                ColorableCapability.class).getColor();
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
