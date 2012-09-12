package com.raytheon.uf.viz.profiler;

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

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerLevel;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.profiler.ui.ProfilerUtils;
import com.raytheon.uf.viz.profiler.ui.ProfilerUtils.PlotObject;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles profiler data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 8, 2009    2219     dhladky     Initial creation
 * Oct  18, 2010    6528     bsteffen    Refactor to handle updates and requesting previous times
 * Feb   8, 2011    8036     bkowal      added the Magnification capability;
 *                                       replaced deprecated function calls
 *                                       replaced deprecated function calls
 * Feb  10, 2011    8030     bkowal      access to the plots ArrayList is now synchronized
 * Feb  15, 2011    8036     bkowal      magnification only affects the x-axis, wind barbs, and
 *                                       the color bar.
 * ======================================
 * AWIPS2 DR Work
 * 08/10/2012         1035 jkorman     Changed number of 'staffs' from 12 to 13 and changed time
 *                                     display to match AWIPS I.
 * 08/13/2012         1046 jkorman     Changed to load colorMap file.                                     
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProfilerResource extends
        AbstractVizResource<ProfilerResourceData, ProfilerDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProfilerResource.class);

    private static final int NUM_PROFILE_STAFFS = 13;

    /* graph max height in meters*/
    private static double MAX_Y = 18000;

    /* Graphic target */
    private IGraphicsTarget target = null;

    /* the wind barb objects that get drawn */
    private HashMap<Long, ArrayList<PlotObject>> plotObjects = null;

    /* Height and Width scales for zooming */
    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    /* The font used */
    private IFont font = null;

    /* increments XY */
    private double incX = 0;

    private double incYheight = 0;

    private long earliestTime = Long.MAX_VALUE;

    /**
     * Required method for getting data
     * 
     * @param resourceData
     * @param loadProperties
     */
    protected ProfilerResource(ProfilerResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        ProfilerUtils.decimalFormat.setMaximumFractionDigits(0);
        ProfilerUtils.decimalFormat.setGroupingUsed(false);
    }

    @Override
    protected void disposeInternal() {
        for (ArrayList<PlotObject> plotList : plotObjects.values()) {
            for (PlotObject plotObject : plotList) {
                if (plotObject.image != null) {
                    plotObject.image.dispose();
                }
            }
        }
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.target = target;
        dataTimes = new ArrayList<DataTime>();

        incX = (ProfilerUtils.profilerRectangle.width / NUM_PROFILE_STAFFS);
        incYheight = ProfilerUtils.profilerRectangle.height / MAX_Y;

        this.font = target.initializeFont("Dialog", 11, null);

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (params == null) {
            params = new ColorMapParameters();
            this.getCapability(ColorMapCapability.class).setColorMapParameters(
                    params);
        }

        String cmName = null;
        if ((cmName = params.getColorMapName()) != null) {
            IColorMap colorMap = ColorMapLoader.loadColorMap(cmName);
            params.setColorMap(colorMap);
        }
        // If we failed to load a colorMap, load a default!
        if (params.getColorMap() == null) {
            params.setColorMap(ProfilerUtils.getColorMap());
        }
        params.setColorMapMin(ProfilerUtils.colorRange[0]);
        params.setColorMapMax(ProfilerUtils.colorRange[1]);
        params.setColorBarIntervals(ProfilerUtils.colorLabels);

        resourceData.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (object instanceof ColorMapCapability) {
                    // dispose of all existing plotObjects
                    for (ArrayList<PlotObject> plotList : plotObjects.values()) {
                        for (PlotObject plotObject : plotList) {
                            if (plotObject.image != null) {
                                plotObject.image.dispose();
                            }
                        }
                    }
                    plotObjects = null;
                    try {
                        loadData();
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);

                    }

                } else if (object instanceof MagnificationCapability) {
                    font.setMagnification(((MagnificationCapability) object)
                            .getMagnification().floatValue());
                } else if (type == ChangeType.DATA_UPDATE) {
                    PluginDataObject[] updates = (PluginDataObject[]) object;
                    List<ProfilerObs> records = new ArrayList<ProfilerObs>(
                            updates.length);
                    for (PluginDataObject pdo : updates) {
                        records.add((ProfilerObs) pdo);
                    }
                    resourceData.getRecords().addAll(records);
                    try {
                        createProfiles(records);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);

                    }
                }
            }
        });
        loadData();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (paintProps.getDataTime() != null
                && paintProps.getDataTime().getValidTime().getTimeInMillis() < earliestTime) {
            // Need to request previous frames;
            long latestTime = Long.MIN_VALUE;
            for (DataTime time : paintProps.getFramesInfo().getTimeMap()
                    .get(this)) {
                if (time == null) {
                    continue;
                }
                long validTime = time.getValidTime().getTimeInMillis();
                earliestTime = Math.min(earliestTime, validTime);
                latestTime = Math.max(latestTime, validTime);
            }
            long earliestRequestTime = earliestTime - NUM_PROFILE_STAFFS * 3600000;
            List<DataTime> requestTimes = new ArrayList<DataTime>();
            for (DataTime time : resourceData.getAvailableTimes()) {
                long validTime = time.getValidTime().getTimeInMillis();
                if (validTime > earliestRequestTime && validTime < latestTime) {
                    requestTimes.add(time);
                }
            }
            resourceData.fireChangeListeners(
                    ChangeType.DATA_UPDATE,
                    resourceData.getLatestPluginDataObjects(
                            requestTimes.toArray(new DataTime[0]),
                            dataTimes.toArray(new DataTime[0])));
        }

        this.target = target;

        if (plotObjects == null) {
            initInternal(target);
        }

        // Determine the magnification for the plot
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        // set the scale sizing
        setScaleWidth(paintProps, magnification);
        setScaleHeight(paintProps, magnification);

        // paint the background and labels
        drawYAxis(paintProps, magnification);
        // draw x after y so x will paint over axis
        drawXAxis(paintProps, magnification);

        if (plotObjects != null) {
            drawProfiles(paintProps);
        }
    }

    /**
     * Set the width scalar
     * 
     * @param props
     * @return
     */
    private void setScaleWidth(PaintProperties props, Double magnification) {
        double screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        scaleWidthValue = ((ProfilerUtils.BARB_SIZE / 2.0) / screenToWorldWidthRatio)
                * magnification;
    }

    /**
     * get the scale width value
     * 
     * @return
     */
    private double getScaleWidth() {
        return scaleWidthValue;
    }

    /**
     * Set the height scalar
     * 
     * @param props
     * @return
     */
    private void setScaleHeight(PaintProperties props, Double magnification) {
        double screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        scaleHeightValue = ((ProfilerUtils.BARB_SIZE / 2.0) / screenToWorldHeightRatio)
                * magnification;
    }

    /**
     * Get the scalar height
     * 
     * @return
     */
    private double getScaleHeight() {
        return scaleHeightValue;
    }

    /**
     * gets the pixel coverage for this image
     * 
     * @return
     */
    private PixelCoverage getPixelCoverage(Coordinate c) {

        Coordinate ul = new Coordinate(c.x - getScaleWidth(), c.y
                - getScaleHeight());
        Coordinate ur = new Coordinate(c.x + getScaleWidth(), c.y
                - getScaleHeight());
        Coordinate lr = new Coordinate(c.x + getScaleWidth(), c.y
                + getScaleHeight());
        Coordinate ll = new Coordinate(c.x - getScaleWidth(), c.y
                + getScaleHeight());

        return new PixelCoverage(ul, ur, lr, ll);
    }

    /**
     * iterate through our list of profile plots and draw images
     * 
     * @throws VizException
     */
    private void drawProfiles(PaintProperties paintProps) throws VizException {
        if (plotObjects == null) {
            return;
        }
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            return;
        }
        long paintTime = time.getValidTime().getTimeInMillis();
        for (Entry<Long, ArrayList<PlotObject>> entry : plotObjects.entrySet()) {
            int x = (int) ((paintTime - entry.getKey()) / 3600000);
            if (x < 0) {
                continue;
            }
            if (x >= NUM_PROFILE_STAFFS) {
                continue;
            }
            ArrayList<PlotObject> plots = entry.getValue();
            if (plots == null) {
                continue;
            }

            synchronized (plots) {
                for (PlotObject po : plots) {
                    if (po != null) {
                        double y = calcY(po.coord.y);
                        target.drawRaster(po.image,
                                getPixelCoverage(new Coordinate(calcX(x), y)),
                                paintProps);
                    }
                }
            }
        }
    }

    /**
     * Create profile windBards
     * 
     * @param profile
     * @throws VizException
     */
    private void createProfiles(List<ProfilerObs> profiles) throws VizException {
        // bottom to top? I hope?
        if (plotObjects == null) {
            plotObjects = new HashMap<Long, ArrayList<PlotObject>>();
        }

        for (ProfilerObs profile : profiles) {
            long time = profile.getDataTime().getValidTime().getTimeInMillis();
            dataTimes.add(profile.getDataTime());
            ArrayList<PlotObject> plots = plotObjects.get(time);
            if (plots == null) {
                plots = new ArrayList<PlotObject>();
                plotObjects.put(time, plots);
            }
            // get the level information for this profile
            synchronized (plots) {
                for (ProfilerLevel level : profile.getLevels()) {
                    try {
                        PlotObject plot = createPlot(level);
                        if (plot != null) {
                            plots.add(plot);
                        }
                    } catch (VizException e) {
                        // do something with the exception
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    /**
     * 
     * @param index
     * @param level
     * @return
     * @throws VizException
     */
    private PlotObject createPlot(ProfilerLevel level) throws VizException {
        PlotObject plot = null;
        if (level.getLevelHeight() != null) {
            if (level.getUcWind() != null) {
                if (level.getVcWind() != null) {
                    plot = ProfilerUtils.createWindBarb(level, target,
                            descriptor,
                            new Coordinate(0, level.getLevelHeight()),
                            getCapability(ColorMapCapability.class)
                                    .getColorMapParameters().getColorMap());
                }
            }
        }
        return plot;
    }

    public void loadData() throws VizException {
        createProfiles(getResourceData().getRecords());
    }

    /**
     * @throws VizException
     * 
     */
    public void drawXAxis(PaintProperties paintProps, Double magnification)
            throws VizException {
        // left edge of graph
        target.drawLine(
                ProfilerUtils.profilerRectangle.x,
                (ProfilerUtils.profilerRectangle.y + ProfilerUtils.profilerRectangle.height),
                0.0, ProfilerUtils.profilerRectangle.x,
                ProfilerUtils.profilerRectangle.y, 0.0,
                ProfilerUtils.GRAPH_COLOR, ProfilerUtils.GRAPH_LINE_WIDTH,
                IGraphicsTarget.LineStyle.SOLID);

        if (paintProps.getDataTime() != null) {
            DrawableString parameters = new DrawableString("",
                    ProfilerUtils.GRAPH_COLOR);
            parameters.textStyle = TextStyle.BLANKED;
            parameters.font = font;
            parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
            parameters.verticallAlignment = IGraphicsTarget.VerticalAlignment.MIDDLE;
            parameters.basics.y = ProfilerUtils.profilerRectangle.y
                    + ProfilerUtils.profilerRectangle.height
                    + ProfilerUtils.LABEL_OFFSET;
            parameters.magnification = magnification;

            double maxY = paintProps.getView().getExtent().getMaxY();
            Rectangle2D rect = target.getStringsBounds(parameters);
            if (parameters.basics.y + (rect.getHeight() / 2) > maxY) {
                parameters.basics.y = maxY;
                parameters.verticallAlignment = IGraphicsTarget.VerticalAlignment.BOTTOM;
            }

            Calendar c = paintProps.getDataTime().getValidTime();
            for (int i = 0; i < NUM_PROFILE_STAFFS; i++) {

//                String d = String.format("%1$tH:%1$tM", c);
                String d = String.format("%1$tH", c);
                parameters.setText(d, ProfilerUtils.GRAPH_COLOR);
                parameters.basics.x = ProfilerUtils.profilerRectangle.x
                        + (i * incX) + (incX / 2);
                target.drawStrings(parameters);
                c.add(Calendar.HOUR, -1);
            }
        }

        // draw right edge
        target.drawLine(
                (ProfilerUtils.profilerRectangle.x + ProfilerUtils.profilerRectangle.width),
                (ProfilerUtils.profilerRectangle.y + ProfilerUtils.profilerRectangle.height),
                0.0,
                (ProfilerUtils.profilerRectangle.x + ProfilerUtils.profilerRectangle.width),
                ProfilerUtils.profilerRectangle.y, 0.0,
                ProfilerUtils.GRAPH_COLOR, ProfilerUtils.GRAPH_LINE_WIDTH,
                IGraphicsTarget.LineStyle.SOLID);
    }

    /**
     * @throws VizException
     * 
     */
    public void drawYAxis(PaintProperties paintProps, Double magnification)
            throws VizException {
        DrawableString parameters = new DrawableString("18km",
                ProfilerUtils.GRAPH_COLOR);
        parameters.textStyle = TextStyle.BLANKED;
        parameters.font = font;
        parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.RIGHT;
        parameters.verticallAlignment = IGraphicsTarget.VerticalAlignment.MIDDLE;
        parameters.basics.x = ProfilerUtils.profilerRectangle.x
                - ProfilerUtils.LABEL_OFFSET;
        parameters.basics.y = ProfilerUtils.profilerRectangle.y;

        double minX = paintProps.getView().getExtent().getMinX();
        double maxX = paintProps.getView().getExtent().getMaxX();

        // top of graph
        target.drawLine(ProfilerUtils.profilerRectangle.x,
                ProfilerUtils.profilerRectangle.y, 0.0,
                ProfilerUtils.profilerRectangle.x
                        + ProfilerUtils.profilerRectangle.width,
                ProfilerUtils.profilerRectangle.y, 0.0,
                ProfilerUtils.GRAPH_COLOR, ProfilerUtils.GRAPH_LINE_WIDTH,
                IGraphicsTarget.LineStyle.SOLID);

        Rectangle2D rect = target.getStringsBounds(parameters);

        if (parameters.basics.x - rect.getWidth() < minX) {
            parameters.basics.x = minX;
            parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
        }
        target.drawStrings(parameters);

        // loop for heights in meters
        for (int i = 0; i < ProfilerUtils.HEIGHTS; i += 2) {
            // draw Y labels
            parameters.setText(
                    ProfilerUtils.decimalFormat.format(new Double(i)) + " km",
                    ProfilerUtils.GRAPH_COLOR);
            parameters.basics.y = calcY(i * 1000);
            rect = target.getStringsBounds(parameters);
            if (parameters.basics.x - rect.getWidth() < minX) {
                parameters.basics.x = minX;
                parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
            }

            target.drawLine(
                    ProfilerUtils.profilerRectangle.x,
                    calcY(i * 1000),
                    0.0,
                    (ProfilerUtils.profilerRectangle.x + ProfilerUtils.profilerRectangle.width),
                    calcY(i * 1000), 0.0, ProfilerUtils.GRAPH_COLOR,
                    ProfilerUtils.GRAPH_LINE_WIDTH,
                    IGraphicsTarget.LineStyle.SOLID);
            // draw after line so it draws on top
            target.drawStrings(parameters);

        }
        double stationElevation = 0;
        if (!getResourceData().records.isEmpty()) {
            stationElevation = getResourceData().getRecords().get(0)
                    .getElevation();
        }
        // Draw the surface line.
        target.drawLine(
                ProfilerUtils.profilerRectangle.x,
                calcY(stationElevation),
                0.0,
                (ProfilerUtils.profilerRectangle.x + ProfilerUtils.profilerRectangle.width),
                calcY(stationElevation), 0.0, ProfilerUtils.GRAPH_COLOR,
                ProfilerUtils.GRAPH_LINE_WIDTH, IGraphicsTarget.LineStyle.SOLID);

        // loop for pressure levels and labels
        parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.LEFT;
        parameters.basics.x = ProfilerUtils.profilerRectangle.x
                + ProfilerUtils.profilerRectangle.width
                + ProfilerUtils.LABEL_OFFSET;

        for (int i = 0; i < ProfilerUtils.PRESSURES.length; i++) {
            double height = WxMath.pressureToHeight(ProfilerUtils.PRESSURES[i]);
            if (height <= MAX_Y) {
                parameters.setText(
                        ProfilerUtils.decimalFormat.format(new Double(
                                ProfilerUtils.PRESSURES[i])) + " mb",
                        ProfilerUtils.GRAPH_COLOR);
                parameters.basics.y = calcY(height);
                rect = target.getStringsBounds(parameters);
                if (parameters.basics.x + rect.getWidth() > maxX) {
                    parameters.basics.x = maxX;
                    parameters.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.RIGHT;
                }
                target.drawStrings(parameters);
            }
        }

        // bottom of graph
        target.drawLine(
                ProfilerUtils.profilerRectangle.x,
                (ProfilerUtils.profilerRectangle.y + ProfilerUtils.profilerRectangle.height),
                0.0,
                (ProfilerUtils.profilerRectangle.x + ProfilerUtils.profilerRectangle.width),
                (ProfilerUtils.profilerRectangle.y + ProfilerUtils.profilerRectangle.height),
                0.0, ProfilerUtils.GRAPH_COLOR, ProfilerUtils.GRAPH_LINE_WIDTH,
                IGraphicsTarget.LineStyle.SOLID);
    }

    /**
     * Gets the name to place on the screen
     */
    @Override
    public String getName() {
        StringBuffer name = new StringBuffer();
        if (!resourceData.records.isEmpty()) {
            ProfilerObs obs = getResourceData().getRecords().get(0);
            name.append(obs.getProfilerName());
            name.append(" ");
            name.append(ProfilerUtils.UNITS);
            return name.toString();
        }
        return "Profiler";
    }

    /**
     * 
     * @param height
     * @return
     */
    private final double calcY(double height) {
        return (ProfilerUtils.profilerRectangle.y + ProfilerUtils.profilerRectangle.height)
                - (height * incYheight);
    }

    /**
     * 
     * @param w
     * @return
     */
    private final double calcX(double w) {
        return ProfilerUtils.profilerRectangle.x + (w * incX) + incX / 2;
    }

}
