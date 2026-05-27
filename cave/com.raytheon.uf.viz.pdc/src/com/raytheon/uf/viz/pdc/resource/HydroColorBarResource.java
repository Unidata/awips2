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
package com.raytheon.uf.viz.pdc.resource;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.LabelEntry;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.uf.viz.pdc.data.PDCFileInfo;
import com.raytheon.uf.viz.pdc.data.PointControlPeTs;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants.AdHocDataElementType;
import com.raytheon.viz.hydrocommon.HydroConstants.TimeStepDataElement;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants.TimeModeType;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Hydro Colorbar Resource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 20, 2009           mpduff    Initial creation
 * Feb 14, 2013  1616     bsteffen  Add option for interpolation of colormap
 *                                  parameters, disable colormap interpolation
 *                                  by default.
 * Mar 03, 2014  2804     mschenke  Set back up clipping pane
 * Jul 30, 2014  3465     mapeters  Updated deprecated drawString() calls.
 * Aug 04, 2014  3489     mapeters  Updated deprecated getStringBounds() calls.
 * Oct 20, 2017  20214    jdeng     Adjust color bar height
 * Apr 17, 2018  6771     randerso  Only blank out the extent of the colorbar
 *                                  and labels. Fixed colorbar label alignment.
 *                                  Code cleanup.
 * Sep 21, 2018  7379     mduff     Moved for PDC Refactor.
 *
 * </pre>
 *
 * @author mpduff
 */

public class HydroColorBarResource
        extends AbstractVizResource<AbstractResourceData, IMapDescriptor> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroColorBarResource.class);

    private static final String[] TIME_STEP_STRINGS = { "Stage/Pool",
            "Flow/Storage", "Depth Above Flood Stage", "Percent Flood Flow",
            "Instantaneous Precip", "Hourly Precip Total",
            "3-Hour Precip Total", "6-Hour Precip Total",
            "24-Hour Precip Total", "Snow-Water Equiv", "SWE 24-Hour Change",
            "Temperature (F)", "Temp. 24-Hour Change", "Temp. 24-Hour Max",
            "Temp. 24-Hour Min", "Dewpoint (F)", "Dewpoint Change (F)",
            "Relative Humidity", "Wind Speed", "Wind Direction" };

    private static final String[] INST_PRECIP_STRINGS = { "30 Min.", "1 Hour",
            "2 Hours", "3 Hours", "4 Hours", "6 Hours", "12 Hours", "18 Hours",
            "24 Hours" };

    /**
     * Ad Hoc time mode options.
     */
    private static final String[] timeModeStrings = { "Latest Value", "Value",
            "Min Value", "Max Value", "Value Change" };

    /** Text height in pixels */
    private double textHeight;

    /** Padding value in pixels */
    private double padding;

    /** Text spacing value */
    private double textSpace;

    private static class HydroColorBarResourceData
            extends AbstractResourceData {

        @Override
        public AbstractVizResource<?, ?> construct(
                LoadProperties loadProperties, IDescriptor descriptor)
                throws VizException {
            return null;
        }

        @Override
        public void update(Object updateData) {
        }

        @Override
        public int hashCode() {
            return super.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return this == obj;
        }
    }

    /**
     * Constructor
     */
    public HydroColorBarResource() {
        super(new HydroColorBarResourceData(), new LoadProperties());
    }

    @Override
    protected void disposeInternal() {

    }

    @Override
    public String getName() {
        return "Hydro Color Bar Resource";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    /**
     * Paints this resource.
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        IExtent screenExtent = paintProps.getView().getExtent();

        double scale = (screenExtent.getHeight()
                / paintProps.getCanvasBounds().height);

        DrawableString string = new DrawableString("0");
        textHeight = target.getStringsBounds(string).getHeight() * scale;

        padding = 3 * scale;
        textSpace = textHeight + padding;

        target.clearClippingPlane();

        try {
            double yMax = screenExtent.getMaxY();

            /* Draw the color bar */
            yMax = drawColorBar(target, screenExtent.getMinX(),
                    screenExtent.getMaxX(), yMax);
        } finally {
            target.setupClippingPlane(paintProps.getClippingPane());
        }
    }

    /**
     * Draws the Hydro Color Bar
     *
     * @param target
     *            the graphics target
     * @param xMin
     *            the smallest x value
     * @param xMax
     *            the maximum x value
     * @param yMax
     *            the maximum y value
     * @return the next y value to use
     * @throws VizException
     */
    private double drawColorBar(IGraphicsTarget target, double xMin,
            double xMax, double yMax) throws VizException {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        double y1 = 0;
        double cmapHeight = textHeight;
        double legendHeight = cmapHeight + 2.0 * textSpace + 2.0 * padding;

        DrawableString description = new DrawableString(getDataInfo(),
                new RGB(250, 250, 0));

        if ((pcOptions.getQueryMode() == 1)
                && (pcOptions.getTsDataElement() != 0)
                && (pcOptions.getTsDataElement() != 1)) {

            AbstractVizResource<?, ?> rsc = HydroDisplayManager.getInstance()
                    .getMultiPointResource();
            ColorMapCapability cmapRsc = rsc
                    .getCapability(ColorMapCapability.class);
            DrawableColorMap cmap = new DrawableColorMap(
                    rsc.getCapability(ColorMapCapability.class)
                            .getColorMapParameters());
            cmap.alpha = 1.0f;

            // The y value to use for drawing
            y1 = yMax - legendHeight;

            /* color bar width */
            double width = (xMax - xMin) / 30.0
                    * cmap.getColorMapParams().getColorMap().getSize();

            /* blank out the area under the legend and labels */
            double legendWidth = target.getStringsBounds(description).getWidth()
                    + padding;
            PixelExtent legendExtent = new PixelExtent(xMin,
                    xMin + Math.max(width, legendWidth), y1, yMax);
            target.drawShadedRect(legendExtent, new RGB(0, 0, 0), 1.0, null);

            /* Draw the threshold values above the color bar */
            int offset = 0;
            double delta = width
                    / cmap.getColorMapParams().getColorMap().getSize();
            offset = (int) (cmapRsc.getColorMapParameters().getLabels().get(1)
                    .getLocation() * width / 2);
            List<DrawableString> strings = new ArrayList<>();
            for (int i = 0; i < cmap.getColorMapParams().getColorMap()
                    .getSize(); i++) {
                LabelEntry entry = cmapRsc.getColorMapParameters().getLabels()
                        .get(i);
                if (entry.getText().length() > 10) {
                    break;
                } else {
                    DrawableString string = new DrawableString(entry.getText(),
                            new RGB(255, 255, 255));
                    string.setCoordinates(xMin + offset + i * delta, y1);
                    string.horizontalAlignment = HorizontalAlignment.CENTER;
                    string.verticallAlignment = VerticalAlignment.TOP;
                    strings.add(string);
                }
            }
            target.drawStrings(strings);

            /* Draw the color ramp */
            y1 += textSpace;
            cmap.extent = new PixelExtent(xMin, xMin + width, y1,
                    y1 + cmapHeight);
            target.drawColorRamp(cmap);
        } else {
            y1 = yMax - legendHeight;
            y1 += textSpace;
        }

        /* Draw the informative text below the color bar */
        y1 += cmapHeight;
        description.setCoordinates(xMin + padding, y1);
        description.verticallAlignment = VerticalAlignment.TOP;
        target.drawStrings(description);

        return yMax - legendHeight;
    }

    private String getDataInfo() {
        String legendString = null;
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        PDCFileInfo fileInfo = PDCFileInfo.getInstance();
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        if (pcOptions.getQueryMode() == 0) {
            // Ad Hoc Mode
            legendString = "I'm a legend";
            int timeMode = pcOptions.getTimeMode();
            int primary = pcOptions.getPrimary();
            final String primaryString = "River - Primary PE";

            int pc_and_pp = pcOptions.getPcAndpp();
            final String pc_and_ppString = "Rain Total - PC AND PP";

            String elementString = null;

            PointControlPeTs petsdata = pdcManager.getPCPeTsData();
            int elementType = pcOptions.getElementType();

            if ((elementType >= 0)
                    && (elementType < petsdata.getElementCount())) {
                // set up the way to display the physical element string
                if (primary == 1) {
                    elementString = primaryString;
                } else if (pc_and_pp == 1) {
                    elementString = pc_and_ppString;
                } else {
                    elementString = pcOptions
                            .getSelectedAdHocElementFullString();
                }

                if (timeMode == TimeModeType.LATEST.getTimeMode()) {
                    // LATEST
                    if (elementType == AdHocDataElementType.RAIN_AD_HOC_TYPE
                            .getAdHocDataElementType()) {
                        // example RAIN - PC AND PP - Total Over last 4
                        // hours
                        Calendar currentTime = new GregorianCalendar();
                        currentTime.set(Calendar.MINUTE, 0);
                        currentTime.set(Calendar.SECOND, 0);

                        if (pcOptions.getDurHours() == 1) {
                            legendString = String.format(
                                    "%s over %d hour ending at latest hour (%s Z) ",
                                    elementString, pcOptions.getDurHours(),
                                    HydroConstants.DATE_FORMAT
                                            .format(currentTime.getTime()));
                        } else {
                            legendString = String.format(
                                    "%s over %d hours ending at latest hour (%s Z) ",
                                    elementString, pcOptions.getDurHours(),
                                    HydroConstants.DATE_FORMAT
                                            .format(currentTime.getTime()));
                        }
                    } else {
                        // not RAIN_AD_HOC_TYPE

                        // example River - Primary - Latest Value
                        legendString = String.format(
                                "%s - latest value within last %d hours",
                                elementString, pcOptions.getDurHours());
                    }
                } else if ((timeMode == TimeModeType.MINSELECT.getTimeMode())
                        || (timeMode == TimeModeType.MAXSELECT.getTimeMode())
                        || (timeMode == TimeModeType.VALUE_CHANGE
                                .getTimeMode())) {
                    if (elementType == AdHocDataElementType.RAIN_AD_HOC_TYPE
                            .getAdHocDataElementType()) {
                        // example RAIN - PC AND PP - value over 4 hours
                        // ending at 2006 03 17 12:00 Z
                        legendString = String.format(
                                "%s over %d hours ending at %s", elementString,
                                pcOptions.getDurHours(),
                                pcOptions.getPcTimeStr());
                    } else {
                        // not RAIN_AD_HOC_TYPE

                        // example River Primary PE - Min value over 4 hours
                        // ending at 2006 03 17 12:00 Z
                        legendString = String.format(
                                "%s - %s over %d hours ending at %s",
                                elementString, timeModeStrings[timeMode],
                                pcOptions.getDurHours(),
                                pcOptions.getPcTimeStr());
                    }
                } else {
                    // time_mode = SETTIME
                    if (elementType == AdHocDataElementType.RAIN_AD_HOC_TYPE
                            .getAdHocDataElementType()) {
                        // example RAIN - PC AND PP - Value over 4 hours
                        // ending at 2006 03 17 12:00 Z
                        legendString = String.format(
                                "%s over %d hours ending at %s", elementString,
                                pcOptions.getDurHours(),
                                pcOptions.getPcTimeStr());
                    } else {
                        // not RAIN_AD_HOC_TYPE
                        legendString = String.format("%s within %d hours of %s",
                                elementString, pcOptions.getDurHours(),
                                pcOptions.getPcTimeStr());
                    }
                }
            } else {
                // element out of range , happens upon startup
                statusHandler.handle(Priority.ERROR,
                        "getPDCAdHocLegendString(): element out of range");
                legendString = "";
            }
        } else {
            // Time Step Mode
            // HydroConstants.TimeStepDataElementType elementType
            int elementType = pcOptions.getElementType();
            int element = pcOptions.getTsDataElement();

            if ((element >= 0)
                    && (element < TimeStepDataElement.TIME_STEP_DATA_ELEMENT_COUNT
                            .getElementType())) {
                // element is in legal range
                if (element == HydroConstants.TimeStepDataElement.INSTANTANEOUS_PRECIP_TSDE
                        .getElementType()) {
                    int selection = pcOptions.getInstPrecipAccumTimeSelection();
                    if ((selection >= 0)
                            && (selection < HydroConstants.InstPrecipSelection.PRECIP_TIME_COUNT
                                    .getInstPrecipSelection())) {

                        Date creationTime = fileInfo
                                .getTimestepFileCreationTime();
                        String dateStr = HydroConstants.DATE_FORMAT
                                .format(creationTime);

                        legendString = String.format("%s - %s ending at %s Z",
                                TIME_STEP_STRINGS[element],
                                INST_PRECIP_STRINGS[selection], dateStr);
                    }
                } else if (elementType == HydroConstants.TimeStepDataElementType.RAIN_TIME_STEP_TYPE
                        .getTimeStepDataElementType()) {
                    // but not inst. precip
                    legendString = String.format("%s ending at %s Z",
                            TIME_STEP_STRINGS[element],
                            pcOptions.getPcTimeStr());
                } else if ((element == TimeStepDataElement.TEMP_MAX_TSDE
                        .getElementType())
                        || (element == TimeStepDataElement.TEMP_MIN_TSDE
                                .getElementType())) {
                    legendString = String.format("%s ending near %s Z",
                            TIME_STEP_STRINGS[element],
                            pcOptions.getPcTimeStr());
                } else {
                    legendString = String.format("%s for %s Z",
                            TIME_STEP_STRINGS[element],
                            pcOptions.getPcTimeStr());
                }
            } else {
                // element is out of range - this would be bad, should not
                // ever happen
                legendString = "Time Step Mode - ERROR";
            }
        }
        return legendString;
    }
}
