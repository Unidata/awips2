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
package com.raytheon.viz.hydro.resource;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension.PointImage;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.uf.viz.pdc.resource.HydroColorBarResource;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.data.GageDataTimeStep;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.datamanager.PDCDataManager;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.InputAdapter;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.index.strtree.STRtree;

/**
 * Multiple point resource.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jun 17, 2008  1194     M. Duff    Initial creation.
 * Nov 06, 2008  1628     D. Hladky  Made it work.
 * Sep 23, 2009  3069     mpduff     Changed the parent class to
 *                                   HydroPointResource.
 * Mar 20, 2010  4671     mpduff     Changed so the colors are updated
 *                                   dynamically.
 * Sep 14, 2010  5282     lbousaidi  reuse the open Time Series Control dialog
 *                                   each time a station is selected.
 * Jan 25, 2011  7881     mpduff     Fixed the double click station selection.
 * Jan 27, 2011  5109     bkowal     Fixed panning.
 * Jan 28, 2011  5274     bkowal     Whenever this resource is disposed, any
 *                                   PointDataControlManager jobs will now be
 *                                   canceled.
 * Apr 05, 2011  8910     jpiatt     Adjusted resource coordinates.
 * May 16, 2011  9356     djingtao   When timeseries is disposed, launch a new
 *                                   timesereis after double click or right
 *                                   click to select TimeSeries
 * Jan 30, 2013  15646    wkwock     Fix middle button drag map incorrect
 * Feb 05, 2013  1578     rferrel    Changes for non-blocking singleton
 *                                   TimeSeriesDlg.
 * Feb 18, 2014  2596     mpduff     Check for null coordinates.
 * Feb 02, 2015  4075     ccody      Added getSelectedGage for HS issue #3961
 * Mar 09, 2015  13998    lbousaidi  changed the dur display when it is null to
 *                                   match A1.
 * Apr 09, 2015  4215     mpduff     Check strTree before removing items.
 * Jun 26, 2015  17386    xwei       Fixed : HydroView crashes in when Refresh
 *                                   Data after loading saved display files
 * Jul 06, 2015  4215     mpduff     Correct the fact that user's cannot click
 *                                   and view time series.
 * Oct 05, 2015  17978    lbousaidi  Enable TimeStep GUI to display multiple
 *                                   values and Parameter Codes for a given lid
 * Nov 05, 2015  5070     randerso   Adjust font sizes for dpi scaling
 * Dec 05, 2015  18357    xwei       Fixed error in opening Timeseries for
 *                                   Timesteps
 * May 03, 2016  5623     bkowal     Fixed improper usage of {@link STRtree}.
 *                                   Cleanup.
 * Jul 11, 2016  19175    lbousaidi  changed the fontSize to 9 to match the
 *                                   default one.
 * Jul 29, 2016  5656     njensen    Fix NPEs when right-clicking off map
 * Oct 27, 2016  5969     randerso   Add support for locating hydroapps on the
 *                                   correct monitor
 * Feb 21, 2018  6918     mduff      Changed to call getFormattedGageValue() for
 *                                   inspection.
 * Apr 12, 2018  6619     randerso   Code cleanup.
 * Jul 30, 2018  6998     dgilling   Fix TimeSeriesLiteAction to handle multiple
 *                                   rain values at the same gage in TimeStep
 *                                   mode.
 * Sep 21, 2018  7379     mduff      Support PDC Refactor.
 *
 * </pre>
 *
 * @author M. Duff
 */

public class MultiPointResource extends AbstractMultiPointResource
        implements IContextMenuContributor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MultiPointResource.class);

    private final DecimalFormat df = new DecimalFormat();

    private final PDCDataManager dataManager = PDCDataManager.getInstance();

    private HydroColorBarResource colorBarResource = null;

    /**
     * Mouse event manager.
     */
    private HydroInputManager inputManager = null;

    private HydroDisplayManager manager = null;

    private PointDataControlManager pdcManager = null;

    /**
     * Constructor
     *
     * @param resourceData
     *            the {@link MultiPointResourceData} to use.
     * @param loadProperties
     *            the {@link LoadProperties} to use.
     */
    public MultiPointResource(MultiPointResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        pdcManager = PointDataControlManager.getInstance();
        manager = HydroDisplayManager.getInstance();
        pcOptions = PDCOptionData.getInstance();

        // Hide the change color and colormap menu items
        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);

        df.setMaximumFractionDigits(2);
        createColorMap();

        inputManager = new HydroInputManager();
    }

    @Override
    public String getName() {
        return ((MultiPointResourceData) resourceData).getName();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inputManager);
        }

        fontSize = 9;
        font = target.initializeFont("Dialog", fontSize, null);
        font.setSmoothing(false);

        manager.setMultiPointResource(this);
        List<GageData> data = manager.getObsReportList();

        resetDataMap();
        if (data != null) {
            for (GageData gage : data) {
                /* Get the point color for this location */
                if ((gage.getLid() != null) && gage.isUse()) {
                    if (pcOptions.getQueryMode() == 1) {
                        addPointTimeStep(gage);
                    } else {
                        addPoint(gage);
                    }
                }
            }
        }
    }

    /**
     * Add a point to this resource in TimeStep Mode.
     *
     * @param gage
     *            GageData object
     */
    private synchronized void addPointTimeStep(GageData gage) {
        String lid = gage.getLid();

        if (!dataMapTimeStep.containsKey(lid)) {
            Coordinate xy = new Coordinate(gage.getLon(), gage.getLat());
            gage.setCoordinate(xy);

            /* Create a small envelope around the point */
            PixelExtent pe = getPixelExtent(gage, getShiftWidth(gage),
                    getShiftHeight(gage));
            Envelope newEnv = descriptor.pixelToWorld(pe);

            GageDataTimeStep newGageTS = new GageDataTimeStep(gage);

            strTree.insert(newEnv, newGageTS);
            dataMapTimeStep.put(lid, newGageTS);
        } else {
            dataMapTimeStep.get(lid).update(gage);
        }
    }

    /**
     * Prepares the {@link DrawableString}s that describe the timestep data.
     *
     * @param gageTimeStep
     *            the {@link GageDataTimeStep} data
     * @param shiftWidth
     *            the shift width coordinate
     * @param shiftHeight
     *            the shift height coordinate
     * @param target
     *            the graphics target
     * @return the {@link DrawableString}s that are generated.
     * @throws VizException
     */
    private Collection<DrawableString> generateDrawableStringsTimeStep(
            GageDataTimeStep gageTimeStep, double shiftWidth,
            double shiftHeight, IGraphicsTarget target) throws VizException {
        List<DrawableString> strings = new ArrayList<>();
        Coordinate c = gageTimeStep.getCoordinate();

        /* Logic for determining how the data values are displayed. */
        boolean showValue1 = pdcManager.isValue();

        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });

        if (showValue1) {
            String[] valueStrings;

            if (pcOptions.getElementType() == 1) {
                valueStrings = gageTimeStep
                        .getRainValue(pcOptions.getPrecipPeFilter())
                        .split("\n");
            } else {
                valueStrings = gageTimeStep.getOtherValue().split("\n");
            }

            int strSize = valueStrings.length;

            RGB[] strColor = new RGB[strSize];

            if (strSize > 0) {
                for (int i = 0; i < strSize; i++) {
                    if (!valueStrings[i].isEmpty()) {
                        if (M.equalsIgnoreCase(valueStrings[i])) {
                            strColor[i] = RGBColors.getRGBColor("White");
                        } else {
                            strColor[i] = getValueLabelColorTimeStep(
                                    gageTimeStep.getLid(),
                                    Double.parseDouble(valueStrings[i]));
                        }
                    }
                }
            }

            Coordinate valueCoor = new Coordinate(
                    (centerpixels[0] + shiftWidth) - getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 2);

            /*
             * If in timestep mode and icon drawing off, draw a circle matching
             * the color of the text
             */
            if (pcOptions.getIcon() == 0) {
                Coordinate cd = gageTimeStep.getCoordinate();
                centerpixels = descriptor
                        .worldToPixel(new double[] { cd.x, cd.y });
                Coordinate[] coors = new Coordinate[4];
                coors[0] = new Coordinate(
                        (centerpixels[0] + shiftWidth) - getScaleWidth(),
                        (centerpixels[1] + shiftHeight) - getScaleHeight());
                coors[1] = new Coordinate(
                        (centerpixels[0] + shiftWidth) + getScaleWidth(),
                        (centerpixels[1] + shiftHeight) - getScaleHeight());
                coors[2] = new Coordinate(
                        (centerpixels[0] + shiftWidth) + getScaleWidth(),
                        (centerpixels[1] + shiftHeight) + getScaleHeight());
                coors[3] = new Coordinate(
                        (centerpixels[0] + shiftWidth) - getScaleWidth(),
                        (centerpixels[1] + shiftHeight) + getScaleHeight());

                PixelExtent pe = new PixelExtent(coors);
                pe.scale(.07);

                target.drawShadedRect(pe, RGBColors.getRGBColor("White"), 1,
                        null);
            }

            DrawableString string = new DrawableString(valueStrings, strColor);

            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;

            string.setCoordinates(valueCoor.x, valueCoor.y);
            strings.add(string);

        }

        if (pdcManager.isTime()) {
            Coordinate dateCoor1 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 0.9);
            Coordinate dateCoor2 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight() / -2);
            // draw the date and time
            DrawableString string = new DrawableString(
                    monthDayFormat.get()
                            .format(gageTimeStep.getValidtime().getTime()),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor1.x, dateCoor1.y);
            strings.add(string);

            string = new DrawableString(
                    hourMinuteFormat.get()
                            .format(gageTimeStep.getValidtime().getTime()),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor2.x, dateCoor2.y);
            strings.add(string);
        }
        // draw the ID
        if (pdcManager.isId()) {
            Coordinate idCoor = new Coordinate(
                    centerpixels[0] + shiftWidth - getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight());

            DrawableString string = new DrawableString(gageTimeStep.getLid(),
                    LABEL_COLOR);
            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;
            string.setCoordinates(idCoor.x, idCoor.y);
            strings.add(string);
        }
        if (pdcManager.isName()) {
            // draw the Name
            Coordinate nameCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight());

            DrawableString string = new DrawableString(gageTimeStep.getName(),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(nameCoor.x, nameCoor.y);
            strings.add(string);
        }

        if (pdcManager.isPe()) {
            String pe = "";
            if (pcOptions.getElementType() == 1) {
                pe = gageTimeStep.getRainParam(pcOptions.getPrecipPeFilter());
            } else {
                pe = gageTimeStep.getOtherParam();
            }

            Coordinate peCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight - getScaleHeight() / 2);
            DrawableString string = new DrawableString(pe, LABEL_COLOR);
            string.font = font;
            string.setCoordinates(peCoor.x, peCoor.y);
            strings.add(string);
        }

        if (pdcManager.isElevation()) {
            // draw the elevation
            Coordinate elCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight - getScaleHeight() / 2);

            DrawableString string = new DrawableString(
                    df.format(gageTimeStep.getElevation()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(elCoor.x, elCoor.y);
            strings.add(string);
        }
        return strings;
    }

    /**
     * Gets the color for value label in TimeStep mode
     *
     * @param plid
     *            lid string
     *
     * @param pValue
     *            value
     *
     * @throws VizException
     */
    private RGB getValueLabelColorTimeStep(String pLid, double pValue) {

        RGB textColor = RGBColors.getRGBColor("White");

        if ((pcOptions
                .getTsDataElement() == HydroConstants.TimeStepDataElement.STAGE_POOL_TSDE
                        .getElementType())
                || (pcOptions
                        .getTsDataElement() == HydroConstants.TimeStepDataElement.FLOW_STORAGE_TSDE
                                .getElementType())) {
            textColor = getRiverValueColorForTimeStepMode(pLid, pValue);
        } else {
            textColor = determineValueColor(pValue);
        }

        return textColor;
    }

    /**
     * Paint method called to display this resource.
     *
     * @param target
     *            The IGraphicsTarget
     * @param paintProps
     *            The Paint Properties
     * @throws VizException
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Check the font size
        font.setMagnification((manager.getFontSize() / (float) fontSize), true);

        /*
         * Only display the color bar in TimeStep mode and if there are data on
         * the screen and not for STAGE/POOL or FLOW/STORAGE. When to display
         * the actual color bar is in the HydroColorBarResource.paintInternal
         * method.
         */
        if ((manager.isDataChanged()) || (colorBarResource == null)) {
            createColorMap();

            // Get color bar
            ResourceList rl = descriptor.getResourceList();
            ResourceProperties props = new ResourceProperties();
            props.setSystemResource(true);

            if (colorBarResource == null) {
                colorBarResource = new HydroColorBarResource();

                props.setSystemResource(true);
                rl.add(colorBarResource, props);
            }

            manager.setDataChanged(false);
        }

        setScaleValues(paintProps);

        resetDataMap();

        if (pcOptions.getQueryMode() == 1) {
            paintInternalHelperTimeStep(target, paintProps);
        } else {
            paintInternalHelper(target, paintProps);
        }

        GageData currentData = manager.getCurrentData();
        if (currentData != null) {
            List<GageData> siteList = manager.getObsReportList();
            if ((siteList != null) && siteList.contains(currentData)) {
                double shiftHeightValue = getShiftHeight(currentData);
                double shiftWidthValue = getShiftWidth(currentData);

                PixelExtent pe = getPixelExtent(currentData, shiftWidthValue,
                        shiftHeightValue);
                target.drawRect(pe, HydroConstants.SQUARE_COLOR, 2, 1);
            }
        }
    }

    /**
     * Paint method called to display this resource in TimeStep mode.
     *
     * @param target
     *            The IGraphicsTarget
     * @param paintProps
     *            The Paint Properties
     * @throws VizException
     */
    private void paintInternalHelperTimeStep(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        List<GageData> data = manager.getObsReportList();

        if (data == null || data.isEmpty()) {
            return;
        }

        for (GageData gage : data) {
            if ((gage.getLid() != null) && gage.isUse()) {
                addPointTimeStep(gage);
            }
        }

        IExtent extent = paintProps.getView().getExtent();

        List<PointImage> images = new ArrayList<>(dataMapTimeStep.size());
        List<DrawableString> strings = new ArrayList<>(
                dataMapTimeStep.size() * 3);

        Iterator<Entry<String, GageDataTimeStep>> it = dataMapTimeStep
                .entrySet().iterator();
        Map.Entry<String, GageDataTimeStep> gageTS = null;
        while (it.hasNext()) {
            gageTS = it.next();

            Coordinate c = gageTS.getValue().getCoordinate();
            double[] pixel = descriptor.worldToPixel(new double[] { c.x, c.y });

            if (pixel != null && extent.contains(pixel)) {
                double shiftHeightValue = getShiftHeight(gageTS.getValue());
                double shiftWidthValue = getShiftWidth(gageTS.getValue());
                /* Draw the icons */
                if (pcOptions.getIcon() == 1) {
                    RGB color = null;
                    if (pcOptions.getRiverStatus() == 1) {
                        color = gageTS.getValue().getColor();
                    } else {
                        color = RGBColors.getRGBColor(
                                colorSet.get(0).getColorname().getColorName());
                    }
                    PointImage image = new PointImage(
                            getIcon(target, gageTS.getValue(), color), pixel[0],
                            pixel[1]);

                    image.setSiteId(gageTS.getValue().getLid());
                    images.add(image);
                }
                strings.addAll(
                        generateDrawableStringsTimeStep(gageTS.getValue(),
                                shiftWidthValue, shiftHeightValue, target));
            }

        }

        if (!images.isEmpty()) {
            target.getExtension(IPointImageExtension.class)
                    .drawPointImages(paintProps, images);
        }
        if (!strings.isEmpty()) {
            target.drawStrings(strings);
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            Envelope env = new Envelope(coord.asLatLon());
            List<?> elements = strTree.query(env);
            if (!elements.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                boolean first = true;
                Iterator<?> iter = elements.iterator();
                while (iter.hasNext()) {
                    GageData gage = (GageData) iter.next();
                    if (!first) {
                        sb.append("\n");
                    }
                    sb.append("GAGE: ").append(gage.getName())
                            .append(" VALUE: ")
                            .append(gage.getFormattedGageValue());
                    first = false;
                }
                return sb.toString();
            }
        } catch (Exception e) {
            throw new VizException(e);
        }
        return null;
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate rcoord)
            throws VizException {
        List<GageData> gageDataList = manager.getObsReportList();

        try {
            GageData selected = null;
            Coordinate coord = rcoord.asLatLon();
            double minDistance = 9999;

            double[] selectedPoint = descriptor
                    .worldToPixel(new double[] { coord.x, coord.y });

            if ((gageDataList != null) && (!gageDataList.isEmpty())) {
                for (GageData gd : gageDataList) {
                    if (gd.isUse() && gd.getCoordinate() != null) {
                        double[] gagePoint = descriptor.worldToPixel(
                                new double[] { gd.getCoordinate().x,
                                        gd.getCoordinate().y });
                        double xDist = Math
                                .abs(selectedPoint[0] - gagePoint[0]);
                        double yDist = Math
                                .abs(selectedPoint[1] - gagePoint[1]);
                        double distance = Math.hypot(xDist, yDist);
                        if (distance < minDistance) {
                            minDistance = distance;
                            selected = gd;
                        }
                    }
                }
            }

            issueRefresh();
            if (selected != null) {
                manager.setCurrentData(selected);
            }
        } catch (Exception e) {
            throw new VizException(e);
        }
        return null;
    }

    private RGB getRiverValueColorForTimeStepMode(String pLid, double pValue) {
        RGB color = null;
        String threatIndex = ThreatIndex.THREAT_MISSING_DATA.getThreatIndex();

        double actionLevel = HydroConstants.MISSING_VALUE;
        double floodLevel = HydroConstants.MISSING_VALUE;

        /* Get the river status object for this lid */
        RiverStat riverStat = dataManager.getRiverStatus(pLid);

        int selectedTimeStepElement = pcOptions.getTsDataElement();

        // set actionLevel and floodLevel
        if (riverStat != null) {
            if (selectedTimeStepElement == HydroConstants.TimeStepDataElement.STAGE_POOL_TSDE
                    .getElementType()) {
                actionLevel = riverStat.getAs();
                floodLevel = riverStat.getFs();
            } else if (selectedTimeStepElement == HydroConstants.TimeStepDataElement.FLOW_STORAGE_TSDE
                    .getElementType()) {
                actionLevel = riverStat.getAq();
                floodLevel = riverStat.getFq();
            } else {
                actionLevel = riverStat.getAs();
                floodLevel = riverStat.getFs();
            }
        }

        // determine the threat level
        if ((pValue) != HydroConstants.MISSING_VALUE) {
            threatIndex = ThreatIndex.THREAT_MISSING_STAGE.getThreatIndex();

            if ((actionLevel != HydroConstants.MISSING_VALUE)
                    && (actionLevel != 0)) {
                if (pValue >= actionLevel) {
                    threatIndex = ThreatIndex.THREAT_ACTION.getThreatIndex();
                } else {
                    threatIndex = ThreatIndex.THREAT_NONE.getThreatIndex();
                }
            }

            if ((floodLevel != HydroConstants.MISSING_VALUE)
                    && (floodLevel != 0)) {
                if (pValue >= floodLevel) {
                    threatIndex = ThreatIndex.THREAT_FLOOD.getThreatIndex();
                } else if (actionLevel == HydroConstants.MISSING_VALUE) {
                    threatIndex = ThreatIndex.THREAT_NONE.getThreatIndex();
                }
            }
        } else {
            // current data was missing
            threatIndex = ThreatIndex.THREAT_MISSING_DATA.getThreatIndex();
        }

        if (threatIndex.equalsIgnoreCase(
                ThreatIndex.THREAT_MISSING_DATA.getThreatIndex())) {
            color = RGBColors
                    .getRGBColor(colorSet.get(0).getColorname().getColorName());
        } else if (threatIndex.equalsIgnoreCase(
                ThreatIndex.THREAT_MISSING_STAGE.getThreatIndex())) {
            color = RGBColors
                    .getRGBColor(colorSet.get(1).getColorname().getColorName());
        } else if (threatIndex
                .equalsIgnoreCase(ThreatIndex.THREAT_NONE.getThreatIndex())) {
            color = RGBColors
                    .getRGBColor(colorSet.get(2).getColorname().getColorName());
        } else if (threatIndex
                .equalsIgnoreCase(ThreatIndex.THREAT_ACTION.getThreatIndex())) {
            color = RGBColors
                    .getRGBColor(colorSet.get(3).getColorname().getColorName());
        } else if (threatIndex
                .equalsIgnoreCase(ThreatIndex.THREAT_FLOOD.getThreatIndex())) {
            color = RGBColors
                    .getRGBColor(colorSet.get(4).getColorname().getColorName());
        } else {
            color = RGBColors
                    .getRGBColor(colorSet.get(0).getColorname().getColorName());
        }

        return color;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        menuManager.add(new Separator());
        menuManager.add(new TimeSeriesLiteAction());
        menuManager.add(new TimeSeriesLaunchAction());
        menuManager.add(new Separator());
        menuManager.add(new Separator());
        menuManager.add(new Separator());
        menuManager.add(new Separator());
    }

    /**
     * Determine the color corresponding to the passed in value.
     *
     * @param value
     *            The value that needs the corresponding color
     * @return the corresponding color
     */
    public RGB determineValueColor(double value) {
        int i = 0;
        RGB rgb = null;
        for (DataMappingEntry entry : dmPref.getEntries()) {
            if (entry.getDisplayValue() != null) {
                if (value == entry.getDisplayValue()) {
                    rgb = convertColor(colorMap.getColors().get(i));
                    break;
                } else if (value < entry.getDisplayValue()) {
                    rgb = convertColor(colorMap.getColors().get(i - 1));
                    break;
                }
            }
            i++;
        }
        if (rgb == null) {
            i = dmPref.getEntries().size();
            rgb = convertColor(colorMap.getColors().get(i - 1));
        }

        return rgb;
    }

    private class TimeSeriesLaunchAction extends AbstractRightClickAction {
        @Override
        public String getText() {
            return "Timeseries";
        }

        @Override
        public void run() {
            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();

                int x = pane.getLastMouseX();
                int y = pane.getLastMouseY();

                Coordinate coord = container.translateClick(x, y);
                if (coord == null) {
                    showMessage();
                    return;
                }

                Envelope env = new Envelope(coord);
                List<?> elements = strTree.query(env);
                GageData closestGage = getNearestPoint(coord, elements);
                if (closestGage != null) {
                    TimeSeriesDlg ts = TimeSeriesDlg.getInstance();
                    ts.open();
                    ts.updateSelection(closestGage, true);
                } else {
                    showMessage();
                }
            }
        }
    }

    private class TimeSeriesLiteAction extends AbstractRightClickAction {

        private static final String TSL_BUNDLE_LOC = "bundles"
                + IPathManager.SEPARATOR + "run-TimeSeriesLite.xml";

        public TimeSeriesLiteAction() {
            super("Timeseries Lite");
        }

        @Override
        public void runWithEvent(Event event) {
            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();

                int x = pane.getLastMouseX();
                int y = pane.getLastMouseY();

                Coordinate coord = container.translateClick(x, y);
                if (coord == null) {
                    showMessage();
                    return;
                }

                Envelope env = new Envelope(coord);
                List<?> elements = strTree.query(env);
                if (!elements.isEmpty()) {
                    GageData gageData = getNearestPoint(coord, elements);
                    if ((gageData != null)) {
                        String lid = gageData.getLid();
                        if (lid != null) {
                            String paramCode = getParamCode(gageData);

                            statusHandler
                                    .debug("TimeSeriesLiteAction: paramCode = ["
                                            + paramCode + "]");

                            if (paramCode.contains("-")) {
                                String message = String.format(
                                        "This location's paramCode, %s, is incomplete.\n"
                                                + "TimeSeriesLite cannot be launched for it. \n"
                                                + "If you are in The Point Data Control's Ad Hoc Mode,\n"
                                                + "try switching to Time-Step Mode for this feature. ",
                                        paramCode);
                                MessageDialog.openInformation(
                                        event.display.getActiveShell(), "",
                                        message);
                                return;
                            }

                            String otherParamCode = getFcstParamCode(lid,
                                    paramCode);

                            statusHandler
                                    .debug("TimeSeriesLiteAction: paramCode = ["
                                            + paramCode + "] otherParamCode = ["
                                            + otherParamCode + "]");

                            try {
                                AppLauncherHandler alh = new AppLauncherHandler();
                                Shell shell = event.display.getActiveShell();
                                alh.execute(shell, TSL_BUNDLE_LOC, lid,
                                        paramCode, otherParamCode);
                            } catch (ExecutionException e) {
                                statusHandler.error(e.getLocalizedMessage(), e);
                            }
                        } else {
                            String message = "To launch TimeSeriesLite, you must first click near the gage you are concerned with.";
                            MessageDialog.openInformation(
                                    event.display.getActiveShell(), "",
                                    message);
                        }
                    } else {
                        showMessage();
                    }
                } else {
                    showMessage();
                }
            } else {
                showMessage();
            }
        }

        private String getParamCode(GageData gageData) {
            if (gageData instanceof GageDataTimeStep) {
                GageDataTimeStep casted = (GageDataTimeStep) gageData;
                if (!StringUtil.isEmptyString(casted.getPpParam())) {
                    /*
                     * we could have multiple param codes that start with PP in
                     * the PP param code field, so we take the last as that
                     * matches the A1 logic.
                     */
                    String[] ppParams = casted.getPpParam()
                            .split("\\p{Space}+");
                    return ppParams[ppParams.length - 1];
                }
            } else if (gageData.getParamCode().startsWith("PC")) {
                String origParamCode = gageData.getParamCode();
                return origParamCode.substring(0, 2) + "I"
                        + origParamCode.substring(3);
            }

            return gageData.getParamCode();
        }

        private String getFcstParamCode(final String lid,
                final String paramCode) {
            String pe = paramCode.substring(0, 2);

            String fcstParamCode = String.format("%sIF-Z", pe);
            return fcstParamCode;
        }
    }

    private void showMessage() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
        mb.setText("Error");
        mb.setMessage(
                "The mouse pointer must be on a gage to use this feature.");
        mb.open();
    }

    /**
     * Unmap the data.
     */
    @Override
    public void unmap() {
        ResourceList rl = descriptor.getResourceList();
        if (rl.containsRsc(colorBarResource)) {
            rl.removeRsc(colorBarResource);
            colorBarResource.dispose();
            colorBarResource = null;
        }
    }

    private class HydroInputManager extends InputAdapter {
        @Override
        public boolean handleDoubleClick(int x, int y, int button) {
            IDisplayPaneContainer container = getResourceContainer();
            ReferencedCoordinate latLon = new ReferencedCoordinate(
                    container.translateClick(x, y));
            if (button == 1) {
                try {
                    interrogate(latLon);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    return false;
                }
            } else if (button == 2) {
                if (container != null) {
                    Coordinate coord = container.translateClick(x, y);

                    Envelope env = new Envelope(coord);
                    List<?> elements = strTree.query(env);
                    if (!elements.isEmpty()) {
                        Iterator<?> iter = elements.iterator();
                        /* Take the first one in the list */
                        if (iter.hasNext()) {
                            /* element 0 = Coordinate, 1 = inspectString */
                            GageData gage = (GageData) iter.next();

                            TimeSeriesDlg ts = TimeSeriesDlg.getInstance();
                            ts.open();
                            ts.updateSelection(gage, false);
                            try {
                                interrogate(latLon);
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                                return false;
                            }
                        }
                    } else {
                        showMessage();
                    }
                }
            }
            return true;

        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 2) {
                return true;
            }
            return false;
        }
    }

    @Override
    protected void disposeInternal() {
        for (Map<RGB, IImage> colorMap : imageMap.values()) {
            for (IImage image : colorMap.values()) {
                image.dispose();
            }
            colorMap.clear();
        }
        imageMap.clear();
        font.dispose();
        resetDataMap();

        manager.setDrawStation(false);

        pdcManager.cancelRunningJobs();
        unmap();

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inputManager);
        }
    }
}
