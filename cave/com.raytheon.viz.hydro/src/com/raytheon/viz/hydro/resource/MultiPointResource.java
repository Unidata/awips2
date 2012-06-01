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

import java.awt.image.RenderedImage;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.hydro.colorbar.HydroColorBarResource;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker.ImageSize;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.pointdatacontrol.util.PDCUtils;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThreshold;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThresholdArray;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.HydroViewColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Multiple point resource.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 17, 2008 1194        M. Duff     Initial creation.
 * Nov 06, 2008 1628        D. Hladky   Made it work.
 * Sep 23, 2009 3069        mpduff      Changed the parent class to HydroPointResource.
 * Mar 20 2010  4671        mpduff      Changed so the colors are updated dynamically.
 * Sep 14 2010  5282		lbousaidi   reuse the open Time Series Control dialog 
 * 										each time a station is selected.
 * Jan 25, 2011 7881        mpduff      Fixed the double click station selection.
 * Jan 27, 2011 5109        bkowal      Fixed panning.
 * Jan 28, 2011 5274        bkowal      Whenever this resource is disposed, any
 *                                      PointDataControlManager jobs will
 *                                      now be canceled.
 * Apr 5, 2011  8910        jpiatt      Adjusted resource coordinates.
 * 
 * May 16, 2011 9356        djingtao    When timeseries is disposed, launch a new timesereis after double click
 *                                      or right click to select TimeSeries
 * 
 * </pre>
 * 
 * @author M. Duff
 * @version 1.0
 */

public class MultiPointResource extends
        AbstractVizResource<MultiPointResourceData, IMapDescriptor> implements
        IContextMenuContributor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MultiPointResource.class);

    private static class HydroImageMakerCallback implements
            IRenderedImageCallback {

        private String dispClass;

        private RGB color;

        private HydroImageMakerCallback(String dispClass, RGB color) {
            this.dispClass = dispClass;
            this.color = color;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IRenderedImageCallback#getImage()
         */
        @Override
        public RenderedImage getImage() throws VizException {
            return HydroImageMaker.getImage(dispClass, ImageSize.MEDIUM, color);
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((color == null) ? 0 : color.hashCode());
            result = prime * result
                    + ((dispClass == null) ? 0 : dispClass.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            HydroImageMakerCallback other = (HydroImageMakerCallback) obj;
            if (color == null) {
                if (other.color != null)
                    return false;
            } else if (!color.equals(other.color))
                return false;
            if (dispClass == null) {
                if (other.dispClass != null)
                    return false;
            } else if (!dispClass.equals(other.dispClass))
                return false;
            return true;
        }

    }

    private static final RGB LABEL_COLOR = RGBColors.getRGBColor("White");

    private Map<String, Map<RGB, IImage>> imageMap = new HashMap<String, Map<RGB, IImage>>();

    private Map<String, GageData> dataMap = new HashMap<String, GageData>();

    private STRtree strTree = new STRtree();

    private IFont font;

    private int fontSize;

    private final DecimalFormat df = new DecimalFormat();

    private final SimpleDateFormat sdf1 = new SimpleDateFormat();

    private final SimpleDateFormat sdf2 = new SimpleDateFormat();

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private double screenToWorldWidthRatio = 0.0;

    private double screenToWorldHeightRatio = 0.0;

    private final PDCDataManager dataManager = PDCDataManager.getInstance();

    private HydroColorBarResource colorBarResource = null;

    private ColorMap colorMap = null;

    private TimeSeriesDlg ts;

    /**
     * List of color value objects.
     */
    private ArrayList<Colorvalue> colorSet = null;

    /**
     * The DataMappingPreferences.
     */
    private DataMappingPreferences dmPref = null;

    /**
     * Mouse event manager.
     */
    private HydroInputManager inputManager = null;

    private PDCOptionData pcOptions = null;

    private HydroDisplayManager manager = null;

    private PointDataControlManager pdcManager = null;

    /**
     * Constructor.
     * 
     * @param name
     *            Resource name
     * @param color
     *            Resource color
     * @param coord
     *            Resource Coordinate
     * @param style
     *            Resource Style
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
        sdf1.applyPattern("MM/dd");
        sdf2.applyPattern("HH:mm");
        sdf1.getCalendar().setTimeZone(TimeZone.getTimeZone("GMT"));
        sdf2.getCalendar().setTimeZone(TimeZone.getTimeZone("GMT"));

        createColorMap();

        inputManager = new HydroInputManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return resourceData.getName();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inputManager);
        }

        fontSize = 10;
        font = target.initializeFont("Dialog", fontSize, null);
        font.setSmoothing(false);

        String colorUseName = HydroViewColors
                .getColorUseNameFromPcOptions(pcOptions);
        pdcManager.setColorUseName(colorUseName);
        pdcManager.setMultiPointResource(this);
        List<GageData> data = pdcManager.getObsReportList();
        resetDataMap();
        if (data != null) {
            for (GageData gage : data) {
                /* Get the point color for this location */
                if ((gage.getLid() != null) && gage.isUse()) {
                    addPoint(gage);
                }
            }
        }
    }

    /**
     * Add a point to this resource.
     * 
     * @param gage
     *            GageData object
     */
    private synchronized void addPoint(GageData gage) {
        String lid = gage.getLid();
        GageData existing = dataMap.get(lid);
        if (existing != gage) {
            Coordinate xy = new Coordinate(gage.getLon(), gage.getLat());
            gage.setCoordinate(xy);

            double latInc = .05;
            double lonInc = .03;

            if (existing != null) {
                Coordinate p1 = new Coordinate(existing.getLon() + lonInc,
                        existing.getLat() + latInc);
                Coordinate p2 = new Coordinate(existing.getLon() - lonInc,
                        existing.getLat() - latInc);
                Envelope oldEnv = new Envelope(p1, p2);
                strTree.remove(oldEnv, existing);
            }

            /* Create a small envelope around the point */
            Coordinate p1 = new Coordinate(gage.getLon() + lonInc,
                    gage.getLat() + latInc);
            Coordinate p2 = new Coordinate(gage.getLon() - lonInc,
                    gage.getLat() - latInc);
            Envelope newEnv = new Envelope(p1, p2);

            strTree.insert(newEnv, gage);
            dataMap.put(lid, gage);
        }
    }

    /**
     * Creates data structure for keeping the buffered images.
     * 
     * @param gage
     */
    private IImage getIcon(IGraphicsTarget target, GageData gage, RGB color) {
        String dispClass = gage.getDispClass();
        Map<RGB, IImage> colorMap = imageMap.get(dispClass);
        if (colorMap == null) {
            colorMap = new HashMap<RGB, IImage>();
            imageMap.put(dispClass, colorMap);
        }
        IImage image = colorMap.get(color);
        if (image == null) {
            image = target.initializeRaster(new HydroImageMakerCallback(
                    dispClass, color));
            colorMap.put(color, image);
        }
        return image;
    }

    /**
     * Gets the pixel extent of the rectangle
     * 
     * @param data
     *            the gage data obj
     * @param shiftWidth
     *            the shift width coordinate
     * @param shiftHeight
     *            the shift height coordinate
     * 
     * @return PixelExtent
     */
    private PixelExtent getPixelExtent(GageData data, double shiftWidth,
            double shiftHeight) {
        Coordinate c = data.getCoordinate();
        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });
        Coordinate[] coors = new Coordinate[4];
        coors[0] = new Coordinate((centerpixels[0] + shiftWidth)
                - getScaleWidth(), (centerpixels[1] + shiftHeight)
                - getScaleHeight());
        coors[1] = new Coordinate((centerpixels[0] + shiftWidth)
                + getScaleWidth(), (centerpixels[1] + shiftHeight)
                - getScaleHeight());
        coors[2] = new Coordinate((centerpixels[0] + shiftWidth)
                + getScaleWidth(), (centerpixels[1] + shiftHeight)
                + getScaleHeight());
        coors[3] = new Coordinate((centerpixels[0] + shiftWidth)
                - getScaleWidth(), (centerpixels[1] + shiftHeight)
                + getScaleHeight());
        return new PixelExtent(coors);
    }

    /**
     * Draws the plot information
     * 
     * @param data
     *            the gage data obj
     * @param shiftWidth
     *            the shift width coordinate
     * @param shiftHeight
     *            the shift height coordinate
     * @param paintProps
     *            the paint properties
     * @param target
     *            the graphics target
     * @throws VizException
     */
    private Collection<DrawableString> drawPlotInfo(GageData gage,
            double shiftWidth, double shiftHeight, PaintProperties paintProps,
            IGraphicsTarget target) throws VizException {
        List<DrawableString> strings = new ArrayList<DrawableString>();
        Coordinate c = gage.getCoordinate();

        int floodLevel = pcOptions.getFloodLevel();
        int deriveStageFlow = pcOptions.getDeriveStageFlow();

        boolean isTimeStepMode = false;

        String valueLabel = null;
        String formatStr = null;

        int queryMode = pcOptions.getQueryMode();

        if (queryMode == 1) {
            // TimeStep Mode
            isTimeStepMode = true;
        }

        formatStr = getDataFormat(gage.getPe());

        /* Logic for determining how the data values are displayed. */
        boolean showValue1 = pdcManager.isValue();
        boolean showValue2 = false;
        if (!showValue1) {
            showValue2 = false;
        } else {
            if (((floodLevel == 1) || (deriveStageFlow == 1))
                    && (pcOptions.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                            .getAdHocDataElementType())) {
                showValue2 = true;
                if (pcOptions.getQueryMode() == PDCConstants.QueryMode.TIME_STEP_MODE
                        .getQueryMode()) {
                    // never show value2 in TimeStep Mode
                    showValue2 = false;
                }
            }
        }

        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });

        if (showValue1) {
            RGB textColor = RGBColors.getRGBColor("White");
            if (gage.getGageValue() == PDCConstants.MISSING_VALUE) {
                valueLabel = "M";
            } else {
                valueLabel = String.format(formatStr, gage.getGageValue());
            }

            Coordinate valueCoor = new Coordinate(
                    (centerpixels[0] + shiftWidth) - getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 2);

            // Color text based on value and thresholds
            if (isTimeStepMode) {
                if ((pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.STAGE_POOL_TSDE
                        .getElementType())
                        || (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.FLOW_STORAGE_TSDE
                                .getElementType())) {
                    textColor = getRiverValueColorForTimeStepMode(gage);
                } else {
                    // textColor = new RGB(255, 255, 255);
                    textColor = determineValueColor(gage.getValue());
                }

                /*
                 * If in timestep mode and icon drawing off, draw a circle
                 * matching the color of the text
                 */
                if (pcOptions.getIcon() == 0) {
                    Coordinate cd = gage.getCoordinate();
                    centerpixels = descriptor.worldToPixel(new double[] { cd.x,
                            cd.y });
                    Coordinate[] coors = new Coordinate[4];
                    coors[0] = new Coordinate((centerpixels[0] + shiftWidth)
                            - getScaleWidth(), (centerpixels[1] + shiftHeight)
                            - getScaleHeight());
                    coors[1] = new Coordinate((centerpixels[0] + shiftWidth)
                            + getScaleWidth(), (centerpixels[1] + shiftHeight)
                            - getScaleHeight());
                    coors[2] = new Coordinate((centerpixels[0] + shiftWidth)
                            + getScaleWidth(), (centerpixels[1] + shiftHeight)
                            + getScaleHeight());
                    coors[3] = new Coordinate((centerpixels[0] + shiftWidth)
                            - getScaleWidth(), (centerpixels[1] + shiftHeight)
                            + getScaleHeight());

                    PixelExtent pe = new PixelExtent(coors);
                    pe.scale(.4);

                    target.drawShadedRect(pe, textColor, 1, null);
                }
            } else { // in AD_HOC_MODE, color the text labelColor
                textColor = RGBColors.getRGBColor("white");
            }

            DrawableString string = new DrawableString(valueLabel, textColor);
            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;
            string.setCoordinates(valueCoor.x, valueCoor.y);
            strings.add(string);

            if (pcOptions.getTimeMode() != PDCConstants.TimeModeType.VALUE_CHANGE
                    .getTimeMode()) {
                if (showValue2) {
                    String valueLabel2 = null;
                    if (gage.getGageValue2() != PDCConstants.MISSING_VALUE) {
                        /*
                         * Determine the format that value2 should be displayed
                         * as. Use the format string for value1 except in the
                         * case where value2 represents a derived flow.
                         */
                        if (gage.getPe().equalsIgnoreCase("HG")
                                && (deriveStageFlow == 1)) {
                            valueLabel2 = String.format("%6.0f",
                                    gage.getValue2());
                        } else if (gage.getPe().equalsIgnoreCase("QR")
                                && (deriveStageFlow == 1)) {
                            valueLabel2 = String.format("%6.2f",
                                    gage.getValue2());
                        } else {
                            valueLabel2 = String.format(formatStr,
                                    gage.getValue2());
                        }
                    } else {
                        valueLabel2 = "M";
                    }

                    valueCoor = new Coordinate((centerpixels[0] + shiftWidth)
                            - getScaleWidth(), (centerpixels[1] + shiftHeight)
                            + getScaleHeight() / -0.9);

                    string = new DrawableString(valueLabel2, textColor);
                    string.font = font;
                    string.horizontalAlignment = HorizontalAlignment.RIGHT;
                    string.setCoordinates(valueCoor.x, valueCoor.y);
                    strings.add(string);
                }
            }
        }

        if (pdcManager.isTime()) {
            Coordinate dateCoor1 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 0.9);
            Coordinate dateCoor2 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight() / -2);
            // draw the date and time
            DrawableString string = new DrawableString(sdf1.format(gage
                    .getValidtime().getTime()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor1.x, dateCoor1.y);
            strings.add(string);

            string = new DrawableString(sdf2.format(gage.getValidtime()
                    .getTime()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor2.x, dateCoor2.y);
            strings.add(string);
        }
        // draw the ID
        if (pdcManager.isID()) {
            Coordinate idCoor = new Coordinate(centerpixels[0] + shiftWidth
                    - getScaleWidth(), centerpixels[1] + shiftHeight
                    + getScaleHeight());

            DrawableString string = new DrawableString(gage.getLid(),
                    LABEL_COLOR);
            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;
            string.setCoordinates(idCoor.x, idCoor.y);
            strings.add(string);
        }
        if (pdcManager.isName()) {
            // draw the Name
            Coordinate nameCoor = new Coordinate(centerpixels[0] + shiftWidth
                    + getScaleWidth(), centerpixels[1] + shiftHeight
                    + getScaleHeight());

            DrawableString string = new DrawableString(gage.getName(),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(nameCoor.x, nameCoor.y);
            strings.add(string);
        }

        if (pdcManager.isPE()) {
            String shefDurCode;
            if (gage.getPe().equalsIgnoreCase("PC")) {
                /*
                 * PC is always "I", but sometimes the duration might have been
                 * screwed up
                 */
                shefDurCode = "I";
            } else {
                shefDurCode = PDCUtils.convertDur((int) gage.getDur());
            }
            String pe = gage.getPe() + shefDurCode + gage.getTs()
                    + gage.getExtremum();

            Coordinate peCoor = new Coordinate(centerpixels[0] + shiftWidth
                    + getScaleWidth(), centerpixels[1] + shiftHeight
                    - getScaleHeight() / 2);
            DrawableString string = new DrawableString(pe, LABEL_COLOR);
            string.font = font;
            string.setCoordinates(peCoor.x, peCoor.y);
            strings.add(string);
        }

        if (pdcManager.isElevation()) {
            // draw the elevation
            Coordinate elCoor = new Coordinate(centerpixels[0] + shiftWidth
                    + getScaleWidth(), centerpixels[1] + shiftHeight
                    - getScaleHeight() / 2);

            DrawableString string = new DrawableString(df.format(gage
                    .getElevation()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(elCoor.x, elCoor.y);
            strings.add(string);
        }
        return strings;
    }

    private void setScaleValues(PaintProperties props) {
        screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        setScaleWidth(props);
        setScaleHeight(props);
    }

    /**
     * Set the width scalar
     * 
     * @param props
     *            the paint properties
     */
    private void setScaleWidth(PaintProperties props) {
        scaleWidthValue = (ImageSize.MEDIUM.getWidth() / 2.0)
                / screenToWorldWidthRatio;
    }

    /**
     * get the scale width value
     * 
     * @return scale width value
     */
    private double getScaleWidth() {
        return scaleWidthValue;
    }

    /**
     * Set the height scalar
     * 
     * @param props
     *            the paint properties
     */
    private void setScaleHeight(PaintProperties props) {
        scaleHeightValue = (ImageSize.MEDIUM.getHeight() / 2.0)
                / screenToWorldHeightRatio;
    }

    /**
     * Get the scalar height
     * 
     * @return scale height value
     */
    private double getScaleHeight() {
        return scaleHeightValue;
    }

    /**
     * Get the x direction shift value.
     * 
     * @param props
     *            The PaintProperties object
     * @param gage
     *            The GageData object
     * @return The number of pixels to shift in the x direction
     */
    private double getShiftWidth(PaintProperties props, GageData gage) {
        double shiftWidthValue = (gage.getX_shift() / 2.0)
                / screenToWorldWidthRatio;

        return shiftWidthValue;
    }

    /**
     * Get the y direction shift value.
     * 
     * @param props
     *            The PaintProperties object
     * @param gage
     *            The GageData object
     * @return The number of pixels to shift in the y direction
     */
    private double getShiftHeight(PaintProperties props, GageData gage) {
        double shiftHeightValue = (gage.getY_shift() / 2.0)
                / screenToWorldHeightRatio;

        return shiftHeightValue;
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
        if ((manager.isDataChanged() == true) || (colorBarResource == null)) {
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

        String colorUseName = HydroViewColors
                .getColorUseNameFromPcOptions(pcOptions);
        pdcManager.setColorUseName(colorUseName);
        pdcManager.setMultiPointResource(this);
        setScaleValues(paintProps);
        IExtent extent = paintProps.getView().getExtent();
        List<GageData> data = pdcManager.getObsReportList();
        if (data != null) {
            List<PointImage> images = new ArrayList<PointImage>(data.size());
            List<DrawableString> strings = new ArrayList<DrawableString>(
                    data.size() * 3);
            for (GageData gage : data) {
                /* Get the point color for this location */
                if ((gage.getLid() != null) && gage.isUse()) {
                    addPoint(gage);
                    Coordinate c = gage.getCoordinate();
                    double[] pixel = descriptor.worldToPixel(new double[] {
                            c.x, c.y });
                    if (pixel != null && extent.contains(pixel)) {
                        double shiftHeightValue = getShiftHeight(paintProps,
                                gage);
                        double shiftWidthValue = getShiftWidth(paintProps, gage);
                        /* Draw the icons */
                        if (pcOptions.getIcon() == 1) {
                            RGB color = null;
                            if (pcOptions.getRiverStatus() == 1) {
                                color = gage.getColor();
                            } else {
                                color = RGBColors.getRGBColor(colorSet.get(0)
                                        .getColorname().getColorName());
                            }
                            PointImage image = new PointImage(getIcon(target,
                                    gage, color), pixel[0], pixel[1]);
                            image.setSiteId(gage.getLid());
                            images.add(image);
                        }
                        strings.addAll(drawPlotInfo(gage, shiftWidthValue,
                                shiftHeightValue, paintProps, target));
                    }
                }
            }
            if (images.size() > 0) {
                target.getExtension(IPointImageExtension.class)
                        .drawPointImages(paintProps, images);
            }
            if (strings.size() > 0) {
                target.drawStrings(strings);
            }
        }

        GageData currentData = manager.getCurrentData();
        if (currentData != null) {
            List<GageData> siteList = pdcManager.getObsReportList();
            if ((siteList != null) && siteList.contains(currentData)) {
                double shiftHeightValue = getShiftHeight(paintProps,
                        currentData);
                double shiftWidthValue = getShiftWidth(paintProps, currentData);

                PixelExtent pe = getPixelExtent(currentData, shiftWidthValue,
                        shiftHeightValue);
                target.drawRect(pe, HydroConstants.SQUARE_COLOR, 2, 1);
            }
        }

    }

    /**
     * Set the selected coordinate
     * 
     * @param selectedCoordinate
     */
    public void setSelectedCoordinate(Coordinate selectedCoordinate) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            Envelope env = new Envelope(coord.asLatLon());
            List<?> elements = strTree.query(env);
            if (elements.size() > 0) {
                Iterator<?> iter = elements.iterator();
                while (iter.hasNext()) {
                    GageData gage = (GageData) iter.next();
                    return "GAGE: " + gage.getName() + " VALUE: "
                            + gage.getGageValue();
                }
            }
        } catch (Exception e) {
            throw new VizException(e);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate rcoord)
            throws VizException {
        List<GageData> gageDataList = pdcManager.getObsReportList();
        try {
            GageData selected = null;
            Coordinate coord = rcoord.asLatLon();
            double minDistance = 9999;

            double[] selectedPoint = descriptor.worldToPixel(new double[] {
                    coord.x, coord.y });

            if ((gageDataList != null) && (gageDataList.size() > 0)) {
                for (GageData gd : gageDataList) {
                    if (gd.isUse()) {
                        double[] gagePoint = descriptor
                                .worldToPixel(new double[] {
                                        gd.getCoordinate().x,
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

    private String getDataFormat(String pe) {
        String format = "6.2f";

        if (pe.toUpperCase().startsWith("H")) {
            /* Height data */
            format = "%6.2f";
        } else if (pe.toUpperCase().startsWith("P")) {
            /* Precip/Pressure data */
            format = "%6.2f";
        } else if (pe.toUpperCase().startsWith("T")) {
            /* Temperature data */
            format = "%6.0f";
        } else if (pe.toUpperCase().startsWith("S")) {
            /* Snow data */
            if (pe.equalsIgnoreCase("SL")) {
                format = "%6.2f";
            } else {
                format = "%6.1f";
            }
        } else if (pe.toUpperCase().startsWith("U")) {
            /* Wind data */
            if (pe.equalsIgnoreCase("UQ")) {
                format = "%8.4f";
            } else {
                format = "%6.0f";
            }
        } else if (pe.toUpperCase().startsWith("X")) {
            /* Weather data */
            format = "%5.0f";
        } else if (pe.toUpperCase().startsWith("Q")) {
            /* Flow/Runoff data */
            if (!pe.equalsIgnoreCase("QB")) {
                format = "%6.0f";
            } else {
                format = "%6.2f";
            }
        }

        return format;
    }

    private RGB getRiverValueColorForTimeStepMode(GageData gage) {
        RGB color = null;
        String threatIndex = ThreatIndex.THREAT_MISSING_DATA.getThreatIndex();

        double actionLevel = PDCConstants.MISSING_VALUE;
        double floodLevel = PDCConstants.MISSING_VALUE;

        /* Get the river status object for this lid */
        RiverStat riverStat = dataManager.getRiverStatus(gage.getLid());

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
        if ((gage.getValue()) != PDCConstants.MISSING_VALUE) {
            threatIndex = ThreatIndex.THREAT_MISSING_STAGE.getThreatIndex();

            if ((actionLevel != PDCConstants.MISSING_VALUE)
                    && (actionLevel != 0)) {
                if (gage.getValue() >= actionLevel) {
                    threatIndex = ThreatIndex.THREAT_ACTION.getThreatIndex();
                } else {
                    threatIndex = ThreatIndex.THREAT_NONE.getThreatIndex();
                }
            }

            if ((floodLevel != PDCConstants.MISSING_VALUE) && (floodLevel != 0)) {
                if (gage.getValue() >= floodLevel) {
                    threatIndex = ThreatIndex.THREAT_FLOOD.getThreatIndex();
                } else if (actionLevel == PDCConstants.MISSING_VALUE) {
                    threatIndex = ThreatIndex.THREAT_NONE.getThreatIndex();
                }
            }
        } else { // current data was missing
            threatIndex = ThreatIndex.THREAT_MISSING_DATA.getThreatIndex();
        }

        if (threatIndex.equalsIgnoreCase(ThreatIndex.THREAT_MISSING_DATA
                .getThreatIndex())) {
            color = RGBColors.getRGBColor(colorSet.get(0).getColorname()
                    .getColorName());
        } else if (threatIndex
                .equalsIgnoreCase(ThreatIndex.THREAT_MISSING_STAGE
                        .getThreatIndex())) {
            color = RGBColors.getRGBColor(colorSet.get(1).getColorname()
                    .getColorName());
        } else if (threatIndex.equalsIgnoreCase(ThreatIndex.THREAT_NONE
                .getThreatIndex())) {
            color = RGBColors.getRGBColor(colorSet.get(2).getColorname()
                    .getColorName());
        } else if (threatIndex.equalsIgnoreCase(ThreatIndex.THREAT_ACTION
                .getThreatIndex())) {
            color = RGBColors.getRGBColor(colorSet.get(3).getColorname()
                    .getColorName());
        } else if (threatIndex.equalsIgnoreCase(ThreatIndex.THREAT_FLOOD
                .getThreatIndex())) {
            color = RGBColors.getRGBColor(colorSet.get(4).getColorname()
                    .getColorName());
        } else {
            // fprintf ( stderr ,
            // "\nIn routine \"getRiverValueColorForTimeStepMode\":\n"
            // "Reached default case in switch statement.\n"
            // "Unrecognized switch value '%c'.\n" ,
            // threat_index ) ;
            color = RGBColors.getRGBColor(colorSet.get(0).getColorname()
                    .getColorName());
        }

        return color;
    }

    /**
     * Create the ColorMap.
     */
    private void createColorMap() {
        String userId = System.getProperty("user.name");
        String appName = HydroColorManager.APPLICATION_NAME;
        List<NamedColorUseSet> pColorSetGroup = null;

        String colorUseNameDuration = HydroViewColors
                .getColorUseNameFromPcOptions(pcOptions);

        String[] parts = colorUseNameDuration.split("\\|");
        final String colorUseName = parts[0];
        int durHour = Integer.parseInt(parts[1]);
        int durSeconds = durHour * HydroConstants.SECONDS_PER_HOUR;

        // List of colors in the colorset
        colorSet = (ArrayList<Colorvalue>) GetColorValues.get_colorvalues(
                userId, appName, colorUseName, durSeconds, "E", pColorSetGroup);

        NamedColorUseSet namedColorUseSet = null;
        ArrayList<Double> thresholdValues = new ArrayList<Double>();
        ArrayList<String> colorNames = new ArrayList<String>();
        String missingColorName = null;
        String defaultColorName = null;
        String dbColorUseName = null;
        String colorName = null;
        double thresholdValue = 0;
        int duration = 0;
        int i = 0;

        for (Colorvalue colorValue : colorSet) {
            thresholdValue = colorValue.getId().getThresholdValue();
            thresholdValues.add(thresholdValue);

            colorName = colorValue.getColorname().getColorName();
            colorNames.add(colorName);

            dbColorUseName = colorValue.getId().getColorUseName();
            duration = colorValue.getId().getDuration();

            if (thresholdValue == HydroConstants.MISSING_VALUE) {
                missingColorName = colorName;
            }

            if (thresholdValue == -8888) {
                defaultColorName = colorName;
            }
        }

        // Convert Double[] to double[]
        double[] valueArray = new double[thresholdValues.size()];
        for (int j = 0; j < thresholdValues.size(); j++) {
            valueArray[j] = thresholdValues.get(j);
        }

        namedColorUseSet = new NamedColorUseSet(dbColorUseName, colorUseName,
                valueArray, colorNames.toArray(new String[colorNames.size()]),
                missingColorName, defaultColorName, duration);

        ColorThresholdArray colorArray = namedColorUseSet.getThreshold_array();

        dmPref = new DataMappingPreferences();
        colorMap = new ColorMap(colorArray.getThresholds().length);
        colorMap.setName(pdcManager.getColorUseName());

        DataMappingEntry entry = null;
        int index = 0;
        for (i = 0; i < colorArray.getThresholds().length; i++) {
            index = i; // compensate for the first two values in the list
            ColorThreshold threshold = colorArray.getThresholds()[i];
            RGB color = RGBColors.getRGBColor(threshold.getColorName());

            colorMap.setColor(index, new Color(color.red / 255f,
                    color.green / 255f, color.blue / 255f));

            entry = new DataMappingEntry();
            entry.setPixelValue((double) index);
            entry.setDisplayValue(threshold.getValue());
            dmPref.addEntry(entry);
        }

        entry = new DataMappingEntry();
        entry.setPixelValue((double) (index - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);

        dmPref.getEntries().get(0).setLabel("");
        dmPref.getEntries().get(1).setLabel("");

        colorMap.setChanged(true);

        ColorMapParameters parameters = new ColorMapParameters();
        getCapability(ColorMapCapability.class).setColorMapParameters(
                parameters);
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = NonSI.FAHRENHEIT;
        Unit<?> dataUnit = NonSI.FAHRENHEIT;

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);

        // Save the color map for later
        pdcManager.setColorMap(colorMap);
        pdcManager.setColorMapParameters(parameters);
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
        // menuManager.add(new SelectContiguousAction(true));

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

    /**
     * Convert a Color object to an RGB Object.
     * 
     * @param color
     *            The color to convert
     * @return The RGB object
     */
    private RGB convertColor(Color color) {
        int blue = (int) (color.getBlue() * 255f);
        int green = (int) (color.getGreen() * 255f);
        int red = (int) (color.getRed() * 255f);

        return new RGB(red, green, blue);
    }

    /**
     * Clear the data map.
     */
    public void resetDataMap() {
        dataMap.clear();
        strTree = new STRtree();
    }

    private class TimeSeriesLaunchAction extends AbstractRightClickAction {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return "Timeseries";
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();

                int x = pane.getLastMouseX();
                int y = pane.getLastMouseY();

                Coordinate coord = container.translateClick(x, y);

                Envelope env = new Envelope(coord);
                List<?> elements = strTree.query(env);
                GageData closestGage = getNearestPoint(coord, elements);
                if (closestGage != null) {
                    if ((ts == null) || !ts.isOpen()) {
                        Shell shell = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell();
                        ts = new TimeSeriesDlg(shell, closestGage, true);
                        ts.open();
                    } else {
                        ts.updateSelection(closestGage, true);
                    }

                } else {
                    showMessage();
                }
            }
        }
    }

    private class TimeSeriesLiteAction extends AbstractRightClickAction {
        private static final String TSL_BUNDLE_LOC = "bundles/run-TimeSeriesLite.xml";

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return "Timeseries Lite";
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();

                int x = pane.getLastMouseX();
                int y = pane.getLastMouseY();

                Coordinate coord = container.translateClick(x, y);

                Envelope env = new Envelope(coord);
                List<?> elements = strTree.query(env);
                if (elements.size() > 0) {
                    GageData gageData = getNearestPoint(coord, elements);
                    if ((gageData != null)) {
                        String lid = gageData.getLid();
                        String dataType = toPEDTSEP(gageData);
                        String fcstType = null;
                        String ts = gageData.getTs();
                        // Don't create a fcstType if we are already going
                        // to display forecast data.
                        if ((ts != null) && (!ts.startsWith("F"))) {
                            fcstType = createFcstParm(lid);
                        }

                        try {
                            AppLauncherHandler alh = new AppLauncherHandler();
                            if ((dataType != null)
                                    && (dataType.indexOf('-') < 0)) {
                                if (fcstType != null) {
                                    alh.execute(TSL_BUNDLE_LOC, lid, dataType,
                                            fcstType);
                                } else {
                                    alh.execute(TSL_BUNDLE_LOC, lid, dataType);
                                }
                            } else {
                                Shell shell = PlatformUI.getWorkbench()
                                        .getActiveWorkbenchWindow().getShell();

                                MessageBox mb = new MessageBox(shell,
                                        SWT.ICON_INFORMATION | SWT.OK);
                                mb.setText("");
                                String msg = String
                                        .format("This location's paramCode, %s, is incomplete.\nTimeSeriesLite cannot be launched for it.",
                                                dataType);
                                mb.setMessage(msg);
                                mb.open();
                            }
                        } catch (ExecutionException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
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
    }

    /**
     * 
     * @param lid
     * @return
     */
    private String createFcstParm(String lid) {
        String parm = null;

        GageData fcstGage = null;
        List<GageData> fcstList = pdcManager.getFcstReportList();
        if ((lid != null) && (fcstList != null)) {
            for (GageData gage : fcstList) {
                if (lid.equals(gage.getLid())) {
                    fcstGage = gage;
                    break;
                }
            }
        }
        // Did we find forecast data?
        if (fcstGage != null) {
            parm = toPEDTSEP(fcstGage);
        }
        return parm;
    }

    /**
     * 
     * @param gage
     * @return
     */
    private static String toPEDTSEP(GageData gage) {

        long dur = gage.getDur();
        String duration = TimeSeriesUtil.convertDur2Code((int) dur);

        String pedtsep = gage.getPe() + duration + gage.getTs()
                + gage.getExtremum();

        return pedtsep;
    }

    /**
     * Return the nearest data in the elements list to the given coordinate
     * latitude/longitude.
     * 
     * @param coord
     *            Reference coordinate latitude/longitude
     * @param elements
     *            List of Coordinates
     * @return The closest data if found. If the input list is null or empty a
     *         null reference is returned.
     */
    private GageData getNearestPoint(Coordinate coord, List<?> elements) {
        if (elements == null || elements.size() <= 0) {
            return null;
        }

        Iterator<?> iter = elements.iterator();
        double minDistance = Double.MAX_VALUE;
        GageData closestGage = null;
        while (iter.hasNext()) {
            GageData gage = (GageData) iter.next();
            double lon = gage.getLon();
            double lat = gage.getLat();
            double distance = Math.sqrt(Math.pow((lon - coord.x), 2)
                    + Math.pow((lat - coord.y), 2));
            if (distance < minDistance) {
                minDistance = distance;
                closestGage = gage;
            }
        }
        return closestGage;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.map.rsc.PointResource#dispose()
     */
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

    private void showMessage() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
        mb.setText("Error");
        mb.setMessage("The mouse pointer must be on a gage to use this feature.");
        mb.open();
    }

    /**
     * @return the ts
     */
    public TimeSeriesDlg getTs() {
        return ts;
    }

    /**
     * @param ts
     *            the ts to set
     */
    public void setTs(TimeSeriesDlg ts) {
        this.ts = ts;
    }

    /**
     * @return the isDisposed
     */
    public boolean isDisposed() {
        return getStatus() == ResourceStatus.DISPOSED;
    }

    /**
     * Unmap the data.
     */
    public void unmap() {
        ResourceList rl = descriptor.getResourceList();
        if (rl.containsRsc(colorBarResource)) {
            rl.removeRsc(colorBarResource);
            colorBarResource.dispose();
            colorBarResource = null;
        }
    }

    private class HydroInputManager extends InputAdapter {
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleDoubleClick(int,
         * int)
         */
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
                    if (elements.size() > 0) {
                        Iterator<?> iter = elements.iterator();
                        /* Take the first one in the list */
                        if (iter.hasNext()) {
                            /* element 0 = Coordinate, 1 = inspectString */
                            GageData gage = (GageData) iter.next();

                            Shell shell = PlatformUI.getWorkbench()
                                    .getActiveWorkbenchWindow().getShell();

                            if ((ts == null) || !ts.isOpen()) {
                                ts = new TimeSeriesDlg(shell, gage, false);
                                ts.open();
                            } else {
                                ts.updateSelection(gage, false);
                            }
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

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDown(int, int,
         * int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (mouseButton == 2) {
                return true;
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 2) {
                return true;
            }
            return false;
        }
    }
}
