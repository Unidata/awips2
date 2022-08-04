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
package com.raytheon.viz.hydrocommon.resource;

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

import si.uom.NonSI;
import systems.uom.common.USCustomary;

import javax.measure.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension;
import com.raytheon.uf.viz.core.point.drawables.ext.IPointImageExtension.PointImage;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageDataTimeStep;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.util.HydroImageMaker;
import com.raytheon.viz.hydrocommon.util.HydroImageMaker.ImageSize;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThreshold;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorThresholdArray;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.HydroViewColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.index.strtree.STRtree;

/**
 * Parent class for Hydro/D2D MultiPointResources.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Sep 17, 2018  7379     mduff      Initial creation.
 *
 * </pre>
 *
 * @author muff
 */

public abstract class AbstractMultiPointResource extends
        AbstractVizResource<AbstractMultiPointResourceData, IMapDescriptor> {

    /** Missing value indicator */
    protected static final String M = "M";

    protected final ThreadLocal<SimpleDateFormat> monthDayFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("MM/dd");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };

    protected final ThreadLocal<SimpleDateFormat> hourMinuteFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };

    protected static class HydroImageMakerCallback
            implements IRenderedImageCallback {

        private final String dispClass;

        private final RGB color;

        public HydroImageMakerCallback(String dispClass, RGB color) {
            this.dispClass = dispClass;
            this.color = color;
        }

        @Override
        public RenderedImage getImage() throws VizException {
            return HydroImageMaker.getImage(dispClass, ImageSize.MEDIUM, color);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((color == null) ? 0 : color.hashCode());
            result = prime * result
                    + ((dispClass == null) ? 0 : dispClass.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            HydroImageMakerCallback other = (HydroImageMakerCallback) obj;
            if (color == null) {
                if (other.color != null) {
                    return false;
                }
            } else if (!color.equals(other.color)) {
                return false;
            }
            if (dispClass == null) {
                if (other.dispClass != null) {
                    return false;
                }
            } else if (!dispClass.equals(other.dispClass)) {
                return false;
            }
            return true;
        }

    }

    protected static final RGB LABEL_COLOR = RGBColors.getRGBColor("White");

    protected final Map<String, Map<RGB, IImage>> imageMap = new HashMap<>();

    protected final Map<String, GageData> dataMap = new HashMap<>();

    protected final Map<String, GageDataTimeStep> dataMapTimeStep = new HashMap<>();

    protected STRtree strTree = new STRtree();

    protected IFont font;

    protected int fontSize;

    private final DecimalFormat df = new DecimalFormat();

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private double screenToWorldWidthRatio = 0.0;

    private double screenToWorldHeightRatio = 0.0;

    protected ColorMap colorMap = null;

    /**
     * List of color value objects.
     */
    protected List<Colorvalue> colorSet = null;

    /**
     * The DataMappingPreferences.
     */
    protected DataMappingPreferences dmPref = null;

    protected PDCOptionData pcOptions = null;

    private HydroDisplayManager manager = null;

    /**
     * Constructor
     *
     * @param resourceData
     *            the {@link MultiPointResourceData} to use.
     * @param loadProperties
     *            the {@link LoadProperties} to use.
     */
    public AbstractMultiPointResource(
            AbstractMultiPointResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        manager = HydroDisplayManager.getInstance();
        pcOptions = PDCOptionData.getInstance();

        // Hide the change color and colormap menu items
        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);

        df.setMaximumFractionDigits(2);
    }

    /**
     * Remove loaded resources associated with this resource.
     */
    public abstract void unmap();

    protected synchronized void addPoint(GageData gage) {
        String lid = gage.getLid();
        if (!dataMap.containsKey(lid)) {
            Coordinate xy = new Coordinate(gage.getLon(), gage.getLat());
            gage.setCoordinate(xy);

            /* Create a small envelope around the point */
            PixelExtent pe = getPixelExtent(gage, getShiftWidth(gage),
                    getShiftHeight(gage));
            Envelope newEnv = descriptor.pixelToWorld(pe);

            strTree.insert(newEnv, gage);
            dataMap.put(lid, gage);
        }
    }

    /**
     * Creates data structure for keeping the buffered images.
     *
     * @param gage
     */
    protected IImage getIcon(IGraphicsTarget target, GageData gage, RGB color) {
        String dispClass = gage.getDispClass();
        Map<RGB, IImage> colorMap = imageMap.get(dispClass);
        if (colorMap == null) {
            colorMap = new HashMap<>();
            imageMap.put(dispClass, colorMap);
        }
        IImage image = colorMap.get(color);
        if (image == null) {
            image = target.initializeRaster(
                    new HydroImageMakerCallback(dispClass, color));
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
    protected PixelExtent getPixelExtent(GageData data, double shiftWidth,
            double shiftHeight) {
        Coordinate c = data.getCoordinate();
        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });
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
        return new PixelExtent(coors);
    }

    /**
     * Prepares the {@link DrawableString}s that describe the data.
     *
     * @param gage
     *            the gage data obj
     * @param shiftWidth
     *            the shift width coordinate
     * @param shiftHeight
     *            the shift height coordinate
     * @throws VizException
     */
    protected Collection<DrawableString> generateDrawableStrings(GageData gage,
            double shiftWidth, double shiftHeight) throws VizException {
        List<DrawableString> strings = new ArrayList<>();
        Coordinate c = gage.getCoordinate();

        int floodLevel = pcOptions.getFloodLevel();
        int deriveStageFlow = pcOptions.getDeriveStageFlow();

        String valueLabel = null;
        String formatStr = null;

        formatStr = GageData.getDataFormat(gage.getPe());

        /* Logic for determining how the data values are displayed. */
        boolean showValue1 = manager.isValue();
        boolean showValue2 = false;
        if (!showValue1) {
            showValue2 = false;
        } else {
            if (((floodLevel == 1) || (deriveStageFlow == 1)) && (pcOptions
                    .getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                            .getAdHocDataElementType())) {
                showValue2 = true;
            }
        }

        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });
        if (showValue1) {
            RGB textColor = RGBColors.getRGBColor("White");
            if (gage.getGageValue() == HydroConstants.MISSING_VALUE) {
                valueLabel = M;
            } else {
                valueLabel = String.format(formatStr, gage.getGageValue());
            }

            Coordinate valueCoor = new Coordinate(
                    (centerpixels[0] + shiftWidth) - getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 2);

            textColor = RGBColors.getRGBColor("white");

            DrawableString string = new DrawableString(valueLabel, textColor);
            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;
            string.setCoordinates(valueCoor.x, valueCoor.y);
            strings.add(string);

            if (pcOptions
                    .getTimeMode() != PDCConstants.TimeModeType.VALUE_CHANGE
                            .getTimeMode()) {
                if (showValue2) {
                    String valueLabel2 = null;
                    if (gage.getGageValue2() != HydroConstants.MISSING_VALUE) {
                        /*
                         * Determine the format that value2 should be displayed
                         * as. Use the format string for value1 except in the
                         * case where value2 represents a derived flow.
                         */
                        if ("HG".equalsIgnoreCase(gage.getPe())
                                && (deriveStageFlow == 1)) {
                            valueLabel2 = String.format("%6.0f",
                                    gage.getValue2());
                        } else if ("QR".equalsIgnoreCase(gage.getPe())
                                && (deriveStageFlow == 1)) {
                            valueLabel2 = String.format("%6.2f",
                                    gage.getValue2());
                        } else {
                            valueLabel2 = String.format(formatStr,
                                    gage.getValue2());
                        }
                    } else {
                        valueLabel2 = M;
                    }

                    valueCoor = new Coordinate(
                            (centerpixels[0] + shiftWidth) - getScaleWidth(),
                            (centerpixels[1] + shiftHeight)
                                    + getScaleHeight() / -0.9);

                    string = new DrawableString(valueLabel2, textColor);
                    string.font = font;
                    string.horizontalAlignment = HorizontalAlignment.RIGHT;
                    string.setCoordinates(valueCoor.x, valueCoor.y);
                    strings.add(string);
                }
            }
        }

        if (manager.isTime()) {
            Coordinate dateCoor1 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    (centerpixels[1] + shiftHeight) - getScaleHeight() / 0.9);
            Coordinate dateCoor2 = new Coordinate(
                    (centerpixels[0] + shiftWidth) + getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight() / -2);
            // draw the date and time
            DrawableString string = new DrawableString(
                    monthDayFormat.get().format(gage.getValidtime().getTime()),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor1.x, dateCoor1.y);
            strings.add(string);

            string = new DrawableString(hourMinuteFormat.get()
                    .format(gage.getValidtime().getTime()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(dateCoor2.x, dateCoor2.y);
            strings.add(string);
        }
        // draw the ID
        if (manager.isId()) {
            Coordinate idCoor = new Coordinate(
                    centerpixels[0] + shiftWidth - getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight());

            DrawableString string = new DrawableString(gage.getLid(),
                    LABEL_COLOR);
            string.font = font;
            string.horizontalAlignment = HorizontalAlignment.RIGHT;
            string.setCoordinates(idCoor.x, idCoor.y);
            strings.add(string);
        }
        if (manager.isName()) {
            // draw the Name
            Coordinate nameCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight + getScaleHeight());

            DrawableString string = new DrawableString(gage.getName(),
                    LABEL_COLOR);
            string.font = font;
            string.setCoordinates(nameCoor.x, nameCoor.y);
            strings.add(string);
        }

        if (manager.isPe()) {
            String shefDurCode;
            if ("PC".equalsIgnoreCase(gage.getPe())) {
                /*
                 * PC is always "I", but sometimes the duration might have been
                 * screwed up
                 */
                shefDurCode = "I";
            } else {
                shefDurCode = GageData.convertDur((int) gage.getDur());
                if (shefDurCode == null) {
                    shefDurCode = "?";
                }
            }
            String pe = gage.getPe() + shefDurCode + gage.getTs()
                    + gage.getExtremum();

            Coordinate peCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight - getScaleHeight() / 2);
            DrawableString string = new DrawableString(pe, LABEL_COLOR);
            string.font = font;
            string.setCoordinates(peCoor.x, peCoor.y);
            strings.add(string);
        }

        if (manager.isElevation()) {
            // draw the elevation
            Coordinate elCoor = new Coordinate(
                    centerpixels[0] + shiftWidth + getScaleWidth(),
                    centerpixels[1] + shiftHeight - getScaleHeight() / 2);

            DrawableString string = new DrawableString(
                    df.format(gage.getElevation()), LABEL_COLOR);
            string.font = font;
            string.setCoordinates(elCoor.x, elCoor.y);
            strings.add(string);
        }
        return strings;
    }

    protected void setScaleValues(PaintProperties props) {
        screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        setScaleWidth();
        setScaleHeight();
    }

    /**
     * Set the width scalar
     */
    private void setScaleWidth() {
        scaleWidthValue = (ImageSize.MEDIUM.getWidth() / 2.0)
                / screenToWorldWidthRatio;
    }

    /**
     * get the scale width value
     *
     * @return scale width value
     */
    protected double getScaleWidth() {
        return scaleWidthValue;
    }

    /**
     * Set the height scalar
     */
    protected void setScaleHeight() {
        scaleHeightValue = (ImageSize.MEDIUM.getHeight() / 2.0)
                / screenToWorldHeightRatio;
    }

    /**
     * Get the scalar height
     *
     * @return scale height value
     */
    protected double getScaleHeight() {
        return scaleHeightValue;
    }

    /**
     * Get the x direction shift value.
     *
     * @param gage
     *            The GageData object
     * @return The number of pixels to shift in the x direction
     */
    protected double getShiftWidth(GageData gage) {
        double shiftWidthValue = (gage.getX_shift() / 2.0)
                / screenToWorldWidthRatio;

        return shiftWidthValue;
    }

    /**
     * Get the y direction shift value.
     *
     * @param gage
     *            The GageData object
     * @return The number of pixels to shift in the y direction
     */
    protected double getShiftHeight(GageData gage) {
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
    protected void paintInternalHelper(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        List<GageData> data = manager.getObsReportList();

        if (data == null || data.isEmpty()) {
            return;
        }

        /*
         * Filter out additional gages for a particular location and gages that
         * are not of interest from the returned data. This data is merged for
         * multiple gages for the same location from the time step perspective.
         */
        // Map: lid -> Gage
        Map<String, GageData> gagesOfInterestMap = new HashMap<>(data.size(),
                1.0f);
        for (GageData gage : data) {
            if (gage.getLid() == null || !gage.isUse()
                    || gagesOfInterestMap.containsKey(gage.getLid())) {
                continue;
            }
            gagesOfInterestMap.put(gage.getLid(), gage);
        }
        if (gagesOfInterestMap.isEmpty()) {
            return;
        }

        IExtent extent = paintProps.getView().getExtent();

        List<PointImage> images = new ArrayList<>(gagesOfInterestMap.size());
        List<DrawableString> strings = new ArrayList<>(
                gagesOfInterestMap.size() * 3);

        for (GageData gage : gagesOfInterestMap.values()) {
            /* Get the point color for this location */
            addPoint(gage);
            Coordinate c = gage.getCoordinate();
            if (c == null) {
                continue;
            }
            double[] pixel = descriptor.worldToPixel(new double[] { c.x, c.y });

            if (pixel != null && extent.contains(pixel)) {
                double shiftHeightValue = getShiftHeight(gage);
                double shiftWidthValue = getShiftWidth(gage);
                /* Draw the icons */
                if (pcOptions.getIcon() == 1) {
                    RGB color = null;
                    if (pcOptions.getRiverStatus() == 1) {
                        color = gage.getColor();
                    } else {
                        color = RGBColors.getRGBColor(
                                colorSet.get(0).getColorname().getColorName());
                    }
                    PointImage image = new PointImage(
                            getIcon(target, gage, color), pixel[0], pixel[1]);
                    image.setSiteId(gage.getLid());
                    images.add(image);
                }
                strings.addAll(generateDrawableStrings(gage, shiftWidthValue,
                        shiftHeightValue));
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

    /**
     * Create the ColorMap.
     */
    protected void createColorMap() {
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
        colorSet = GetColorValues.get_colorvalues(userId, appName, colorUseName,
                durSeconds, "E", pColorSetGroup);

        NamedColorUseSet namedColorUseSet = null;
        List<Double> thresholdValues = new ArrayList<>();
        List<String> colorNames = new ArrayList<>();
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

        DataMappingEntry entry = null;
        int index = 0;
        for (i = 0; i < colorArray.getThresholds().length; i++) {
            // compensate for the first two values in the list
            index = i;
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
        getCapability(ColorMapCapability.class)
                .setColorMapParameters(parameters);
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = USCustomary.FAHRENHEIT;

        parameters.setDisplayUnit(displayUnit);
        parameters.setColorMapUnit(dmPref.getImageUnit(displayUnit));

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);

        // Save the color map for later
        manager.setPdcColorMap(colorMap);
        manager.setPdcColorMapParameters(parameters);
    }

    /**
     * Convert a Color object to an RGB Object.
     *
     * @param color
     *            The color to convert
     * @return The RGB object
     */
    protected RGB convertColor(Color color) {
        int blue = (int) (color.getBlue() * 255f);
        int green = (int) (color.getGreen() * 255f);
        int red = (int) (color.getRed() * 255f);

        return new RGB(red, green, blue);
    }

    /**
     * Clear the data map.
     */
    public void resetDataMap() {
        if (pcOptions.getQueryMode() == 1) {
            dataMapTimeStep.clear();
        } else {
            dataMap.clear();
        }
        strTree = new STRtree();
    }

    /**
     * Return the selected (GageData).
     *
     * @return The selected Gage or null
     */
    public GageData getSelectedGage() {
        return (manager.getCurrentData());
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
    protected GageData getNearestPoint(Coordinate coord, List<?> elements) {
        if (elements == null || elements.isEmpty()) {
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

    /**
     * @return the isDisposed
     */
    public boolean isDisposed() {
        return getStatus() == ResourceStatus.DISPOSED;
    }
}
