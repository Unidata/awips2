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

import java.awt.Rectangle;
import java.nio.FloatBuffer;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.common.mpe.util.DPAFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarLoc;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.raytheon.viz.mpe.ui.dialogs.hourlyradar.RadarDataManager;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * The Digital Precipitation Array Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009 2675       mpduff      Initial creation
 * Aug 13, 2009 2675       mpduff      TIM Changes added
 * Nov 05, 2015 5070       randerso    Adjust font sizes for dpi scaling
 * Sep 16, 2016 5631       bkowal      Cleanup. Use {@link DPAConstants}.
 * Apr 15, 2019 7596       lsingh      Updated units framework to JSR-363. 
 *                                     Handled unit conversion.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class DPAResource
        extends AbstractVizResource<GenericResourceData, MapDescriptor>
        implements IMpeResource {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(DPAResource.class);

    private static final String MISSING_DATA = "Missing Data";

    private static final String RADAR_IGNORED = "Radar Data Ignored";

    private static final String ALL_ZERO = "Radar Data All Zero";

    /** Radar types */
    public static enum SingleSiteRadarType {
        RAW_SP_RADAR("Raw SP Radar"), RAW_DP_RADAR(
                "Raw DP Radar"), MEAN_FIELD_BIAS_CORRECTED_SP_RADAR(
                        "Mean Field Bias Corrected SP Radar"), RADAR_COVERAGE_MAP(
                                "Radar Coverage Map");

        private String text;

        private SingleSiteRadarType(String text) {
            this.text = text;
        }
    }

    private DPAFile dpaFile;

    /** The HRAP subgrid */
    private HRAPSubGrid subGrid;

    private GriddedImageDisplay gridDisplay;

    private ColorMapParameters parameters;

    private MPEDisplayManager displayMgr;

    private GriddedContourDisplay contourDisplay;

    private GridGeometry2D gridGeometry;

    private FloatBuffer buf;

    private List<Colorvalue> colorSet;

    /**
     * Radar Data.
     */
    private double[][] data = new double[131][131];

    /**
     * The Radar ID.
     */
    private String radId = null;

    /** Number of grid cells */
    private int ngrd = -999;

    private Rectangle extent;

    private DrawableString missingString;

    private DrawableString mainString;

    private DrawableString ignoredStr;

    private DrawableString allZeroStr;

    private int available = 0;

    private SingleSiteRadarType radarType;

    private boolean ignored = false;

    private Date lastDate = null;

    /**
     * Constructor.
     * 
     * @param dpaFile
     *            The DPA File
     * @param colorSet
     *            The colorset
     * @param radId
     *            The radar id
     * @param radarType
     *            The radar Type
     * @param ngrd
     *            The ngrd value
     */
    public DPAResource(DPAFile dpaFile, List<Colorvalue> colorSet, String radId,
            SingleSiteRadarType radarType, int ngrd) {
        super(null, new LoadProperties());
        displayMgr = MPEDisplayManager.getCurrent();
        this.dpaFile = dpaFile;
        this.colorSet = colorSet;
        this.radId = radId;
        this.radarType = radarType;
        this.ngrd = ngrd;

        getCapability(ColorableCapability.class)
                .setColor(RGBColors.getRGBColor("Yellow"));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }

        if (mainString.font != null) {
            mainString.font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        if (dpaFile == null) {
            return "No Radar Data Available";
        }

        return dpaFile.getFile().getAbsolutePath();
    }

    // -------------------------------------------------------------------

    @Override
    protected void initInternal(IGraphicsTarget target) {
        boolean missingData = false;
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName("PRECIP_ACCUM");
        DataMappingPreferences dmPref = new DataMappingPreferences();
        int index = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(index, new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f));

            DataMappingEntry entry = new DataMappingEntry();
            entry.setPixelValue((double) index);
            entry.setDisplayValue(cv.getId().getThresholdValue());
            dmPref.addEntry(entry);

            index++;
        }

        dmPref.getEntries().get(0).setLabel("");
        dmPref.getEntries().get(1).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);

        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<Length> displayUnit = USCustomary.INCH;

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(displayUnit);
        parameters.setColorMapUnit(dmPref.getImageUnit(displayUnit));

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);

        Unit<Length> dataUnit = MetricPrefix.MILLI(SI.METRE).divide(100);
        UnitConverter cvt = UnitConv.getConverterToUnchecked(dataUnit,
                parameters.getColorMapUnit());
        try {
            buf = FloatBuffer.allocate(17161);
            Date dtg = MPEDisplayManager.getCurrent().getCurrentEditDate();

            // available = radar availability flag read from
            // RWRadarResult or DAARadarResult table
            // = 0 -- field available (some values > 0.0)
            // = 1 -- field not available
            // = 2 -- field available (all values = 0.0)

            available = 0;
            if (radarType == SingleSiteRadarType.RAW_DP_RADAR) {
                available = RadarDataManager.getInstance()
                        .getAvailableRadarDP(radId, dtg);
            } else {
                available = RadarDataManager.getInstance()
                        .getAvailableRadarSP(radId, dtg);

                // check for special case of ignored radar
                // this behavior is different than A1
                ignored = RadarDataManager.getInstance().getIgnoreRadarSP(radId,
                        dtg);
                if (ignored) {
                    available = 0;
                }
            }

            if (available == 0) {

                dpaFile.load();
                if (radarType == SingleSiteRadarType.RAW_SP_RADAR) {
                    handler.info("Reading SP Radar Data");
                    data = dpaFile.getStage1i();
                } else if (radarType == SingleSiteRadarType.RAW_DP_RADAR) {
                    handler.info("Reading DP Radar Data");
                    data = dpaFile.getStage1i();
                } else if (radarType == SingleSiteRadarType.MEAN_FIELD_BIAS_CORRECTED_SP_RADAR) {
                    handler.info("Reading MFB Corrected SP Radar Data");
                    data = dpaFile.getStage1u();
                } else {
                    data = dpaFile.getZeroData();
                }

                for (int i = 0; i < DPAConstants.NUM_DPA_ROWS; i++) {
                    for (int j = 0; j < DPAConstants.NUM_DPA_COLS; j++) {
                        float f = DPAConstants.DPA_MISSING;
                        if (data[i][j] != DPAConstants.DPA_MISSING) {
                            f = (float) Math.floor(cvt.convert(data[i][j]));
                        }

                        int dx = i - 66;
                        int dy = j - 66;
                        double dist = Math.sqrt(dx * dx + dy * dy);

                        // If grid cell outside radius then set missing
                        if (dist > ngrd) {
                            f = DPAConstants.DPA_MISSING;
                        }
                        buf.put(f);
                    }
                }
                buf.rewind();
                missingData = false;
            } else if (available == 2) {
                for (int i = 0; i < 131; i++) {
                    for (int j = 0; j < 131; j++) {
                        buf.put(0.0f);
                    }
                }
                buf.rewind();
                missingData = false;
            } else {
                for (int i = 0; i < 131; i++) {
                    for (int j = 0; j < 131; j++) {
                        buf.put(-1f);
                    }
                }
                buf.rewind();
                missingData = true;
            }

            /* Retrieve the Latitude/Longitude of the radars */
            Coordinate latLon;
            List<MPERadarLoc> radarList = MPEDataManager.getInstance()
                    .getRadars();
            for (MPERadarLoc radarLoc : radarList) {
                if (radarLoc.getId().equalsIgnoreCase(radId)) {
                    latLon = radarLoc.getLatLon();
                    if (latLon == null) {
                        continue;
                    }
                    if ((latLon.x == 0.) || (latLon.y == 0.)) {
                        continue;
                    }

                    /* calculate HRAP coordinates from lat,lon */
                    Coordinate hp = new Coordinate(0, 0);
                    hp = HRAP.getInstance().latLonToGridCoordinate(latLon,
                            PixelOrientation.UPPER_LEFT);
                    int minx = (int) hp.x - 65;
                    int miny = (int) hp.y - 65;

                    extent = new Rectangle(minx, miny, 131, 131);
                    break;
                }
            }

            if ((extent.x == 0) && (extent.y == 0)) {
                Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                if ((extent.width == coord.width)
                        && (extent.height == coord.height)) {
                    extent = coord;
                } else {
                    dpaFile = null;
                    return;
                }
            }
            subGrid = new HRAPSubGrid(extent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            project(gridGeometry.getCoordinateReferenceSystem());
        } catch (Exception e) {
            dpaFile = null;
            statusHandler.error("Failed to initialize the DPA Resource.", e);
            return;
        }

        mainString = new DrawableString(radarType.text,
                getCapability(ColorableCapability.class).getColor());
        ignoredStr = new DrawableString(RADAR_IGNORED,
                RGBColors.getRGBColor("Red"));
        allZeroStr = new DrawableString(ALL_ZERO,
                RGBColors.getRGBColor("Green"));

        if (missingData
                && (radarType == SingleSiteRadarType.MEAN_FIELD_BIAS_CORRECTED_SP_RADAR
                        || radarType == SingleSiteRadarType.RAW_SP_RADAR
                        || radarType == SingleSiteRadarType.RAW_DP_RADAR)) {
            missingString = new DrawableString(MISSING_DATA,
                    getCapability(ColorableCapability.class).getColor());
        }

        ignoredStr.font = allZeroStr.font = mainString.font = target
                .initializeFont("Dialog", 10, null);

        if (missingString != null) {
            missingString.font = mainString.font;
        }

    }

    // ---------------------------------------------------------------

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Set<DisplayMode> mode = displayMgr.getDisplayMode();

        if (buf != null) {
            if (mode.contains(DisplayMode.Image)) {
                if (gridDisplay == null) {
                    gridDisplay = new GriddedImageDisplay(buf, descriptor,
                            gridGeometry);

                    gridDisplay.setColorMapParameters(
                            getCapability(ColorMapCapability.class)
                                    .getColorMapParameters());
                }

                GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                        paintProps, 1.0f, 1.0f, false);

                gridDisplay.paint(target, giProps);
            }

            if (mode.contains(DisplayMode.Contour)) {
                if (contourDisplay == null) {
                    contourDisplay = new GriddedContourDisplay(descriptor,
                            gridGeometry, buf);

                    contourDisplay.setColor(ColorUtil.WHITE);
                    contourDisplay.setLineStyle(LineStyle.SOLID);
                    contourDisplay.setOutlineWidth(1);
                }
                contourDisplay.paint(target, paintProps);
            }
        }

        IExtent screenExtent = paintProps.getView().getExtent();

        mainString.setCoordinates(screenExtent.getMinX() + 50,
                screenExtent.getMinY() + 50);
        target.drawStrings(mainString);

        Date date = MPEDisplayManager.getCurrent().getCurrentEditDate();
        if (date.equals(lastDate) == false) {
            // Check for ignored Radar
            if (radarType.equals(SingleSiteRadarType.RAW_SP_RADAR) || radarType
                    .equals(SingleSiteRadarType.MEAN_FIELD_BIAS_CORRECTED_SP_RADAR)) {
                ignored = RadarDataManager.getInstance().getIgnoreRadarSP(radId,
                        date);
                lastDate = date;
            } else if (radarType.equals(SingleSiteRadarType.RAW_DP_RADAR)) {
                ignored = RadarDataManager.getInstance().getIgnoreRadarDP(radId,
                        date);
                lastDate = date;
            }
        }

        if (missingString != null) {
            missingString.setCoordinates(screenExtent.getMinX() + 350,
                    screenExtent.getMinY() + 550);
            target.drawStrings(missingString);
        }

        if (ignored
                && (radarType == SingleSiteRadarType.MEAN_FIELD_BIAS_CORRECTED_SP_RADAR
                        || radarType == SingleSiteRadarType.RAW_SP_RADAR)) {
            ignoredStr.setCoordinates(screenExtent.getMinX() + 50,
                    screenExtent.getMaxY() - 25, 0.0);
            target.drawStrings(ignoredStr);
        }

        else if (ignored && (radarType == SingleSiteRadarType.RAW_DP_RADAR)) {
            ignoredStr.setCoordinates(screenExtent.getMinX() + 50,
                    screenExtent.getMaxY() - 25, 0.0);
            target.drawStrings(ignoredStr);
        }
        if (available == 2 && (radarType == SingleSiteRadarType.RAW_DP_RADAR
                || radarType == SingleSiteRadarType.RAW_SP_RADAR)) {
            allZeroStr.setCoordinates(screenExtent.getMinX() + 600,
                    screenExtent.getMaxY() - 25, 0.0);
            target.drawStrings(allZeroStr);
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (contourDisplay != null) {
            contourDisplay.dispose();
            contourDisplay = null;
        }
    }

    /**
     * Update the Xmrg Display.
     * 
     * @param reload
     *            Reread the data from the file if true
     */
    /**
     * @return the dpaFile
     */
    public DPAFile getDpaFile() {
        return dpaFile;
    }

    /**
     * @return the data
     */
    public double[][] getStage1i() {
        return data;
    }

    public double[][] getData() {
        return data;
    }

    public Rectangle getExtent() {
        return extent;
    }

    public void update() {
        lastDate = null;
        issueRefresh();
    }
}
