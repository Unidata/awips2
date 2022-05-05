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

import java.awt.Point;
import java.awt.Rectangle;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * XMRG Display Resource.
 *
 * Reused/Modified from MPE.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009 2258       mpduff      Initial creation.
 * Mar 14, 2017 18417      snaples     Updated loadData to handle trace precip color mapping properly.
 * Jun 11, 2018 6605       tgurney     Null check in loadData
 * Sep 04, 2018 20694      mporricelli Updated loadData and updateXmrg to properly
 *                                     display trace precip.
 * </pre>
 *
 * @author mpduff
 */

public class XmrgResource
        extends AbstractVizResource<XmrgResourceData, MapDescriptor> {
    private static final GeometryFactory gf = new GeometryFactory();

    /** The xmrg file */
    private XmrgFile xmrg;

    /** The HRAP sub grid */
    private HRAPSubGrid subGrid;

    /** The GriddedImageDisplay */
    private GriddedImageDisplay2 gridDisplay;

    /** The ColorMapParameters */
    private ColorMapParameters parameters;

    /** The image brightness */
    private float brightness = 1.0f;

    /** The image contrast */
    private float contrast = 1.0f;

    private boolean isInterpolated;

    private GridGeometry2D gridGeometry;

    /** Buffer to hold the data for display */
    private FloatBuffer buf;

    private List<Colorvalue> colorSet;

    private String cv_use;

    /**
     * The accumulated interval
     */
    private int accumInterval;

    /** The date of the data */
    private Date dataDate;

    /** Array of xmrg data values */
    private short[] data;

    private float[] floatData;

    private List<Float> sampleData;

    private Rectangle extent;

    private boolean isGridded = true;

    /**
     * The Unit Converter
     */
    private UnitConverter cvt;

    /**
     * Constructor.
     *
     * @param cv_use
     *            The data type
     * @param accumInterval
     *            The accumulation interval, number of hours
     * @param xmrg
     *            The xmrg file
     * @param colorSet
     *            The color set to use
     * @param dataDate
     *            The date of the data
     */
    public XmrgResource(XmrgResourceData resourceData, String cv_use,
            int accumInterval, XmrgFile xmrg, List<Colorvalue> colorSet) {
        super(resourceData, new LoadProperties());

        this.colorSet = colorSet;
        this.cv_use = cv_use;
        this.accumInterval = accumInterval;
        this.xmrg = xmrg;
        this.resourceData = resourceData;
        isGridded = true;
        loadData();
    }

    public XmrgResource(XmrgResourceData resourceData, String cv_use,
            int accumInterval, float[] floatData, List<Colorvalue> colorSet,
            Rectangle extent) {
        super(resourceData, new LoadProperties());
        this.colorSet = colorSet;
        this.cv_use = cv_use;
        this.accumInterval = accumInterval;
        this.resourceData = resourceData;
        this.floatData = floatData;
        this.extent = extent;
        dataDate = HydroDisplayManager.getInstance().getDataDate();
        isGridded = false;
        loadData();
    }

    @Override
    public void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    @Override
    public String getName() {
        String noData = "";
        if ((xmrg == null) && (floatData == null)) {
            noData = " No Data Available";
        }
        HydroDisplayManager dman = HydroDisplayManager.getInstance();
        String name = null;
        if ("FFG".equals(cv_use)) {
            ResolutionLevel res = resourceData.getResolution();
            SimpleDateFormat sdf = new SimpleDateFormat("EEE MM/dd HH:mm");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            int hours = resourceData.getDuration()
                    / HydroConstants.MILLIS_PER_HOUR;
            String hourStr = "hour";
            if (hours != 1) {
                hourStr = hourStr.concat("s");
            }
            if (dataDate == null) {
                name = "FFG No Data Available";
            } else {
                name = "FFG " + res.getResolution() + " " + hours + " "
                        + hourStr + " " + sdf.format(dataDate) + "z (in)"
                        + noData;
            }
        } else {
            name = dman.getAccumInterval()
                    + " hr Accumulated Best Estimate QPE Ending "
                    + HydroConstants.DISPLAY_DATE_FORMAT
                            .format(dman.getDataDate())
                    + "z (in)" + noData;
        }
        return name;
    }

    private void initColorMapParams() {
        ColorMap colorMap = new ColorMap(colorSet.size());
        DataMappingPreferences dmPref = new DataMappingPreferences();
        int i = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(i, new Color(rgb.red / 255f, rgb.green / 255f,
                    rgb.blue / 255f));

            DataMappingEntry entry = new DataMappingEntry();
            entry.setPixelValue((double) i);
            entry.setDisplayValue(cv.getId().getThresholdValue());
            dmPref.addEntry(entry);

            i++;
        }
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (i - 1));
        entry.setDisplayValue(Double.MAX_VALUE);
        dmPref.addEntry(entry);
        dmPref.getEntries().get(0).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);

        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = USCustomary.INCH;
        Unit<?> dataUnit = MetricPrefix.MILLI(SI.METRE).divide(100);

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();
    }

    /**
     * @param mm100
     *            hundredths of a millimeter
     * @return inches
     */
    private float mm100ToInches(float mm100) {
        UnitConverter converter = MetricPrefix.MILLI(SI.METRE).getConverterTo(USCustomary.INCH);
        return (float) (converter.convert(mm100) / 100.0);
    }

    /**
     * Load the data from the xmrg file into memory
     */
    private void loadData() {
        initColorMapParams();
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        if (accumInterval > 1) {
            // this will accumulate all hours requested and display it
            // this holds current xmrg values
            short[] tempdata = null;
            int numhours = accumInterval;
            try {
                Rectangle extent = HRAPCoordinates.getHRAPCoordinates();
                int datasz = extent.height * extent.width;

                // this is used to accumulate all hours
                data = new short[datasz];

                buf = FloatBuffer.allocate(datasz);
            } catch (Exception e1) {
                statusHandler.warn("Failed to get HRAP coordinates", e1);
            }

            String dirname = appsDefaults
                    .getToken(HydroConstants.XMRG_DIR_TOKEN);
            String fname = "";
            HydroDisplayManager dman = HydroDisplayManager.getInstance();
            dataDate = dman.getDataDate();
            // this.dataDate = xmrg.getHeader().getValidDate();
            // HydroDisplayManager.getInstance().setDataDate(this.dataDate);

            Calendar cal1 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal1.setTime(dataDate);
            Calendar cal2 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal2.setTime(cal1.getTime());

            try {

                for (int k = 0; k < numhours; k++) {
                    cal2.setTime(cal1.getTime());
                    cal2.add(Calendar.SECOND,
                            -(k * HydroConstants.SECONDS_PER_HOUR));
                    String dtform = HydroConstants.QPE_DATE_FORMAT
                            .format(cal2.getTime());
                    fname = FileUtil.join(dirname, cv_use + dtform + "z");
                    XmrgFile wmrg = null;

                    try {
                        wmrg = new XmrgFile(fname);
                        wmrg.load();
                    } catch (FileNotFoundException e) {
                        statusHandler.debug("XMRG file " + fname + " not found",
                                e);
                        continue;
                    } catch (IOException e) {
                        statusHandler.warn("Failed to load XMRG file " + fname,
                                e);
                        continue;
                    }

                    xmrg = wmrg;
                    extent = xmrg.getHrapExtent();
                    tempdata = xmrg.getData();

                    int c = 0;
                    for (@SuppressWarnings("unused")
                    short s : tempdata) {
                        if ((data[c] < 0) && (tempdata[c] >= 0)) {
                            data[c] = tempdata[c];
                        } else if ((data[c] >= 0) && (tempdata[c] > 0)) {
                            data[c] += tempdata[c];
                        }
                        c++;
                    }
                }
                sampleData = new ArrayList<>(data.length);
                if (extent != null) {
                    for (short s : data) {
                    // Map <0 and 0 to the first two color segments of the color scale, respectively
                    if (s < 0) {
                        buf.put(0.0f);
                        sampleData.add(0.0f);
                    } else if (s == 0) {
                        buf.put(1.0f);
                        sampleData.add(0.0f);
                    } else {
                        // Map values >0 to appropriate color bar segment
                        float f = (float) Math.floor(cvt.convert(s));
                        buf.put(f);
                        sampleData.add(mm100ToInches(s));
                    }
                }
                    buf.rewind();

                    if ((extent.x == 0) && (extent.y == 0)) {
                        Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                        if ((extent.width == coord.width)
                                && (extent.height == coord.height)) {
                            extent = coord;
                        } else {
                            xmrg = null;
                            return;
                        }
                    }
                    subGrid = new HRAPSubGrid(extent);

                    gridGeometry = MapUtil.getGridGeometry(subGrid);

                    project(gridGeometry.getCoordinateReferenceSystem());
                } else {
                    buf = null;
                    xmrg = null;
                }
            } catch (Exception e) {
                xmrg = null;
                statusHandler
                        .warn("Failed to load data from XMRG file " + fname, e);
            }
        } else {
            // this just loads current working hour requested
            try {
                if (xmrg != null) {
                    xmrg.load();
                    HydroDisplayManager.getInstance()
                            .setDataDate(xmrg.getHeader().getValidDate());
                    dataDate = xmrg.getHeader().getValidDate();
                    data = xmrg.getData();
                    buf = FloatBuffer.allocate(data.length);
                    sampleData = new ArrayList<>(data.length);
                    for (short s : data) {
                        // Map <0 and 0 to the first two color segments of the color scale, respectively
                        if (s < 0) {
                            buf.put(0.0f);
                            sampleData.add(0.0f);
                        } else if (s == 0) {
                            buf.put(1.0f);
                            sampleData.add(0.0f);
                        } else {
                            float f = (float) Math.floor(cvt.convert(s));
                            buf.put(f);
                            sampleData.add(mm100ToInches(s));
                        }
                    }
                    buf.rewind();
                    extent = xmrg.getHrapExtent();

                } else {
                    buf = FloatBuffer.allocate(floatData.length);
                    for (float f : floatData) {
                        if (f < 0) {
                            f = -9999;
                            buf.put(0.0f);
                        } else {
                            buf.put((float) Math.floor(cvt.convert(f)));
                        }
                    }
                }

                if ((extent.x == 0) && (extent.y == 0)) {
                    Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                    if ((extent.width == coord.width)
                            && (extent.height == coord.height)) {
                        extent = coord;
                    } else {
                        xmrg = null;
                        return;
                    }
                }
                subGrid = new HRAPSubGrid(extent);

                gridGeometry = MapUtil.getGridGeometry(subGrid);

                project(gridGeometry.getCoordinateReferenceSystem());

            } catch (FileNotFoundException e) {
                xmrg = null;
                statusHandler.debug("XMRG file not found", e);
            } catch (Exception e) {
                xmrg = null;
                statusHandler.warn("Failed to load XMRG data", e);
            }
        }
    }

    @Override
    public void initInternal(IGraphicsTarget target) {
    }

    @Override
    public void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (buf == null) {
            return;
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(buf, gridGeometry, this);
        }

        GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                paintProps, brightness, contrast, isInterpolated);

        gridDisplay.paint(target, giProps);
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> Values = interrogate(coord);

        if (Values == null) {
            return "NO DATA";
        } else {
            return Values.get("Value").toString();
        }
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        if (isGridded) {
            if (xmrg == null) {
                return null;
            }
        } else {
            if (floatData == null) {
                return null;
            }
        }

        Map<String, Object> values = new HashMap<>();

        try {
            Coordinate gridCell = coord.asGridCell(
                    HRAP.getInstance().getGridGeometry(),
                    PixelInCell.CELL_CENTER);

            Point p = new Point((int) gridCell.x, (int) gridCell.y);

            values.put("X", Integer.toString(p.x));
            values.put("Y", Integer.toString(p.y));
            values.put("Value", "");
            values.put("County", "Not Defined");
            values.put("Basin", "Not Defined");

            Rectangle extent = subGrid.getExtent();
            if (extent.contains(p)) {
                int x = p.x - extent.x;
                int y = extent.height - 1 - (p.y - extent.y);
                double value = sampleData.get(y * subGrid.getNx() + x);

                DecimalFormat df = new DecimalFormat(
                        parameters.getFormatString());
                values.put("Value", df.format(value));
            }

            ISpatialQuery query = SpatialQueryFactory.create();

            org.locationtech.jts.geom.Point point = gf
                    .createPoint(coord.asLatLon());

            SpatialQueryResult[] results = query.query("county",
                    new String[] { "countyname" }, point, null, false,
                    SearchMode.WITHIN);

            String county = null;
            if ((results != null) && (results.length > 0)) {
                county = (String) results[0].attributes.get("countyname");
            }

            if ((county == null) || (county.length() < 0)) {
                values.put("County", county);
            }

            results = query.query("basins", new String[] { "name" }, point,
                    null, false, SearchMode.WITHIN);

            String basin = null;
            if ((results != null) && (results.length > 0)) {
                basin = (String) results[0].attributes.get("name");
            }

            if ((basin == null) || (basin.length() < 0)) {
                values.put("Basin", basin);
            }
        } catch (Exception e) {
            throw new VizException("Error performing interrogation", e);
        }

        return values;
    }

    /**
     * Update the Xmrg Display.
     *
     * @param reload
     *            Reread the data from the file if true
     */
    public void updateXmrg(boolean reload) {
        if (xmrg == null) {
            return;
        }

        try {
            if (reload || (data == null)) {
                cvt = parameters.getDataToImageConverter();
                data = xmrg.getData();
            }
            buf = FloatBuffer.allocate(data.length);
            sampleData = new ArrayList<Float>(data.length);
            for (short s : data) {
                // Map <0 and 0 to the first two color segments of the color scale, respectively
                if (s < 0) {
                    buf.put(0.0f);
                    sampleData.add(0.0f);
                } else if (s == 0){
                    buf.put(1.0f);
                    sampleData.add(0.0f);
                } else {
                    // Map values >0 to appropriate color bar segment
                    float f = (float) Math.floor(cvt.convert(s));
                    buf.put(f);
                 // mm/100 to inch
                    sampleData.add(s * 0.03937f / 100);
                }
            }
            buf.rewind();
            Rectangle extent = xmrg.getHrapExtent();
            if ((extent.x == 0) && (extent.y == 0)) {
                Rectangle coord = HRAPCoordinates.getHRAPCoordinates();
                if ((extent.width == coord.width)
                        && (extent.height == coord.height)) {
                    extent = coord;
                } else {
                    xmrg = null;
                    return;
                }
            }
            subGrid = new HRAPSubGrid(extent);

            gridGeometry = MapUtil.getGridGeometry(subGrid);

            project(gridGeometry.getCoordinateReferenceSystem());

            issueRefresh();
        } catch (Exception e) {
            statusHandler.warn("Failed to update display", e);
        }
    }

    public XmrgFile getXmrgFile() {
        return xmrg;
    }

    public short[] getData() {
        return data;
    }

    public void setData(short[] data) {
        this.data = data;
    }
}
