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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class XmrgResource extends
        AbstractVizResource<XmrgResourceData, MapDescriptor> {
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

    private ArrayList<Float> sampleData;

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    public void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        String noData = "";
        if ((xmrg == null) && (floatData == null)) {
            noData = " No Data Available";
        }
        HydroDisplayManager dman = HydroDisplayManager.getInstance();
        String name = null;
        if (cv_use.equals("FFG")) {
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
                    + HydroConstants.DISPLAY_DATE_FORMAT.format(dman
                            .getDataDate()) + "z (in)" + noData;
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
        dmPref.getEntries().get(1).setLabel("");

        ColorMapCapability cmc = getCapability(ColorMapCapability.class);

        parameters = cmc.getColorMapParameters();
        if (parameters == null) {
            parameters = new ColorMapParameters();
            cmc.setColorMapParameters(parameters);
        }
        parameters.setColorMap(colorMap);
        parameters.setDataMapping(dmPref);

        Unit<?> displayUnit = NonSI.INCH;
        Unit<?> dataUnit = SI.MILLIMETER.divide(100);

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
                e1.printStackTrace();
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
                    String dtform = HydroConstants.QPE_DATE_FORMAT.format(cal2
                            .getTime());
                    fname = FileUtil.join(dirname, cv_use + dtform + "z");
                    XmrgFile wmrg = null;

                    try {
                        wmrg = new XmrgFile(fname);
                        wmrg.load();
                    } catch (IOException io) {
                        System.out.println("XMRG file not found " + fname);
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
                sampleData = new ArrayList<Float>(data.length);

                for (short s : data) {
                    float f = (float) Math.floor(cvt.convert(s));
                    buf.put(f);
                    // mm/100 to inch
                    sampleData.add(s * 0.03937f / 100);
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

            } catch (Exception e) {
                xmrg = null;
                System.err.println("XMRG file not found " + fname);
            }
        } else {
            // this just loads current working hour requested
            try {
                if (xmrg != null) {
                    xmrg.load();
                    HydroDisplayManager.getInstance().setDataDate(
                            xmrg.getHeader().getValidDate());
                    dataDate = xmrg.getHeader().getValidDate();
                    data = xmrg.getData();
                    buf = FloatBuffer.allocate(data.length);
                    sampleData = new ArrayList<Float>(data.length);
                    for (short s : data) {
                        if (s < 0) {
                            buf.put(0.0f);
                            sampleData.add(0.0f);
                        } else {
                            float f = (float) Math.floor(cvt.convert(s));
                            buf.put(f);

                            // mm/100 to inch
                            sampleData.add(s * 0.03937f / 100);
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

            } catch (Exception e) {
                xmrg = null;
                System.err.println("XMRG file not found");
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    public void initInternal(IGraphicsTarget target) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> Values = interrogate(coord);

        if (Values == null) {
            return "NO DATA";
        } else {
            return Values.get("Value").toString();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
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

        Map<String, Object> values = new HashMap<String, Object>();

        try {
            Coordinate gridCell = coord.asGridCell(HRAP.getInstance()
                    .getGridGeometry(), PixelInCell.CELL_CENTER);

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

            com.vividsolutions.jts.geom.Point point = gf.createPoint(coord
                    .asLatLon());

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
            for (short s : data) {
                float f = (float) Math.floor(cvt.convert(s));
                buf.put(f);
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
            e.printStackTrace();
        }
    }

    /**
     * @return the xmrg
     */
    public XmrgFile getXmrgFile() {
        return xmrg;
    }

    /**
     * @return the data
     */
    public short[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(short[] data) {
        this.data = data;
    }
}
