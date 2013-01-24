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
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
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
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.time.DataTime;
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
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Flash Flood Guidance Grid Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009 2256       mpduff      Initial creation.  Moved here
 *                                     for additional functionality.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FFGGridResource extends
        AbstractVizResource<FFGGridResourceData, MapDescriptor> {

    /**
     * Data duration.
     */
    private int duration;

    /**
     * The Unit Converter
     */
    private UnitConverter cvt;

    /** The ColorMapParameters */
    private ColorMapParameters parameters;

    /**
     * List of Colorvalue objects
     */
    private List<Colorvalue> colorSet;

    /**
     * The data in a float[]
     */
    private float[] data;

    /** Buffer to hold the data for display */
    private FloatBuffer buf;

    /** The GriddedImageDisplay */
    private GriddedImageDisplay2 gridDisplay;

    /** The grid geometry */
    private GridGeometry2D gridGeometry;

    /** The image brightness */
    private float brightness = 1.0f;

    /** The image contrast */
    private float contrast = 1.0f;

    private boolean isInterpolated;

    private GridRecord gr;

    private Rectangle rfcExtent;

    /**
     * Constructor
     */
    public FFGGridResource(FFGGridResourceData data, LoadProperties props) {
        super(data, props);
        dataTimes = new ArrayList<DataTime>();
        String user_id = System.getProperty("user.name");
        this.duration = data.getDuration();
        colorSet = HydroDisplayManager.getInstance().getFFGColorMap(user_id,
                "FFG", duration);

        loadData();
    }

    private void loadData() {
        gr = resourceData.getGridRecord();

        if (gr == null) {
            return;
        }

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
        Unit<?> dataUnit = SI.MILLIMETER;

        parameters.setFormatString("0.00");

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);
        cvt = parameters.getDataToImageConverter();

        gridGeometry = gr.getLocation().getGridGeometry();

        try {
            data = new float[((float[]) gr.getMessageData()).length];
            data = (float[]) gr.getMessageData();
            HydroDisplayManager.getInstance().setDataDate(
                    gr.getDataTime().getRefTime());

            buf = FloatBuffer.allocate(data.length);
            for (float f : data) {
                f = (float) Math.floor(cvt.convert(f));
                if (f < 0) {
                    f = -9999f;
                }
                buf.put(f);
            }
            buf.rewind();

            HRAP hrap;
            Coordinate org = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(0, 0), PixelOrientation.LOWER_LEFT,
                    gr.getLocation());

            int nx = gr.getLocation().getNx();
            int ny = gr.getLocation().getNy();

            hrap = HRAP.getInstance();

            Coordinate ulRfcNationalScale = hrap.latLonToGridCoordinate(org,
                    PixelOrientation.CENTER);

            rfcExtent = new Rectangle((int) ulRfcNationalScale.x,
                    (int) ulRfcNationalScale.y - ny, nx, ny);

            project(gridGeometry.getCoordinateReferenceSystem());

        } catch (Exception e) {
            e.printStackTrace();
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
        if ((buf == null) || (buf.array().length == 0)) {
            noData = " No Data Available";
        }

        String name = null;
        ResolutionLevel res = this.resourceData.getResolution();
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        int hours = this.resourceData.getDuration()
                / HydroConstants.SECONDS_PER_HOUR;
        String hourStr = "hour";
        if (hours != 1) {
            hourStr = hourStr.concat("s");
        }

        if (this.resourceData.getDataDate() == null) {
            name = "FFG No Data Available";
        } else {
            name = "FFG " + res.getResolution() + " " + hours + " " + hourStr
                    + " " + sdf.format(this.resourceData.getDataDate())
                    + noData;
        }

        return name;
    }

    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (buf == null) {
            return;
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay2(buf, gridGeometry, this);
        }

        gridDisplay.paint(target, paintProps);
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        this.descriptor = descriptor;
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
        Map<String, Object> values = interrogate(coord);

        if ((data == null) || (data.length == 0)) {
            return "No Data";
        }

        return (String) values.get("Value");
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
        if (buf == null) {
            return null;
        }

        Map<String, Object> values = new HashMap<String, Object>();

        try {
            int nx = gr.getLocation().getNx();
            int ny = gr.getLocation().getNy();

            Coordinate gridCell = coord.asGridCell(HRAP.getInstance()
                    .getGridGeometry(), PixelInCell.CELL_CORNER);

            Point p = new Point((int) gridCell.x, (int) gridCell.y);
            values.put("X", Integer.toString(p.x));
            values.put("Y", Integer.toString(p.y));
            values.put("Value", "");

            if (rfcExtent.contains(p)) {
                int x = p.x - rfcExtent.x;
                int y = rfcExtent.height - (p.y - rfcExtent.y) + 1;

                float f = data[y * nx + x - 1];

                double d = parameters.getDataToDisplayConverter().convert(f);

                if (d < 0) {
                    values.put("Value", String.valueOf(-9999.0));
                } else {
                    DecimalFormat df = new DecimalFormat(
                            parameters.getFormatString());

                    values.put("Value", df.format(d));
                }
            }

        } catch (TransformException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY, StatusConstants.SUBCATEGORY,
            // e.getLocalizedMessage(), e);
            e.printStackTrace();
        } catch (FactoryException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY, StatusConstants.SUBCATEGORY,
            // e.getLocalizedMessage(), e);
            e.printStackTrace();
        } catch (Exception e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY, StatusConstants.SUBCATEGORY,
            // e.getLocalizedMessage(), e);
            e.printStackTrace();
        }
        return values;
    }
}
