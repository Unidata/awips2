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

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PAXmrgResource extends
        AbstractVizResource<PAXmrgResourceData, MapDescriptor> {

    private static final GeometryFactory gf = new GeometryFactory();
    private static final String PRECIP_BIAS = "PRECIP_BIAS";
    private static final String PRECIP_RATIO = "PRECIP_RATIO";


    private XmrgFile xmrg;

    private HRAPSubGrid subGrid;

    private GriddedImageDisplay gridDisplay;

    private ColorMapParameters parameters;

    private final float brightness = 1.0f;

    private final float contrast = 1.0f;

    private boolean isInterpolated;

    private GriddedContourDisplay contourDisplay;

    private GridGeometry2D gridGeometry;

    private FloatBuffer buf;

    private FloatBuffer cbuf;

    private List<Colorvalue> colorSet;

    private DisplayFieldData dataType;

    /** Array of xmrg data values */
    private short[] data;

    private float[] floatData;

    private UnitConverter cvt;

    private String cvUse;

    private boolean isGridded = true;

    private Rectangle extent;

    /** The date of the data */
    private Date dataDate;

    public PAXmrgResource(PAXmrgResourceData resourceData, String cvUse,
            XmrgFile xmrg, List<Colorvalue> colorSet) {
        super(resourceData, new LoadProperties());
        this.colorSet = colorSet;
        this.cvUse = cvUse;
        this.xmrg = xmrg;
        this.resourceData = resourceData;
        this.isGridded = true;
        loadData();
    }

    public PAXmrgResource(PAXmrgResourceData resourceData, String cvUse,
            float[] floatData, List<Colorvalue> colorSet, Rectangle extent) {
        super(resourceData, new LoadProperties());
        this.colorSet = colorSet;
        this.cvUse = cvUse;
        this.resourceData = resourceData;
        this.floatData = floatData;
        this.extent = extent;
        this.dataDate = MPEDisplayManager.getCurrent().getCurrentDisplayedDate();
        this.isGridded = false;
        loadData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (buf == null) {
            return;
        }

        if (gridDisplay == null) {
            gridDisplay = new GriddedImageDisplay(buf, descriptor, gridGeometry);

            gridDisplay.setColorMapParameters(getCapability(
                    ColorMapCapability.class).getColorMapParameters());
        }

        GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                paintProps, brightness, contrast, isInterpolated);

        gridDisplay.paint(target, giProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        // Delete the gridDisplay so it can be regenerated in the correct
        // projection
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
     * .uf.common.geospatial.ReferencedCoordinate)
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
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        // This returns the sampling text
        if (isGridded) {
            if (xmrg == null) {
                return null;
            }
        } else {
            if (this.floatData == null) {
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

                double value;
                if (xmrg != null) {
                    short s = xmrg.getData()[y * subGrid.getNx() + x];
                    value = parameters.getDataToDisplayConverter().convert(s);
                } else {
                    float f = floatData[y * subGrid.getNx() + x];
                    value = f;
                }

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        // This is the name that shows up in the legend
        return "Post Analysis XMRG Data";
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
   
   
    	
    	 //Chip Change that was removed, at least temporarily
    	//This change allow the displayUnit to be set differently for the bias map,
    	//the problem is that the map ALSO displays precip
      
        
      //  parameters.setDataUnit(dataUnit);
        
        if (cvUse.equals(PRECIP_RATIO ))
        {
        	dataUnit = displayUnit;
        	parameters.setDataUnit(dataUnit);
        }
        else
        {
        	parameters.setDataUnit(dataUnit);
        }
  
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
        String header = "PAXmrgResource.loadData(): ";
        
        try {
            // Load the xmrg data
            if (xmrg != null) {
                xmrg.load();
                this.dataDate = xmrg.getHeader().getValidDate();
                data = xmrg.getData();
                buf = FloatBuffer.allocate(data.length);
                float f = 0.0f;
                String temps = "TEMP";
                int tempsval = cvUse.indexOf(temps);
                for (short s : data) {
                    if (s < 0) {
                        if ((s == -9999) || (s == -999) || (s == -99)
                                || ((s == -9) && (tempsval == -1))) {
                            f = 0;
                        } else if ((s == -8888) || (s == -899)) {
                            f = 1;
                        } else {
                            f = (float) cvt.convert(s);
                        }
                    } else {
                        if ((s < 30) && (s > 24)) {
                            s = 26;
                        }
                        f = (float) cvt.convert(s);           
                    }
                    buf.put(f);
                }
                buf.rewind();
                this.extent = xmrg.getHrapExtent();

            } else {
            // Load and array of floats    
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
