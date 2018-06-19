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

import java.awt.Rectangle;
import java.io.IOException;
import java.nio.FloatBuffer;
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

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;

/**
 * Hydroview Time Lapse Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2009 2258       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeLapseResource extends
        AbstractVizResource<XmrgResourceData, MapDescriptor> {

    private List<Colorvalue> colorSet;

    private ColorMapParameters parameters;

    private Map<DataTime, GriddedImageDisplay> bufferMap;

    private GriddedImagePaintProperties giProps = null;

    private XmrgFile xmrg;

    private short[] data;

    private HRAPSubGrid subGrid;

    private GriddedImageDisplay gridDisplay;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean isInterpolated;

    private GridGeometry2D gridGeometry;

    private boolean keepLooping = false;

    protected DataTime displayedDate;
    
    private String cvUse = null;
    
    private int numFrames = 1;
    
    private Date dataDate = null;

    private UnitConverter cvt;

    public TimeLapseResource(String cvUse, int numFrames, XmrgFile xmrg,
            List<Colorvalue> colorSet, Date dataDate) {
        super(new XmrgResourceData(), new LoadProperties());
        this.colorSet = colorSet;
        this.cvUse = cvUse;
        this.numFrames= numFrames;
        this.xmrg = xmrg;
        this.dataDate = dataDate;
        dataTimes = new ArrayList<DataTime>();
        descriptor = (MapDescriptor) HydroDisplayManager.getInstance().getPane().getDescriptor();
        loadData();
    }

    @Override
    protected void disposeInternal() {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }

        if (bufferMap != null) {
            bufferMap.clear();
        }
    }
    
    private void loadData() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        HydroDisplayManager displayMgr = HydroDisplayManager.getInstance();
        
        ColorMap colorMap = new ColorMap(colorSet.size());
        colorMap.setName(cvUse);

        DataMappingPreferences dmPref = new DataMappingPreferences();
        int index = 0;
        for (Colorvalue cv : colorSet) {
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            colorMap.setColor(index, new Color(rgb.red / 255f,
                    rgb.green / 255f, rgb.blue / 255f));

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

        // Get all the files for the loop
        try {

            String dirname = appsDefaults.getToken(HydroConstants.XMRG_DIR_TOKEN);
            String fname = "";

            bufferMap = new HashMap<DataTime, GriddedImageDisplay>(numFrames);
            
            Calendar cal1 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal1.setTime(displayMgr.getDataDate());
            Calendar cal2 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal2.setTime(cal1.getTime());

            for (int i = 0; i < numFrames; i++) {
                cal2.setTime(cal1.getTime());
                cal2.add(Calendar.SECOND,
                        -(i * HydroConstants.SECONDS_PER_HOUR));
                DataTime dTime = new DataTime(cal2.getTime());
                dataTimes.add(dTime);
                
                String dtform = HydroConstants.QPE_DATE_FORMAT.format(cal2
                        .getTime());
                fname = FileUtil.join(dirname, cvUse + dtform + "z");
                XmrgFile wmrg = null;

                try {
                    wmrg = new XmrgFile(fname);
                    wmrg.load();
                } catch (IOException io) {
                    System.out.println("XMRG file not found " + fname);
                    continue;
                }

                xmrg = wmrg;
                data = xmrg.getData();
                Rectangle extent = xmrg.getHrapExtent();

                FloatBuffer buf = FloatBuffer.allocate(data.length);
                for (short s : data) {
                    float f = (float) Math.floor(cvt.convert(s));
                    buf.put(f);
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

                gridDisplay = new GriddedImageDisplay(buf, descriptor,
                        gridGeometry);

                gridDisplay.setColorMapParameters(getCapability(
                        ColorMapCapability.class).getColorMapParameters());
                bufferMap.put(dTime, gridDisplay);
                
                project(gridGeometry.getCoordinateReferenceSystem());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        displayedDate = paintProps.getDataTime();

        if ((bufferMap == null) || (displayedDate == null)
                || (HydroDisplayManager.getInstance().isTimeLapseMode() == false)) {
            return;
        }

        if (giProps == null) {
            giProps = new GriddedImagePaintProperties(paintProps,
                    brightness, contrast, isInterpolated);
        }

        gridDisplay = bufferMap.get(displayedDate);
        if (gridDisplay != null) {
            gridDisplay.paint(target, giProps);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        if (xmrg == null) {
            return "No Data Available";
        }

        if (displayedDate == null) {
            return "No Data Available";
        }
        
        String retVal = null;
        
        if (HydroDisplayManager.getInstance().getCvUse().equalsIgnoreCase("xmrg")) {
            retVal = "Saved Precip Estimate (in) " + HydroConstants.DISPLAY_DATE_FORMAT.format(displayedDate.getRefTime()) + "z";
        } else {
            retVal = "RFC Best Estimate Mosaic " + HydroConstants.DISPLAY_DATE_FORMAT.format(displayedDate.getRefTime()) + "z";
        }
        
        return retVal;
        // return sdf.format(xmrg.getHeader().getValidDate()) + "z  site="
        // + MPEDataManager.getInstance().getRFC() + "  "
        // + dataType.toString();
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (gridDisplay != null) {
            gridDisplay.dispose();
            gridDisplay = null;
        }
    }

    /**
     * @return the keepLooping
     */
    public boolean isKeepLooping() {
        return keepLooping;
    }

    /**
     * @param keepLooping
     *            the keepLooping to set
     */
    public void setKeepLooping(boolean keepLooping) {
        this.keepLooping = keepLooping;
    }
}
