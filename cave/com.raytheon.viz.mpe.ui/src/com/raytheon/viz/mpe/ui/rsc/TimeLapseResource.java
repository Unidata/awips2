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
import java.io.IOException;
import java.nio.Buffer;
import java.nio.FloatBuffer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.DataTime;
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
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarLoc;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;

/**
 * The Time Lapse looping functionality display resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeLapseResource extends
        AbstractVizResource<XmrgResourceData, MapDescriptor> implements
        IMpeResource {

    private final MPEDisplayManager displayMgr;

    private final List<Colorvalue> colorSet;

    private ColorMapParameters parameters;

    private final DisplayFieldData dataType;

    private Map<DataTime, GriddedImageDisplay2> bufferMap;

    private final HashMap<DataTime, GriddedImageData> dataMap;

    private XmrgFile xmrg;

    private short[] data;

    private HRAPSubGrid subGrid;

    private GridGeometry2D gridGeometry;

    private boolean keepLooping = false;

    protected DataTime displayedDate;

    private static final SimpleDateFormat sdf;

    private static class GriddedImageData {
        Buffer buff = null;

        GridGeometry2D geometry = null;
    }

    static {
        sdf = new SimpleDateFormat("MMM dd yyyy HH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    public TimeLapseResource(MPEDisplayManager displayMgr,
            DisplayFieldData dataType, List<Colorvalue> colorSet) {
        super(new XmrgResourceData(), new LoadProperties());
        this.displayMgr = displayMgr;
        this.dataType = dataType;
        this.colorSet = colorSet;
        dataTimes = new ArrayList<DataTime>();
        descriptor = (MapDescriptor) MPEDisplayManager.getCurrent()
                .getRenderableDisplay().getDescriptor();

        dataMap = new HashMap<DataTime, GriddedImageData>();
        loadData();
    }

    @Override
    protected void disposeInternal() {

        if (bufferMap != null) {
            bufferMap.clear();
        }

        for (DataTime dTime : bufferMap.keySet()) {
            GriddedImageDisplay2 gDisplay = bufferMap.get(dTime);
            if (gDisplay != null) {
                gDisplay.dispose();
            }
        }

        bufferMap.clear();
    }

    private void loadData() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        ColorMap colorMap = new ColorMap(colorSet.size());

        DisplayFieldData fieldData = displayMgr.getDisplayFieldType();
        colorMap.setName(fieldData.getCv_use());
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
        DataMappingEntry entry = new DataMappingEntry();
        entry.setPixelValue((double) (index - 1));
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

        switch (dataType) {
        case Locbias:
            displayUnit = Unit.ONE;
            dataUnit = Unit.ONE.divide(100);
            break;

        case Height:
            displayUnit = NonSI.FOOT;
            dataUnit = SI.METER;
            break;

        case Index:
            int j = 2;
            for (MPERadarLoc radar : MPEDataManager.getInstance().getRadars()) {
                dmPref.getEntries().get(j++).setLabel(radar.getId());
            }
            while (j < dmPref.getEntries().size()) {
                dmPref.getEntries().get(j++).setLabel("");
            }
        case Locspan:
            dmPref.getEntries().get(1).setLabel("mis");

            displayUnit = Unit.ONE;
            dataUnit = Unit.ONE;
            break;

        case Prism:
            displayUnit = NonSI.INCH;
            dataUnit = SI.MILLIMETER;
            parameters.setFormatString("0.00");
            break;

        case mintempPrism:
        case maxtempPrism:
            displayUnit = NonSI.FAHRENHEIT;
            dataUnit = NonSI.FAHRENHEIT.divide(10);
            break;

        default:
            displayUnit = NonSI.INCH;
            dataUnit = SI.MILLIMETER.divide(100);
            parameters.setFormatString("0.00");

        }

        parameters.setDisplayUnit(displayUnit);
        parameters.setImageUnit(dmPref.getImageUnit(displayUnit));
        parameters.setDataUnit(dataUnit);

        parameters.setColorMapMax(parameters.getColorMap().getSize() - 1);
        parameters.setColorMapMin(0);
        parameters.setDataMax(parameters.getColorMap().getSize() - 1);
        parameters.setDataMin(0);

        UnitConverter cvt = parameters.getDataToImageConverter();

        // Get all the files for the loop
        try {

            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = "";

            // lapse hours
            int nhours = displayMgr.getTimeLapseHours();

            bufferMap = new HashMap<DataTime, GriddedImageDisplay2>(nhours);
            Calendar cal1 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal1.setTime(displayMgr.getCurrentDate());
            Calendar cal2 = Calendar.getInstance((TimeZone.getTimeZone("GMT")));
            cal2.setTime(cal1.getTime());

            for (int i = 0; i < nhours; i++) {
                cal2.setTime(cal1.getTime());
                cal2.add(Calendar.SECOND,
                        -(i * HydroConstants.SECONDS_PER_HOUR));
                DataTime dTime = new DataTime(cal2.getTime());
                dataTimes.add(dTime);
                String dtform = HydroConstants.FILE_DATE_FORMAT.format(cal2
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
                if (xmrg.getHrapExtent() == null) {
                    continue;
                }
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

                GriddedImageData grid = new GriddedImageData();
                grid.geometry = gridGeometry;
                grid.buff = buf;

                dataMap.put(dTime, grid);
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
                || (MPEDisplayManager.getCurrent().isTimeLapseMode() == false)) {
            return;
        }

        Set<DisplayMode> mode = displayMgr.getDisplayMode();

        if (mode.contains(DisplayMode.Image)) {
            GriddedImageDisplay2 gridDisplay = bufferMap.get(displayedDate);
            if (gridDisplay == null) {
                GriddedImageData dat = dataMap.get(displayedDate);
                gridDisplay = new GriddedImageDisplay2(dat.buff, dat.geometry,
                        this);

                bufferMap.put(displayedDate, gridDisplay);
                // project(gridGeometry.getCoordinateReferenceSystem());
                gridDisplay = bufferMap.get(displayedDate);
            }

            if (gridDisplay != null) {
                gridDisplay.paint(target, paintProps);
            }
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

        return sdf.format(displayedDate.getRefTime()) + "z site="
                + MPEDataManager.getInstance().getRFC() + "  "
                + dataType.toString();
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (DataTime dTime : bufferMap.keySet()) {
            GriddedImageDisplay2 gDisplay = bufferMap.get(dTime);
            if (gDisplay != null) {
                gDisplay.project(descriptor.getGridGeometry());
            }
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
