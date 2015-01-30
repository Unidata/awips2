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

import java.io.File;
import java.io.FileInputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.MPECommandConstants;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarLoc;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.IDisplayFieldChangedListener;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.GageColor;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.GageDisplay;
import com.raytheon.viz.mpe.ui.MPEFontFactory;
import com.raytheon.viz.mpe.ui.dialogs.Display7x7Dialog;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * Displays MPE Gage data, handler for popping up 7x7 dialog as well
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2011            mschenke     Initial creation
 * Aug 8, 2012   15271	  snaples      Updated hourly slot
 * Aug 17, 2012  15271    snaples      Added check to add only PP gages
 * Sep 5, 2012   15079    snaples      Added constant for Milli to inches conversion factor
 * Feb 12, 2013  15773    snaples      Updated addPoints to display PC gages when token is set to use PC data.
 * Mar 14, 2013  1457     mpduff       Fixed various bugs.
 * Apr 19, 2013  1920     mpduff       Fixed gage color contrast, add e to display value of manually edited gages.
 * Dec 12, 2014  15689    cgobs        Fixed problem with mismatched color set for Color by value color scale (RM 15689 = DIM 17541
 * Dec 12, 2014  16748    cgobs        Fixed problem with missing "d" for disagged gages (RM 16748 = DIM 17606).
 *                                     
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEGageResource extends AbstractMPEInputResource implements
        IDisplayFieldChangedListener {

    public static final String APPLICATION_NAME = "hmapmpe";

    private static final String GAGE_TRIANGLES = "GAGETRIANGLES%sz";

    private static final double POINT_RADIUS = 2;

    private final SimpleDateFormat sdf;

    private final Object mutex = new Object();

    private MPEDisplayManager displayMgr;

    private Hashtable<Coordinate, MPEGageData> dataMap = null;

    private STRtree strTree = null;

    private final RGB triangleColor = RGBColors.getRGBColor("YELLOW");

    private final double MILLICVT = 25.4;

    private ColorMapParameters parameters;
    private Colorvalue[] colorvalueArray;
    private  float[] thresholdArray;
    private RGB[] rgbArray;

    private Display7x7Dialog dialog;

    private Date lastDate = null;

    private IWireframeShape gageTriangles;

    private MPEFontFactory fontFactory;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private final Map<GageDisplay, Boolean> displayTypes = new HashMap<GageDisplay, Boolean>();

    /**
     * Constructor.
     * 
     * @param resourceData
     *            The resourceData
     * @param loadProperties
     *            The loadProperties
     */
    public MPEGageResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        setStates();
    }

    /**
     * Set the state of the Gage display for values and Ids
     */
    private void setStates() {
        boolean stateSetting = MPEDisplayManager
                .getToggleState(MPECommandConstants.TOGGLE_GAGE_VALUE_COMMAND_ID);
        displayTypes.put(GageDisplay.Values, stateSetting);

        stateSetting = MPEDisplayManager
                .getToggleState(MPECommandConstants.TOGGLE_GAGEID_COMMAND_ID);
        displayTypes.put(GageDisplay.Ids, stateSetting);

        // Triangles default to off
        MPEDisplayManager.setToggleState(
                MPECommandConstants.TOGGLE_GAGE_TRIANGLE_COMMAND_ID, false);
        displayTypes.put(GageDisplay.Triangles, false);
    }

    /**
     * Toggles visibility of {@link GageDisplay} type
     * 
     * @param display
     *            The GageDisplay
     * @param isOn
     *            true if on
     */
    public void toggleGageDisplay(GageDisplay display, boolean isOn) {
        displayTypes.put(display, isOn);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        displayMgr = MPEDisplayManager.getInstance(descriptor
                .getRenderableDisplay());
        displayMgr.registerDisplayFieldChangedListener(this);
        fontFactory = new MPEFontFactory(target, this);
        loadColors();
        reloadGages();
    }

    @Override
    protected void disposeInternal() {
        if (gageTriangles != null) {
            gageTriangles.dispose();
        }
        fontFactory.dispose();
        displayMgr.unregisterDisplayFieldChangedListener(this);
    }

    /**
     * Trigger the resource to reload the gages for the current time
     */
    public void reloadGages() {
        synchronized (mutex) {
            lastDate = null;
            if (gageTriangles != null) {
                gageTriangles.dispose();
                gageTriangles = null;
            }
        }
        lastDate = displayMgr.getCurrentEditDate();
        addPoints(MPEDataManager.getInstance().readGageData(lastDate, lastDate));
        issueRefresh();
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
        if (gageTriangles != null) {
            gageTriangles = null;
        }
        lastDate = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (!displayTypes.containsValue(Boolean.TRUE)) {
            // Nothing to paint
            return;
        }

        Date curDate = displayMgr.getCurrentEditDate();
        synchronized (mutex) {
            if (curDate != null && curDate.equals(lastDate) == false) {
                lastDate = curDate;
                addPoints(MPEDataManager.getInstance().readGageData(lastDate,
                        lastDate));
            }
        }

        if (displayTypes.get(GageDisplay.Ids)
                || displayTypes.get(GageDisplay.Values)) {
            paintPlotInfo(target, paintProps,
                    displayTypes.get(GageDisplay.Ids),
                    displayTypes.get(GageDisplay.Values));
        }

        try {
            if (displayTypes.get(GageDisplay.Triangles)) {
                paintTriangles(target, paintProps);
            }
        } catch (Exception e) {
            Activator.statusHandler
                    .handle(Priority.PROBLEM, "Error painting gage triangles: "
                            + e.getLocalizedMessage(), e);
        }
    }

    /**
     * @param target
     * @param paintProps
     */
    private void paintTriangles(IGraphicsTarget target,
            PaintProperties paintProps) throws Exception {
        if (gageTriangles == null) {
            gageTriangles = target.createWireframeShape(false, descriptor);
            GeometryFactory gf = new GeometryFactory();
            JTSCompiler compiler = new JTSCompiler(null, gageTriangles,
                    descriptor);
            File dir = new File(AppsDefaults.getInstance().getToken(
                    "rfcwide_gagetriangles_dir"));
            String fileName = String.format(GAGE_TRIANGLES,
                    sdf.format(lastDate));
            File file = new File(dir, fileName);
            if (!file.exists()) {
                return;
            }
            ByteBuffer in = ByteBuffer.allocate((int) file.length());
            in.order(ByteOrder.LITTLE_ENDIAN);
            FileChannel fc = new FileInputStream(file).getChannel();
            fc.read(in);
            in.rewind();
            while (in.hasRemaining()) {
                byte[] tmp = new byte[9];
                in.get(tmp);
                int gage_n_contig = in.getInt();
                double lat_1 = in.getDouble();
                double lon_1 = in.getDouble();

                lon_1 *= (lon_1 > 0) ? -1 : 1;
                lat_1 *= (lat_1 < 0) ? -1 : 1;

                for (int i = 0; i < gage_n_contig; ++i) {
                    double lat_2 = in.getDouble();
                    double lon_2 = in.getDouble();
                    lon_2 *= (lon_2 > 0) ? -1 : 1;
                    lat_2 *= (lat_2 < 0) ? -1 : 1;

                    compiler.handle(gf.createLineString(new Coordinate[] {
                            new Coordinate(lon_1, lat_1),
                            new Coordinate(lon_2, lat_2) }));
                }
            }

            gageTriangles.compile();
        }
        // paint triangle shape
        target.drawWireframeShape(gageTriangles, triangleColor, 1.0f);
    }

    /**
     * Draws the plot information
     * 
     * @param target
     * @param paintProps
     * @param isGageIdsDisplayed
     * @param isGageValuesDisplayed
     */
    private void paintPlotInfo(IGraphicsTarget target,
            PaintProperties paintProps, boolean isGageIdsDisplayed,
            boolean isGageValuesDisplayed) throws VizException {
        Rectangle bounds = paintProps.getCanvasBounds();
        IExtent extent = paintProps.getView().getExtent();
        double screenToWorldWidthRatio = bounds.width / extent.getWidth();

        List<DrawableString> strings = new ArrayList<DrawableString>(
                dataMap.size());
        List<DrawableCircle> points = new ArrayList<DrawableCircle>(
                dataMap.size());

        // Fonts are shared and cached, no need to init or dispose
        IFont font = fontFactory.getMPEFont(MPEDisplayManager.getFontId());
        font.setSmoothing(false);

        MPEDisplayManager.GageMissingOptions gm = MPEDisplayManager
                .getGageMissing();
        boolean displayIsEdit = false;
        DataTime paintTime = paintProps.getFramesInfo().getCurrentFrame();
        Date editTime = displayMgr.getCurrentEditDate();
        if (paintTime != null && editTime != null) {
            displayIsEdit = editTime.equals(paintTime.getRefTime());
        }
        boolean xor = MPEDisplayManager.getGageColor() == GageColor.Contrast
                && displayIsEdit;

        for (Coordinate point : dataMap.keySet()) {
            if (extent.contains(new double[] { point.x, point.y })) {
                MPEGageData gageData = dataMap.get(point);
                RGB gageColor = getGageColor(gageData);

                boolean isReportedMissing = gageData.isReported_missing();
                boolean isMissing = ((gageData.getGval() == -999.f || gageData
                        .getGval() == -9999.f) ? true : false);
                if (gageData.isManedit() == true
                        && gageData.getEdit().equalsIgnoreCase("m")) {
                    isMissing = true;
                }

                switch (gm) {
                case MissingNone:
                    if (isMissing) {
                        break;
                    }

                case MissingReported:
                    if ((isMissing) && (!isReportedMissing)) {
                        break;
                    }

                case MissingAll:
                    String gageValue = "";
                    String gageId = "";
                    if (isGageValuesDisplayed) {
                        gageValue = "m";
                        if (!isMissing) {
                            gageValue = String.format("%5.2f",
                                    gageData.getGval());
                        }

                        // draw the value
                        if (!gageData.isManedit()) {
                            if (gageData.getId().contains("PSEUDO")) {
                                gageValue = String.format("%5.2f",
                                        gageData.getGval() / MILLICVT);
                            }
                        } else {
                            if (gageData.getId().contains("PSEUDO")
                                    && !isMissing) {
                                gageValue = String.format("%5.2f",
                                        gageData.getGval() / MILLICVT);
                            }
                        }
                    }

                    if (isGageIdsDisplayed) {
                        gageId = gageData.getId();

                        DrawableCircle circle = new DrawableCircle();
                        circle.radius = POINT_RADIUS / screenToWorldWidthRatio;
                        circle.filled = true;
                        circle.numberOfPoints = 8;
                        circle.setCoordinates(point.x, point.y);
                        circle.basics.color = gageColor;
                        circle.basics.xOrColors = xor;
                        points.add(circle);
                    }

                    if (isGageIdsDisplayed || isGageValuesDisplayed) {
                        if (gageData.isManedit()) {
                            gageValue = gageValue.concat("e");
                        }
                    
                        // if gageData is  result of running disagg in DailyQC
                        // (time-distributed)
                        // append a "d" to the gage value
                        if (gageData.isTd()) {
                            gageValue = gageValue.concat("d");
                        }
                    
                        
                        DrawableString string = new DrawableString(
                                new String[] { gageValue, gageId, }, gageColor);
                        string.font = font;
                        string.basics.xOrColors = xor;
                        string.horizontalAlignment = HorizontalAlignment.LEFT;
                        string.verticallAlignment = VerticalAlignment.BOTTOM;

                        string.setCoordinates(
                                point.x
                                        + (POINT_RADIUS / screenToWorldWidthRatio),
                                point.y
                                        + (POINT_RADIUS / screenToWorldWidthRatio));
                        strings.add(string);
                    }
                }
            }
        }

        target.drawCircle(points.toArray(new DrawableCircle[points.size()]));
        target.drawStrings(strings);
    }

    private RGB getGageColor(MPEGageData gageData) {
        RGB gageColor = new RGB(255, 255, 255);
        if (displayTypes.containsValue(Boolean.TRUE)) {
            MPEDisplayManager.GageColor gc = MPEDisplayManager.getGageColor();
            switch (gc) {
            case Solid:
                gageColor = RGBColors.getRGBColor("SandyBrown");
                break;

            case Contrast:
                gageColor = RGBColors.getRGBColor("SandyBrown");
                break;

            case ByQC:
                int qc = gageData.getQc();
                if (qc == 3) {
                    gageColor = RGBColors.getRGBColor("SandyBrown");
                } else if (qc == 1) {
                    gageColor = RGBColors.getRGBColor("Red");
                } else {
                    gageColor = RGBColors.getRGBColor("Yellow");
                }
                break;

            case ByValue:
                // Check for pseudo gage and convert
                float fltVal = gageData.getGval();
                if (gageData.getId().contains("PSEUDO")) {
                    fltVal = (float) (gageData.getGval() / MILLICVT);
                }
                gageColor = getColorByValue(fltVal);
                break;

            default:
                break;
            }
        }

        return gageColor;
    }

    /**
     * Process the gage data for the resource
     * 
     * @param gages
     */
    private void addPoints(List<MPEGageData> gages) {
        dataMap = new Hashtable<Coordinate, MPEGageData>();
        strTree = new STRtree();
        /* read process PC token from .Apps_defaults */
        String processpc = appsDefaults.getToken("mpe_process_PC");
        boolean process_PC = false;
        if (processpc.equalsIgnoreCase("ON")) {
            process_PC = true;
        }

        if (!gages.isEmpty()) {
            for (ListIterator<MPEGageData> it = gages.listIterator(); it
                    .hasNext();) {
                MPEGageData gageData = it.next();
                // DR15773 Use PC gages only when token set to ON.
                if ((gageData.getPe().equalsIgnoreCase("PC"))
                        && (process_PC == false)) {
                    continue;
                }
                Coordinate latLon = gageData.getLatLon();
                double[] pixel = descriptor.worldToPixel(new double[] {
                        latLon.x, latLon.y });
                dataMap.put(new Coordinate(pixel[0], pixel[1]), gageData);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(latLon.x + .05, latLon.y + .05);
                Coordinate p2 = new Coordinate(latLon.x - .05, latLon.y - .05);
                Envelope env = new Envelope(p1, p2);
                ArrayList<Object> data = new ArrayList<Object>();
                data.add(latLon);
                String newData = "GAGE: " + gageData.getId() + " VALUE: "
                        + gageData.getGval();
                data.add(newData);
                strTree.insert(env, data);
            }
        }
    }
    
    public RGB getColorByValue(float value) {
        
        if (value == -999.0) {
            value = -9999.0f;
        }
        
        int colorIndex = getColorIndex(value, thresholdArray);
        
        
   //     System.out.println("getColorByValue(): colorIndex = " + colorIndex);
        return rgbArray[colorIndex];

    }
  
    
    private int getColorIndex(float value, float[] threshholdValueArray) {
      
 
        int index = -1;
        
        if (threshholdValueArray == null)
        {
            index = -1;
        }
      
        else if (value == threshholdValueArray[0])
        {
            index = 0;
        }
        
        else if (value == threshholdValueArray[1])
        {
            index = 1;
        }
        
        else
        {
            index = threshholdValueArray.length-1;
            for (int i  = 2 ; i < threshholdValueArray.length-1; i++)
            {


                if (value < threshholdValueArray[i+1])
                {
                    index = i;
                    break;       
                }
            }

        }
        
        return index;
    }
    
    private void loadColors() {
        
       loadColorData(
                "PRECIP_ACCUM",//color value use
                1, //duration in hours
                NonSI.INCH, //original data  units
                NonSI.INCH); //display units
       
        return;
 
    }
    
    public void loadColorData(String cvUse,
            int durationInHrs, Unit<?> dataUnit, Unit<?> displayUnit)
    {


        String header =  "MPEGageResource.getColorvalueArray(): ";

        if (durationInHrs == 0) {
            durationInHrs = 1;
        }

        MPEDisplayManager displayMgr =  MPEDisplayManager.getCurrent();
        List<NamedColorUseSet> pColorSetGroup = displayMgr.getColorSetGroup();

        ColorMapParameters params = new ColorMapParameters();
        params.setFormatString("0.00");
        params.setDisplayUnit(displayUnit);
    

        Colorvalue[] colorSet = GetColorValues.get_colorvalues(
                LocalizationManager.getContextName(LocalizationLevel.USER),
                APPLICATION_NAME, cvUse, durationInHrs * 60 * 60, "E",
                pColorSetGroup).toArray(new Colorvalue[0]);

        int numColors = colorSet.length;
        thresholdArray = new float[numColors];
        rgbArray = new RGB[numColors];

        for (int i = 0; i < numColors; ++i) {
            
            Colorvalue cv = colorSet[i];
            String colorName = cv.getColorname().getColorName();
            RGB rgb = RGBColors.getRGBColor(colorName);
        

            float threshold = (float) cv.getId().getThresholdValue();

            thresholdArray[i] = threshold;
            rgbArray[i] = rgb;        
           // System.out.printf(header + "threshold = %f, color = %s\n", threshold, colorName);

        }


        return;    
    }
    
    /**
     * Constructs a new {@link ColorMapParameters} object for the given
     * parameters
     * 
     * @param cvUse
     * @param durationInHrs
     * @param dataUnit
     * @param displayUnit
     * @return
     */
    public static ColorMapParameters createColorMap(String cvUse,
            int durationInHrs, Unit<?> dataUnit, Unit<?> displayUnit) {
        
        
        String header =  "MPEGageResource.createColorMap(): ";
        
        if (durationInHrs == 0) {
            durationInHrs = 1;
        }
        
        MPEDisplayManager displayMgr =  MPEDisplayManager.getCurrent();
        List<NamedColorUseSet> pColorSetGroup = displayMgr.getColorSetGroup();
        
        ColorMapParameters params = new ColorMapParameters();
        params.setFormatString("0.00");
        params.setDisplayUnit(displayUnit);
        params.setDataUnit(dataUnit);

        UnitConverter displayToData = params.getDisplayToDataConverter();
        UnitConverter dataToDisplay = params.getDataToDisplayConverter();

        DataMappingPreferences dm = new DataMappingPreferences();

        Colorvalue[] colorSet = GetColorValues.get_colorvalues(
                LocalizationManager.getContextName(LocalizationLevel.USER),
                APPLICATION_NAME, cvUse, durationInHrs * 60 * 60, "E",
                pColorSetGroup).toArray(new Colorvalue[0]);

     
        DecimalFormat format = new DecimalFormat(params.getFormatString());

        int numColors = colorSet.length;
        float[] red = new float[numColors];
        float[] green = new float[numColors];
        float[] blue = new float[numColors];

        for (int i = 0; i < numColors; ++i) {
            Colorvalue cv = colorSet[i];
            String colorName = cv.getColorname().getColorName();
            RGB rgb = RGBColors.getRGBColor(cv.getColorname().getColorName());
            red[i] = rgb.red / 255f;
            green[i] = rgb.green / 255f;
            blue[i] = rgb.blue / 255f;

            double threshold = cv.getId().getThresholdValue();
            
            
            System.out.printf(header + "threshold = %f, color = %s\n", threshold, colorName);
            
            DataMappingEntry entry = new DataMappingEntry();
            if (threshold == -9999.0 || threshold == -8888.0) {
                entry.setDisplayValue(threshold);
                entry.setOperator("<");
                entry.setLabel("");
            } else {
                // Convert display to data, cast to int, convert back to display
                entry.setDisplayValue(dataToDisplay.convert((int) displayToData
                        .convert(threshold)));
            }
            entry.setPixelValue((double) i);

            dm.addEntry(entry);
        }

        DataMappingEntry entry = new DataMappingEntry();
        entry.setDisplayValue(Double.MAX_VALUE);
        entry.setPixelValue((double) (numColors - 1));
        entry.setLabel("");
        dm.addEntry(entry);

        params.setColorMap(new ColorMap(cvUse, red, green, blue));
        params.setDataMapping(dm);
        params.setDataMin(0);
        params.setDataMax(numColors - 1);
        params.setColorMapMin(params.getDataMin());
        params.setColorMapMax(params.getDataMax());

        

        return params;
    }


    /**
     * Inspect method called when moused over while inspect is enabled
     * 
     * @param coord
     *            The coordinate of the inspection
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            Envelope env = new Envelope(coord.asLatLon());
            List<?> elements = strTree.query(env);
            if (elements.size() > 0) {
                Iterator<?> iter = elements.iterator();
                while (iter.hasNext()) {
                    ArrayList<?> list = (ArrayList<?>) iter.next();
                    if (list.get(1) instanceof String) {
                        return (String) list.get(1);
                    } else {
                        return null;
                    }
                }
            }
        } catch (Exception e) {
            throw new VizException("Error transforming", e);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEInputResource#handleMouseUp(int,
     * int, int)
     */
    @Override
    protected boolean handleMouseUp(int x, int y, int mouseButton) {
        if (mouseButton == 1) {
            Coordinate coord = getResourceContainer().translateClick(x, y);
            double mindist = Double.MAX_VALUE;
            double testdist;
            MPEGageData bestData = null;
            // Find closest gage...
            for (MPEGageData data : dataMap.values()) {
                Coordinate latLon = data.getLatLon();
                float lon = (float) latLon.x;
                float lat = (float) latLon.y;
                testdist = Math.pow((coord.x - lon), 2)
                        + Math.pow((coord.y - lat), 2);
                testdist = Math.pow(testdist, .5);
                if (testdist < mindist) {
                    mindist = testdist;
                    bestData = data;
                }
            }

            // We found a gage:
            if (bestData != null) {
                if (dialog == null) {
                    dialog = new Display7x7Dialog(shell, bestData);
                    dialog.open();
                    dialog.getShell().addDisposeListener(new DisposeListener() {
                        @Override
                        public void widgetDisposed(DisposeEvent e) {
                            dialog = null;
                            EditableManager.makeEditable(MPEGageResource.this,
                                    false);
                            final ICommandService service = (ICommandService) PlatformUI
                                    .getWorkbench().getService(
                                            ICommandService.class);
                            service.refreshElements(
                                    "com.raytheon.viz.mpe.ui.actions.display7x7",
                                    map);
                        }
                    });
                } else {
                    dialog.updateGageData(bestData);
                }
            }
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.IDisplayFieldChangedListener#displayFieldChanged
     * (com.raytheon.viz.mpe.ui.DisplayFieldData,
     * com.raytheon.viz.mpe.ui.DisplayFieldData)
     */
    @Override
    public void displayFieldChanged(DisplayFieldData oldFieldData,
            DisplayFieldData newFieldData) {
        loadColors();
        reloadGages();
        issueRefresh();
    }
    
    
    
}
