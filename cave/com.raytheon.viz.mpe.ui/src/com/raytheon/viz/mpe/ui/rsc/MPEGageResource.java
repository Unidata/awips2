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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;
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
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEGageResource extends AbstractMPEInputResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEGageResource.class);

    private static final String GAGE_TRIANGLES = "GAGETRIANGLES%sz";

    private static final double POINT_RADIUS = 2;

    private final SimpleDateFormat sdf;

    private final Object mutex = new Object();

    private MPEDisplayManager displayMgr;

    private Hashtable<Coordinate, MPEGageData> dataMap = null;

    private STRtree strTree = null;

    private final RGB triangleColor = RGBColors.getRGBColor("YELLOW");

    private DataMappingPreferences dmPref;

    private ColorMap colorMap;

    private Display7x7Dialog dialog;

    private Date lastDate = null;

    private IWireframeShape gageTriangles;

    private MPEFontFactory fontFactory;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public MPEGageResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        displayMgr = MPEDisplayManager.getInstance(descriptor
                .getRenderableDisplay());
        fontFactory = new MPEFontFactory(target, this);
        loadColors();
        lastDate = displayMgr.getCurrentDate();
        addPoints(MPEDataManager.getInstance().readGageData(lastDate));
    }

    @Override
    protected void disposeInternal() {
        if (gageTriangles != null) {
            gageTriangles.dispose();
        }
        fontFactory.dispose();
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
        // set the plot draw or no draw values
        Set<MPEDisplayManager.GageDisplay> gd = displayMgr.getGageDisplay();
        if (gd.isEmpty()) {
            // Nothing to paint
            return;
        }

        Date curDate = displayMgr.getCurrentDate();
        synchronized (mutex) {
            if (curDate != null && curDate.equals(lastDate) == false) {
                lastDate = curDate;
                addPoints(MPEDataManager.getInstance().readGageData(lastDate));
            }
        }

        if (gd.contains(GageDisplay.Ids) || gd.contains(GageDisplay.Values)) {
            paintPlotInfo(target, paintProps, gd.contains(GageDisplay.Ids),
                    gd.contains(GageDisplay.Values));
        }

        try {
            if (gd.contains(GageDisplay.Triangles)) {
                paintTriangles(target, paintProps);
            }
        } catch (Exception e) {
            statusHandler
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
        IFont font = fontFactory.getMPEFont(displayMgr.getFontState());
        font.setSmoothing(false);

        MPEDisplayManager.GageMissingOptions gm = displayMgr.getGageMissing();
        boolean xor = displayMgr.getGageColor() == GageColor.Contrast;

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
                                UnitConverter conv = SI.MILLIMETER
                                        .getConverterTo(NonSI.INCH);
                                gageValue = String.format("%5.2f",
                                        conv.convert(gageData.getGval()));
                            }
                        } else {
                            if (gageData.getId().contains("PSEUDO")
                                    && !isMissing) {
                                UnitConverter conv = SI.MILLIMETER
                                        .getConverterTo(NonSI.INCH);
                                gageValue = String.format("%5.2f",
                                        conv.convert(gageData.getGval()));
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
        if (!displayMgr.getGageDisplay().isEmpty()) {
            MPEDisplayManager.GageColor gc = displayMgr.getGageColor();
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
                    UnitConverter conv = SI.MILLIMETER
                            .getConverterTo(NonSI.INCH);
                    fltVal = (float) conv.convert(gageData.getGval());
                }
                // System.out.println("--- fltVal = " + fltVal);
                gageColor = getColorByValue(fltVal);
                // gageColor = getColorByValue(gageData.getGval());
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

        if (!gages.isEmpty()) {
            for (ListIterator<MPEGageData> it = gages.listIterator(); it
                    .hasNext();) {
                MPEGageData gageData = it.next();
                Coordinate xy = gageData.getLatLon();
                double[] pixel = descriptor.worldToPixel(new double[] { xy.x,
                        xy.y });
                xy = new Coordinate(pixel[0], pixel[1]);
                dataMap.put(xy, gageData);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(xy.x + 10, xy.y + 10);
                Coordinate p2 = new Coordinate(xy.x - 10, xy.y - 10);
                Envelope env = new Envelope(p1, p2);
                ArrayList<Object> data = new ArrayList<Object>();
                data.add(xy);
                data.add("GAGE: " + gageData.getId() + " VALUE: "
                        + gageData.getGval());
                strTree.insert(env, data);
            }
        }
    }

    private RGB getColorByValue(float gval) {
        float value = gval;

        if (value == -999.0) {
            value = -9999.0f;
        }
        int i = 0;
        RGB gcol = null;

        for (DataMappingEntry entry : dmPref.getEntries()) {

            if (value == entry.getDisplayValue().floatValue()) {
                gcol = ColorMapParameters.colorToRGB((colorMap.getColors()
                        .get(i)));
                break;
            } else if (value < entry.getDisplayValue().floatValue()) {
                gcol = ColorMapParameters.colorToRGB((colorMap.getColors()
                        .get(i - 1)));
                break;
            }
            i++;
        }
        if (gcol == null) {
            i = dmPref.getEntries().size();
            gcol = ColorMapParameters.colorToRGB(colorMap.getColors()
                    .get(i - 1));
        }
        return gcol;
    }

    private void loadColors() {
        List<Colorvalue> colorSet = displayMgr.getGageColorMap();
        if (colorSet == null) {
            return;
        }
        colorMap = new ColorMap(colorSet.size());
        colorMap.setName(displayMgr.getDisplayFieldType().getCv_use());
        dmPref = new DataMappingPreferences();
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
            double maxdist = 99999;
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
                if (testdist < maxdist) {
                    maxdist = testdist;
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

}
