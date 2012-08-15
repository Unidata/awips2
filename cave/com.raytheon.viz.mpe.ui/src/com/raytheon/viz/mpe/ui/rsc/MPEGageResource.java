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
import com.raytheon.uf.viz.core.IGraphicsTarget;
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
import com.raytheon.viz.mpe.ui.MPEDisplayManager.GageDisplay;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.GageMissingOptions;
import com.raytheon.viz.mpe.ui.MPEFontManager;
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

    private static final int IMAGE_WIDTH = 10;

    private static final int IMAGE_HEIGHT = 10;

    private final SimpleDateFormat sdf;

    private final Object mutex = new Object();

    private MPEDisplayManager displayMgr;

    private Hashtable<Coordinate, MPEGageData> dataMap = null;

    private STRtree strTree = null;

    private final RGB triangleColor = RGBColors.getRGBColor("YELLOW");

    private double scaleWidthValue = 0.0;

    private double scaleHeightValue = 0.0;

    private MPEGageData gageData = null;

    private DataMappingPreferences dmPref;

    private ColorMap colorMap;

    private Display7x7Dialog dialog;

    private Date lastDate = null;

    private DrawableCircle point;

    private DrawableString string;

    private IWireframeShape gageTriangles;

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
        point = new DrawableCircle();
        point.filled = true;
        string = new DrawableString("", null);
        displayMgr = MPEDisplayManager.getInstance(descriptor
                .getRenderableDisplay());
        loadColors();
        lastDate = displayMgr.getCurrentDate();
        addPoints(MPEDataManager.getInstance().readGageData(lastDate, lastDate));
    }

    @Override
    protected void disposeInternal() {
        if (gageTriangles != null) {
            gageTriangles.dispose();
        }
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
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        setScaleWidth(paintProps);
        setScaleHeight(paintProps);

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
                addPoints(MPEDataManager.getInstance().readGageData(lastDate, lastDate));
            }
        }

        // Fonts are shared and cached, no need to init or dispose
        IFont font = MPEFontManager.getFont(this, displayMgr.getFontState(),
                target);
        font.setSmoothing(false);
        string.font = font;

        if (gd.contains(GageDisplay.Ids) || gd.contains(GageDisplay.Values)) {
            Iterator<Coordinate> iter = dataMap.keySet().iterator();
            while (iter.hasNext()) {
                try {
                    Coordinate c = iter.next();
                    double[] pixel = descriptor.worldToPixel(new double[] {
                            c.x, c.y });
                    if (paintProps.getView().getExtent().contains(pixel)) {
                        paintPlotInfo(target, paintProps, c, dataMap.get(c),
                                gd.contains(GageDisplay.Ids),
                                gd.contains(GageDisplay.Values));
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error painting gages: " + e.getLocalizedMessage(),
                            e);
                }
            }
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
     * @param c
     * @param gageData
     * @throws VizException
     */
    private void paintPlotInfo(IGraphicsTarget target,
            PaintProperties paintProps, Coordinate c, MPEGageData gageData,
            boolean isGageIdsDisplayed, boolean isGageValuesDisplayed)
            throws VizException {
        RGB gageColor = new RGB(255, 255, 255);
        double[] centerpixels = descriptor
                .worldToPixel(new double[] { c.x, c.y });

        MPEDisplayManager.GageMissingOptions gm = displayMgr.getGageMissing();

        boolean isReportedMissing = gageData.isReported_missing();
        boolean isMissing = ((gageData.getGval() == -999.f || gageData
                .getGval() == -9999.f) ? true : false);
        if (gageData.isManedit() == true
                && gageData.getEdit().equalsIgnoreCase("m")) {
            isMissing = true;
        }

        String val = null;

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

            if (gm.equals(GageMissingOptions.MissingNone)) {
                // System.out.println(gageData.getGval()); // TODO : REMOVE
                gageColor = setGageColor(gageData);
            } else {
                gageColor = RGBColors.getRGBColor("SandyBrown");
            }

            if (isGageValuesDisplayed) {
                Coordinate stageCoor = new Coordinate(centerpixels[0]
                        + scaleWidthValue / 3, centerpixels[1]
                        + scaleHeightValue);
                if (!isMissing) {
                    val = String.format("%5.2f", gageData.getGval());
                } else {
                    val = "m";
                }

                string.setCoordinates(stageCoor.x, stageCoor.y);

                // draw the value
                if (!gageData.isManedit()) {
                    if (gageData.getId().contains("PSEUDO")) {
                        UnitConverter conv = SI.MILLIMETER
                                .getConverterTo(NonSI.INCH);
                        val = String.format("%5.2f",
                                conv.convert(gageData.getGval()));
                    }
                    string.setText(val, gageColor);
                    target.drawStrings(string);
                } else {
                    if (gageData.getId().contains("PSEUDO") && !isMissing) {
                        UnitConverter conv = SI.MILLIMETER
                                .getConverterTo(NonSI.INCH);
                        val = String.format("%5.2f",
                                conv.convert(gageData.getGval()));
                    }
                    string.setText(val + "e", gageColor);
                    target.drawStrings(string);
                }
            }

            // draw the ID
            if (isGageIdsDisplayed) {
                Coordinate idCoor = new Coordinate(centerpixels[0]
                        + scaleWidthValue / 3, centerpixels[1]
                        - scaleHeightValue);

                // System.out.println(gageData.getId()); // TODO : remove

                string.setText(gageData.getId(), gageColor);
                string.setCoordinates(idCoor.x, idCoor.y);

                target.drawStrings(string);
                double[] coords = descriptor.worldToPixel(new double[] { c.x,
                        c.y });
                point.setCoordinates(coords[0], coords[1]);
                point.radius = scaleWidthValue / 3;
                if (point.basics.color == null) {
                    setGageColor(gageData);
                }
                target.drawCircle(point);
            }
        }
    }

    private RGB setGageColor(MPEGageData gageData) throws VizException {
        RGB gageColor = new RGB(255, 255, 255);
        if (!displayMgr.getGageDisplay().isEmpty()) {
            MPEDisplayManager.GageColor gc = displayMgr.getGageColor();
            string.basics.xOrColors = false;
            point.basics.xOrColors = false;
            switch (gc) {

            case Solid:
                gageColor = RGBColors.getRGBColor("SandyBrown");
                break;

            case Contrast:

                // RGB xoc = RGBColors.getRGBColor("SandyBrown");
                // gageColor = getXOR(bg, xoc);
                gageColor = RGBColors.getRGBColor("SandyBrown");
                string.basics.xOrColors = true;
                point.basics.xOrColors = true;
                // if (bg.equals(xmcolor)) {
                // gageColor = getContrast(bg);
                // } else {
                // gageColor = getContrast(xmcolor);
                // }
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

        point.basics.color = gageColor;
        return gageColor;
    }

    /**
     * Process the gage data for the resource
     * 
     * @param gages
     */
    private void addPoints(List<MPEGageData> gages) {
        gageData = new MPEDataManager.MPEGageData();
        dataMap = new Hashtable<Coordinate, MPEGageData>();
        strTree = new STRtree();

        if (!gages.isEmpty()) {
            for (ListIterator<MPEGageData> it = gages.listIterator(); it
                    .hasNext();) {
                gageData = it.next();
                Coordinate xy = gageData.getLatLon();
                dataMap.put(xy, gageData);

                /* Create a small envelope around the point */
                Coordinate p1 = new Coordinate(xy.x + .05, xy.y + .05);
                Coordinate p2 = new Coordinate(xy.x - .05, xy.y - .05);
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
     * Set the width scalar
     * 
     * @param props
     */
    private void setScaleWidth(PaintProperties props) {
        double screenToWorldWidthRatio = props.getCanvasBounds().width
                / props.getView().getExtent().getWidth();
        scaleWidthValue = (IMAGE_WIDTH / 2.0) / screenToWorldWidthRatio;
    }

    /**
     * Set the height scalar
     * 
     * @param props
     */
    private void setScaleHeight(PaintProperties props) {
        double screenToWorldHeightRatio = props.getCanvasBounds().height
                / props.getView().getExtent().getHeight();
        scaleHeightValue = (IMAGE_HEIGHT / 2.0) / screenToWorldHeightRatio;
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
            for (Coordinate ll : dataMap.keySet()) {
                MPEGageData data = dataMap.get(ll);
                float lon = (float) ll.x;
                float lat = (float) ll.y;
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
