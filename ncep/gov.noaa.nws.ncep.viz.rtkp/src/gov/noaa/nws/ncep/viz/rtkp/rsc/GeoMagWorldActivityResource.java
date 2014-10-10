/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.Location;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.LabeledPoint;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader;
import gov.noaa.nws.ncep.viz.rtkp.MagActivityCapability;
import gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpDataBlockWindow;
import gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpRecentKpWindow;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil.GeoMagStationType;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.awt.Color;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides a resource for displaying RTKP world wide mag activity map with
 * recent k values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * May 5, 2014   1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagWorldActivityResource extends
        AbstractVizResource<GeoMagWorldActivityResourceData, MapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagWorldActivityResource.class);

    private SimpleDateFormat topLabelSdf = new SimpleDateFormat(
            "dd-MMM-yy HH:mm:ss 'UTC'");

    private static GeoMagWorldActivityResourceData geoMagWorldActResourceData;

    private static GeoMagWorldActivityResource geoMagWorldActResource = null;

    protected GeoMagWorldActivityTitleDateResource topLabelResource = null;

    protected ResourcePair topLabelRscPair = null;

    private List<Symbol> symbolList = null;

    private SymbolLocationSet otherStnSymbolSet = null;

    private List<LabeledPoint> latestKIndexPoints;

    private List<DrawableString> labelStrings = null;

    private static NatlCntrsEditor mapEditor = null;

    private IFont font = null;

    private List<Map<String, Object>> latestStnKLst = null;

    private List<GeoMagStation> points = new ArrayList<GeoMagStation>();

    private List<GeoMagStation> otherStnPt = new ArrayList<GeoMagStation>();

    private GeoMagRTKpDataBlockWindow dataBlock = null;

    private final GeoMagSampling samplingRsc;

    protected ReferencedCoordinate sampleCoord;

    protected String sampleString;

    private static GeoMagWorldActivityMouseHandler mouseHandler;

    public static NatlCntrsEditor getMapEditor() {
        return mapEditor;
    }

    public List<GeoMagStation> getOtherStnPt() {
        return otherStnPt;
    }

    public void setOtherStnPt(List<GeoMagStation> otherStnPt) {
        if (otherStnPt == null)
            this.otherStnPt.clear();
        else
            this.otherStnPt = otherStnPt;
    }

    public List<GeoMagStation> getPoints() {
        return points;
    }

    public void setPoints(List<GeoMagStation> points) {
        if (points == null)
            this.points.clear();
        else
            this.points = points;
    }

    public List<LabeledPoint> getLatestKIndexPoints() {
        return latestKIndexPoints;
    }

    public void setLatestKIndexPoints(List<LabeledPoint> latestKIndexPoints) {

        this.latestKIndexPoints = latestKIndexPoints;
    }

    private static void createMapEditor() {
        // create an editor MapEditor
        if (mapEditor != null)
            return;

        try {

            // TODO: what if the active editor is not a Map Editor ?
            // should we find one, create one or prompt
            //
            AbstractEditor ed = NcDisplayMngr.getActiveNatlCntrsEditor();

            if (NcEditorUtil.getNcDisplayType(ed) == NcDisplayType.NMAP_DISPLAY) {
                mapEditor = (NatlCntrsEditor) ed;
            } else {
                mapEditor = (NatlCntrsEditor) NcDisplayMngr
                        .createNatlCntrsEditor(NcDisplayType.NMAP_DISPLAY,
                                "Worldwide Mag Activity Map");

                // get this to set the editor to 'Worldwide Mag Activity Map'
                ResourceBndlLoader rbdLoader = new ResourceBndlLoader(
                        "Worldwide Mag Activity Map");
                rbdLoader.addDefaultRBD(NcDisplayType.NMAP_RTKP_WORLD_DISPLAY,
                        mapEditor);
                VizApp.runSync(rbdLoader);
            }

        } catch (Exception ve) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Could not load initial editor: "
                            + ve.getLocalizedMessage(), ve);
        }
    }

    public static GeoMagWorldActivityResource getGeoMagWorldActivityResource() {
        if (geoMagWorldActResource == null) {
            if (mapEditor == null)
                createMapEditor();
            if (mapEditor != null) {
                IMapDescriptor desc = (IMapDescriptor) mapEditor
                        .getActiveDisplayPane().getRenderableDisplay()
                        .getDescriptor();
                try {
                    if (geoMagWorldActResourceData == null)
                        geoMagWorldActResourceData = new GeoMagWorldActivityResourceData();
                    geoMagWorldActResource = geoMagWorldActResourceData
                            .construct(new LoadProperties(), desc);
                    desc.getResourceList().add(geoMagWorldActResource);
                    geoMagWorldActResource.init(mapEditor
                            .getActiveDisplayPane().getTarget());

                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return geoMagWorldActResource;
    }

    /**
     * Default constructor
     */
    protected GeoMagWorldActivityResource(
            GeoMagWorldActivityResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(EditableCapability.class).setEditable(true);
        samplingRsc = new GeoMagSampling();
        topLabelSdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        getCapabilities().addCapability(MagActivityCapability.class);
    }

    /**
     * Called when resource is disposed
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    public void disposeInternal() {
        if (mapEditor != null) {
            mapEditor.unregisterMouseHandler(mouseHandler);
            mouseHandler = null;
            mapEditor = null;
        }
        if (font != null) {
            font.dispose();
            font = null;
        }
        if (samplingRsc != null) {
            samplingRsc.dispose();
        }

        getDescriptor().getResourceList().remove(topLabelRscPair);

        closeAssociatedViews();
        geoMagWorldActResource = null;
        geoMagWorldActResourceData = null;

        if (topLabelResource != null) {
            topLabelResource.dispose();
        }
        super.dispose();
        this.dispose();
    }

    public void registerMouseHandler() {
        mouseHandler = getMouseHandler();
        if (mapEditor != null && mouseHandler != null)
            mapEditor.registerMouseHandler((IInputHandler) mouseHandler);
    }

    public void unregisterMouseHandler() {
        mouseHandler = getMouseHandler();
        if (mapEditor != null && mouseHandler != null)
            mapEditor.unregisterMouseHandler((IInputHandler) mouseHandler);
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {
        if (updatedProps.isVisible()) {
            reopenAssociatedViews();
        } else {
            hideAssociatedViews();
        }
    }

    private void hideAssociatedViews() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(RTKpUtil.DATABLOCK_VIEW_ID);
        if (wpage.isPartVisible(vpart)) {
            GeoMagRTKpDataBlockWindow paletteWin = GeoMagRTKpDataBlockWindow
                    .getAccess();
            paletteWin.setEditorVisible(false);
            wpage.hideView(vpart);
        }
        vpart = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
        if (wpage.isPartVisible(vpart)) {
            GeoMagRTKpRecentKpWindow paletteWin = GeoMagRTKpRecentKpWindow
                    .getAccess();
            paletteWin.setEditorVisible(false);
            wpage.hideView(vpart);
        }
    }

    private void closeAssociatedViews() {

        IWorkbenchWindow win = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        if (win == null)
            return;
        IWorkbenchPage wpage = win.getActivePage();
        if (wpage != null) {
            IViewPart vpart1 = wpage.findView(RTKpUtil.DATABLOCK_VIEW_ID);
            wpage.hideView(vpart1);

            IViewPart vpart2 = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
            wpage.hideView(vpart2);
        }

        NcDisplayMngr.setPanningMode();
    }

    private void reopenAssociatedViews() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(RTKpUtil.DATABLOCK_VIEW_ID);
        if (!wpage.isPartVisible(vpart)) {
            GeoMagRTKpDataBlockWindow paletteWin = GeoMagRTKpDataBlockWindow
                    .getAccess();
            paletteWin.setEditorVisible(true);
            try {
                vpart = wpage.showView(RTKpUtil.DATABLOCK_VIEW_ID);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        vpart = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
        if (!wpage.isPartVisible(vpart)) {
            GeoMagRTKpRecentKpWindow paletteWin = GeoMagRTKpRecentKpWindow
                    .getAccess();
            paletteWin.setEditorVisible(true);
            try {
                vpart = wpage.showView(RTKpUtil.DATABLOCK_VIEW_ID);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#getCoordinateReferenceSystem()
     */
    public CoordinateReferenceSystem getCoordinateReferenceSystem() {

        if (descriptor == null)
            return null;

        return descriptor.getCRS();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {

        return "Worldwide Mag Activity Map";

    }

    public void init(GeoMagRTKpDataBlockWindow dataBlock) {

        this.dataBlock = dataBlock;

        if (topLabelRscPair == null && topLabelResource == null) {
            topLabelRscPair = ResourcePair
                    .constructSystemResourcePair(new GeoMagWorldActivityTitleDateResourceData());

            getDescriptor().getResourceList().add(topLabelRscPair);
            getDescriptor().getResourceList().instantiateResources(
                    getDescriptor(), true);

            topLabelResource = (GeoMagWorldActivityTitleDateResource) topLabelRscPair
                    .getResource();
        }

        // register mouse handler
        registerMouseHandler();

        if (dataBlock.isShowKpOnly()
                || dataBlock.getkType().equals(RTKpUtil.KS_PLOT)) {
            points = RTKpUtil.getGeoMagStations(GeoMagStationType.KP);
            otherStnPt = RTKpUtil.getGeoMagStations(GeoMagStationType.NON_KP);
        } else {
            points = RTKpUtil.getGeoMagStations(GeoMagStationType.K);
            otherStnPt = RTKpUtil.getGeoMagStations(GeoMagStationType.NON_K);
        }

        List<String> stnCodes = RTKpUtil
                .getGeoMagStationCodes(GeoMagStationType.ALL);
        try {
            latestStnKLst = RTKpUtil.getLatestEstKIndex(stnCodes, null, null);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    " Error while retrieving latest est k index. Exception: "
                            + e.getLocalizedMessage(), e);
        }

        try {
            latestKIndexPoints = createKIndexLabelPointData(points);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    public void initInternal(IGraphicsTarget target) throws VizException {
        // EditableManager.makeEditable(this,
        // getCapability(EditableCapability.class).isEditable());

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.
     * core.PixelExtent)
     */
    public boolean isApplicable(PixelExtent extent) {

        return true;

    }

    private void generateSymbolForDrawing() {
        String type;
        float lineWidth = resourceData.getMarkerWidth();

        Boolean clear = false;

        String category = new String("Marker");
        double sizeScale = getCapabilities().getCapability(resourceData,
                MagActivityCapability.class).getMarkerSize();

        if (RTKpUtil.KS_PLOT.equals(dataBlock.getkType())) {
            sizeScale += 0.7;
        }

        if (points.isEmpty() == true) {
            symbolList = null;
        } else {

            symbolList = new ArrayList<Symbol>(points.size());

            Coordinate[] locations = new Coordinate[points.size()];
            Color[] colors = new Color[points.size()];

            HashMap<String, Map<String, Object>> latestStnsKMap = new HashMap<String, Map<String, Object>>();
            for (int i = 0; i < latestStnKLst.size(); i++) {
                latestStnsKMap.put(
                        (String) latestStnKLst.get(i).get("stationcode"),
                        latestStnKLst.get(i));
            }

            int i = 0;

            for (GeoMagStation p : points) {
                Location loc = p.getLocation();
                double lon, lat;
                lon = loc.getLongitude();
                lat = loc.getLatitude();

                Map<String, Object> stnKMap = latestStnsKMap.get(p
                        .getStationCode());
                if (stnKMap != null) {
                    Integer k = (Integer) stnKMap.get("kestindex");
                    Float ks = (Float) stnKMap.get("ks");

                    if (RTKpUtil.KS_PLOT.equals(dataBlock.getkType())) {
                        k = ks.intValue();
                    }
                    colors[i] = getCapabilities().getCapability(resourceData,
                            MagActivityCapability.class).getNOAAScaleColors()[k];

                } else {
                    colors[i] = getCapabilities().getCapability(resourceData,
                            MagActivityCapability.class).getNOAAScaleColors()[0];
                }

                locations[i] = new Coordinate(lon, lat);

                symbolList.add(new Symbol(null, new Color[] { colors[i] },
                        lineWidth, sizeScale, clear, locations[i], category,
                        "FILLED_CIRCLE"));
                i++;

            }

        }
        if (otherStnPt.isEmpty() == true) {
            otherStnSymbolSet = null;
        } else {
            // SymbolLocationSet constructor requires a positive-length array of
            // Coordinate
            Coordinate[] locations = new Coordinate[otherStnPt.size()];

            int i = 0;
            for (GeoMagStation p : otherStnPt) {
                Location loc = p.getLocation();
                double lon, lat;
                lon = loc.getLongitude();
                lat = loc.getLatitude();
                locations[i++] = new Coordinate(lon, lat);
            }

            RGB nonNwMarkerColor = getCapabilities().getCapability(
                    resourceData, MagActivityCapability.class)
                    .getNonNetwrkStnColor();
            Color[] colors = new Color[] { new Color(nonNwMarkerColor.red,
                    nonNwMarkerColor.green, nonNwMarkerColor.blue) };
            type = resourceData.getOtherStnMarkerType().toString();

            otherStnSymbolSet = new SymbolLocationSet(null, colors, lineWidth,
                    sizeScale + 0.5, clear, locations, category, type);

        }

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
        // if the Kindices colors are changed, redraw the text in the
        // "Recent Kp Estimates" block
        if (!Arrays.equals(
                this.dataBlock.getKindicesColors(),
                getCapabilities().getCapability(resourceData,
                        MagActivityCapability.class).getNOAAScaleRGBColors())) {
            this.dataBlock.setKindicesColors(getCapabilities().getCapability(
                    resourceData, MagActivityCapability.class)
                    .getNOAAScaleRGBColors());
            this.dataBlock.setKp_last10_colors();
            RTKpUtil.showRecentKpEstBlock(this.dataBlock);
        }

        if (font == null
                || resourceData.getMarkerTextSize() != getCapabilities()
                        .getCapability(resourceData,
                                MagActivityCapability.class)
                        .getMarkerTextSize()) {

            font = target.initializeFont(
                    "Monospace",
                    (float) (12 * getCapabilities()
                            .getCapability(resourceData,
                                    MagActivityCapability.class)
                            .getMarkerTextSize().getSoftwareSize()), null);

            font.setSmoothing(false);
            font.setScaleFont(false);
            resourceData.setMarkerTextSize(getCapabilities().getCapability(
                    resourceData, MagActivityCapability.class)
                    .getMarkerTextSize());
        }

        // paintTopLabels(target, paintProps);

        generateSymbolForDrawing();

        if (symbolList != null) {

            DisplayElementFactory df = new DisplayElementFactory(target,
                    this.descriptor);

            for (Symbol symbol : symbolList) {
                ArrayList<IDisplayable> elements = df.createDisplayElements(
                        symbol, paintProps);
                for (IDisplayable each : elements) {
                    try {
                        each.draw(target, paintProps);
                        each.dispose();
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        if (otherStnSymbolSet != null) {

            DisplayElementFactory df = new DisplayElementFactory(target,
                    this.descriptor);
            ArrayList<IDisplayable> elements = df.createDisplayElements(
                    otherStnSymbolSet, paintProps);
            for (IDisplayable each : elements) {
                try {
                    each.draw(target, paintProps);
                    each.dispose();
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

        double offsetX = 0.0;// charWidth / 2.0 / screenToWorldRatio;
        double offsetY = 0.0;// charHeight / screenToWorldRatio;

        labelStrings = new ArrayList<DrawableString>();

        if (latestKIndexPoints != null) {

            for (LabeledPoint lp : latestKIndexPoints) {
                double[] latlon = new double[] { lp.getLongitude(),
                        lp.getLatitude() };
                double[] pix = this.descriptor.worldToPixel(latlon);
                if (pix == null) {
                    continue;
                }
                if (paintProps.getView().isVisible(pix)) {

                    String lpStr = lp.getName();
                    double k_val = Double.parseDouble(lpStr);

                    int k_val_int = (int) k_val;
                    if (k_val_int > 9) {
                        k_val_int = 0;
                    }

                    RGB color = getCapabilities().getCapability(resourceData,
                            MagActivityCapability.class)
                            .getkIndicesTextColors()[k_val_int];

                    DrawableString drawStr = new DrawableString(lp.getName(),
                            color);
                    drawStr.font = font;
                    drawStr.setCoordinates(pix[0] + offsetX, pix[1] + offsetY);
                    drawStr.horizontalAlignment = HorizontalAlignment.CENTER;
                    drawStr.verticallAlignment = VerticalAlignment.MIDDLE;
                    drawStr.textStyle = TextStyle.NORMAL;
                    labelStrings.add(drawStr);
                }
            }

            // }

            if (labelStrings != null) {
                target.drawStrings(labelStrings);
            }

        }
        if (sampleCoord != null) {

            samplingRsc
                    .paintResult(target, descriptor, paintProps, sampleCoord);
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

        if (coord == null)
            return "No Data";
        if (sampleString == null)
            return "";

        StringBuilder sb = new StringBuilder();

        sb.append(sampleString);

        for (int i = 0; i < latestStnKLst.size(); i++) {

            Map<String, Object> map = latestStnKLst.get(i);

            String stnCode = (String) map.get("stationcode");

            if (stnCode.equals(sampleString)) {
                Date reftime = (Date) map.get("reftime");

                sb.append(" (Latest Data " + reftime + ")");
            }
        }

        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#isProjectable
     * (org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    public boolean isProjectable(CoordinateReferenceSystem mapData) {

        return true;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

    }

    private GeoMagWorldActivityMouseHandler getMouseHandler() {

        if (mouseHandler == null) {
            mouseHandler = new GeoMagWorldActivityMouseHandler(this);
        }

        return mouseHandler;
    }

    /**
     * Check if the resource is currently editable
     * 
     * @return editable
     */
    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    public void setEditable(boolean enable) {
        getCapability(EditableCapability.class).setEditable(enable);
        EditableManager.makeEditable(this,
                getCapability(EditableCapability.class).isEditable());
    }

    public GeoMagRTKpDataBlockWindow getDataBlock() {
        return dataBlock;
    }

    public void setDataBlock(GeoMagRTKpDataBlockWindow dataBlock) {
        this.dataBlock = dataBlock;
    }

    public List<LabeledPoint> createKIndexLabelPointData(
            List<GeoMagStation> stns) {

        List<LabeledPoint> stnPoints = new ArrayList<LabeledPoint>();
        HashMap<String, GeoMagStation> kStnMap = new HashMap<String, GeoMagStation>();
        for (int i = 0; i < points.size(); i++) {
            kStnMap.put(points.get(i).getStationCode(), points.get(i));
        }

        for (int i = 0; i < latestStnKLst.size(); i++) {
            Map<String, Object> map = latestStnKLst.get(i);
            GeoMagStation stn = kStnMap.get(map.get("stationcode"));

            if (stn != null) {
                Integer k = (Integer) map.get("kestindex");
                Float ks = (Float) map.get("ks");
                Date reftime = (Date) map.get("reftime");

                String displayStr = k.toString();
                if (RTKpUtil.KS_PLOT.equals(dataBlock.getkType())) {
                    displayStr = String.format("%.1f", ks);
                }
                LabeledPoint lp = new LabeledPoint(displayStr, stn
                        .getLocation().getLatitude(), stn.getLocation()
                        .getLongitude());
                lp.addLabel("k_est", "k");
                stnPoints.add(lp);
            }
        }

        return stnPoints;
    }

}
