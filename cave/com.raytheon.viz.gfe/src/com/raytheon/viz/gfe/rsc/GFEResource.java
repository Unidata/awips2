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

package com.raytheon.viz.gfe.rsc;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.contours.rsc.displays.GriddedVectorDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.actions.ChangeCombineMode;
import com.raytheon.viz.gfe.actions.VectorEditModeAction;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.internal.OffscreenSpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowISCGridsMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.EditorType;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisualizationType;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.dialogs.FuzzValueDialog;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.gfe.ui.GfeUiUtil;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Resource for displaying renderables for a particular Parm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/01/2008              chammack    Initial Creation.
 * Aug 20, 2008            dglazesk    Update for the ColorMap interface change
 * Nov 23, 2011            mli         set vector lineStyle
 * May 11, 2012            njensen    Allow rsc to be recycled
 * Nov 08, 2012 1298       rferrel     Changes for non-blocking FuzzValueDialog.
 * 
 * </pre>
 * 
 * 
 * 
 * @author chammack
 * @version 1.0
 */
public class GFEResource extends
        AbstractVizResource<GFEResourceData, MapDescriptor> implements
        IResourceDataChanged, IContextMenuContributor, IMessageClient {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEResource.class);

    /** maximum label size in pixels */
    private final int maxLabelLength = 100;

    /** maximum label height in pixels */
    @SuppressWarnings("unused")
    private final int maxLabelHeight = 40;

    /** distance between labels */
    private final int pixelDistance = 125;

    private final Set<VisualizationType> OUTLINE_TYPES = EnumSet.of(
            VisualizationType.CONTOUR, VisualizationType.WIND_ARROW,
            VisualizationType.WIND_BARB, VisualizationType.BOUNDED_AREA);

    private final Set<VisualizationType> DENSITY_TYPES = EnumSet.of(
            VisualizationType.CONTOUR, VisualizationType.WIND_ARROW,
            VisualizationType.WIND_BARB);

    private final Set<VisualizationType> MAG_TYPES = EnumSet.of(
            VisualizationType.CONTOUR, VisualizationType.WIND_ARROW,
            VisualizationType.WIND_BARB, VisualizationType.BOUNDED_AREA);

    protected Parm parm;

    protected DataTime lastDisplayedTime;

    protected VisMode lastVisMode;

    protected boolean lastIscMode;

    protected DataTime curTime;

    protected List<GriddedVectorDisplay> vectorDisplay;

    protected GriddedImageDisplay gridDisplay;

    protected GriddedContourDisplay contourDisplay;

    protected GridGeometry2D gridGeometry;

    protected PixelCoverage pixelCoverage;

    protected IGraphicsTarget lastGraphicsTarget;

    protected DataManager dataManager;

    protected Map<Object, IWireframeShape> outlineShapes = new HashMap<Object, IWireframeShape>();

    protected Map<Object, Collection<IShadedShape>> shadedShapes = new HashMap<Object, Collection<IShadedShape>>();

    private IFont gfeFont = null;

    protected IGridDataChangedListener gridChanged = new IGridDataChangedListener() {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener#gridDataChanged
         * (com.raytheon.edex.plugin.gfe.db.objects.ParmID,
         * com.raytheon.uf.common.time.TimeRange)
         */
        @Override
        public void gridDataChanged(ParmID incomingParm, TimeRange validTime) {
            resetFrame(validTime);
        }
    };

    protected IParmInventoryChangedListener parmInventoryChanged = new IParmInventoryChangedListener() {

        /*
         * (non-Javadoc)
         * 
         * @seecom.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener#
         * parmInventoryChanged(com.raytheon.viz.gfe.core.parm.Parm,
         * com.raytheon.uf.common.time.TimeRange)
         */
        @Override
        public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
            resetFrame(timeRange);
            TimeMatchingJob.scheduleTimeMatch(getDescriptor());
        }

    };

    protected IParmIDChangedListener parmIdChanged = new IParmIDChangedListener() {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener#parmIDChanged
         * (com.raytheon.viz.gfe.core.parm.Parm,
         * com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
         */
        @Override
        public void parmIDChanged(Parm parm, ParmID newParmID) {
            resetFrame(TimeRange.allTimes());
        }

    };

    /**
     * Construct a resource that is capable of displaying a particular parm
     * 
     * @param parm
     *            the parm
     * @param dataManager
     *            the datamanager responsible for it
     */
    public GFEResource(Parm parm, DataManager dataManager) {
        super(new GFEResourceData(), new LoadProperties());
        this.resourceData.addChangeListener(this);
        this.parm = parm;
        this.dataManager = dataManager;

        this.vectorDisplay = new ArrayList<GriddedVectorDisplay>();

        // Construct reasonable initial colormap parameters
        // from the parm data
        ColorMapParameters colorMapParameters = DiscreteDisplayUtil
                .buildColorMapParameters(parm);

        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);

        int lineWidth = parm.getDisplayAttributes().getLineWidth();
        LineStyle style = parm.getDisplayAttributes().getLineStyle();
        getCapability(OutlineCapability.class).setLineStyle(style);
        getCapability(OutlineCapability.class).setOutlineWidth(lineWidth);

        GridParmInfo info = this.parm.getGridInfo();
        this.gridGeometry = MapUtil.getGridGeometry(info.getGridLoc());

        lastIscMode = dataManager.getParmManager().iscMode();

        updateRightClickMenu();
    }

    public void reset() {
        if (this.curTime != null) {
            resetFrame(new TimeRange(curTime.getRefTime(), 1));
        }

        updateRightClickMenu();
    }

    /**
     * This method should be called when a frame needs to be reset (data
     * changed, data appeared/disappeared)
     */
    private void resetFrame(TimeRange timeRange) {
        if (curTime != null && timeRange.overlaps(curTime.getValidPeriod())) {
            lastDisplayedTime = null;
            issueRefresh();
        }

    }

    /**
     * Gets the parm associated with the GFE Resource.
     * 
     * @return Returns the parm associated with the GFE Resource
     */
    public Parm getParm() {
        Parm retVal = null;
        if (this.getStatus() != ResourceStatus.DISPOSED) {
            retVal = this.parm;
        }
        return retVal;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void disposeInternal() {
        if (gfeFont != null) {
            gfeFont.dispose();
        }

        if (outlineShapes != null) {
            for (IWireframeShape shape : outlineShapes.values()) {
                shape.dispose();
            }
            outlineShapes.clear();
        }

        if (shadedShapes != null) {
            for (Collection<IShadedShape> shadedShapeCol : shadedShapes
                    .values()) {
                for (IShadedShape shadedShape : shadedShapeCol) {
                    shadedShape.dispose();
                }
            }
            shadedShapes.clear();
        }

        parm.getListeners().removeGridChangedListener(gridChanged);
        parm.getListeners().removeParmInventoryChangedListener(
                parmInventoryChanged);
        parm.getListeners().removeParmIDChangedListener(parmIdChanged);
        Message.unregisterInterest(this, ShowISCGridsMsg.class);

        if (this.gridDisplay != null) {
            this.gridDisplay.dispose();
            this.gridDisplay = null;
        }

        if (this.contourDisplay != null) {
            this.contourDisplay.dispose();
            this.contourDisplay = null;
        }

        clearVectorDisplays();
        lastDisplayedTime = null;
    }

    private void clearVectorDisplays() {
        for (GriddedVectorDisplay display : vectorDisplay) {
            if (display != null) {
                display.dispose();
            }
        }
        vectorDisplay.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        return "Parm Resource " + this.parm.getParmID().toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        parm.getListeners().addGridChangedListener(this.gridChanged);
        parm.getListeners().addParmInventoryChangedListener(
                this.parmInventoryChanged);
        parm.getListeners().addParmIDChangedListener(this.parmIdChanged);

        Message.registerInterest(this, ShowISCGridsMsg.class);

        // Get the font configured for this parm type
        String fontPrefName = "";
        GridType gridType = parm.getGridInfo().getGridType();
        if (GridType.SCALAR.equals(gridType)
                || GridType.VECTOR.equals(gridType)) {
            fontPrefName = "Contour_font";
        } else {
            fontPrefName = "BoundedArea_font";
        }

        gfeFont = GFEFonts.makeGFEIFont(target, fontPrefName, 2);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        GFEPaintProperties myPaintProps = (GFEPaintProperties) paintProps;
        this.curTime = myPaintProps.getDataTime();

        if (curTime == null) {
            return;
        }

        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();

        // translate GFE style mag/density to D2D style
        // -3 to 3 -> 0.5 to 2
        double magnification = Math.pow(1.26, parm.getDisplayAttributes()
                .getFontOffset());
        gfeFont.setMagnification((float) magnification);

        // hack to get around target.getDefaultFont()-centeredness in
        // contourDisplay.
        double contourMagnification = magnification * gfeFont.getFontSize()
                / target.getDefaultFont().getFontSize();

        double density = parm.getDisplayAttributes().getDensity();
        density = Math.pow(1.26, density);

        this.lastGraphicsTarget = target;

        // No data to be displayed here
        if (this.curTime == null) {
            return;
        }

        IGridData[] gd = this.parm.getGridInventory(this.curTime
                .getValidPeriod());

        boolean iscParm = this.parm.isIscParm();
        if (gd.length == 0 && !dataManager.getParmManager().iscMode()) {
            return;
        }

        VisMode visMode = myPaintProps.getVisMode();
        Set<VisualizationType> visTypes = parm.getDisplayAttributes()
                .getVisualizationType(EditorType.SPATIAL, visMode);

        if (!this.curTime.equals(this.lastDisplayedTime)
                || !visMode.equals(this.lastVisMode)
                || this.lastIscMode != dataManager.getParmManager().iscMode()) {

            this.lastDisplayedTime = this.curTime;
            this.lastVisMode = visMode;
            this.lastIscMode = dataManager.getParmManager().iscMode();

            int renderingOrder = RenderingOrderFactory
                    .getRenderingOrder("CONTOUR").value;
            if (lastVisMode.equals(VisMode.IMAGE)) {
                renderingOrder = RenderingOrderFactory
                        .getRenderingOrder("IMAGE_LOCAL").value;
            }
            descriptor.getResourceList().getProperties(this)
                    .setRenderingOrder(renderingOrder);
            descriptor.getResourceList().sort();

            if (gd != null) {
                IGridSlice gs = null;
                if (dataManager.getParmManager().iscMode() && gd.length == 0) {
                    GridParmInfo gpi = this.parm.getGridInfo();
                    GridType gridType = gpi.getGridType();

                    IGrid2D dummyGrid = null;
                    switch (gridType) {
                    case SCALAR:
                        dummyGrid = new Grid2DFloat(gpi.getGridLoc().getNx(),
                                gpi.getGridLoc().getNy(), Float.NaN);
                        gs = new ScalarGridSlice(this.curTime.getValidPeriod(),
                                gpi, new GridDataHistory[] {},
                                (Grid2DFloat) dummyGrid);
                        break;
                    case VECTOR:
                        dummyGrid = new Grid2DFloat(gpi.getGridLoc().getNx(),
                                gpi.getGridLoc().getNy(), Float.NaN);
                        gs = new VectorGridSlice(this.curTime.getValidPeriod(),
                                gpi, new GridDataHistory[] {},
                                (Grid2DFloat) dummyGrid,
                                (Grid2DFloat) dummyGrid);
                        break;
                    case WEATHER:
                        dummyGrid = new Grid2DByte(gpi.getGridLoc().getNx(),
                                gpi.getGridLoc().getNy());
                        gs = new WeatherGridSlice(
                                this.curTime.getValidPeriod(), gpi,
                                new GridDataHistory[] {},
                                (Grid2DByte) dummyGrid, new WeatherKey[] {});
                        break;
                    case DISCRETE:
                        dummyGrid = new Grid2DByte(gpi.getGridLoc().getNx(),
                                gpi.getGridLoc().getNy());
                        gs = new DiscreteGridSlice(
                                this.curTime.getValidPeriod(), gpi,
                                new GridDataHistory[] {},
                                (Grid2DByte) dummyGrid, new DiscreteKey[] {});
                        break;
                    default:
                        return;
                    }
                } else if (gd.length == 0) {
                    return;
                } else {
                    gs = gd[0].getGridSlice();
                }

                if (gs instanceof VectorGridSlice
                        || gs instanceof ScalarGridSlice) {

                    if (this.gridDisplay != null) {
                        this.gridDisplay.dispose();
                    }

                    if (this.contourDisplay != null) {
                        this.contourDisplay.dispose();
                    }

                    this.gridDisplay = null;
                    this.contourDisplay = null;
                }

                if (gs instanceof VectorGridSlice) {
                    VectorGridSlice vectorSlice = (VectorGridSlice) gs;
                    Grid2DBit mask = parm.getDisplayAttributes()
                            .getDisplayMask();

                    if (dataManager.getParmManager().iscMode() || iscParm) {
                        vectorSlice = new VectorGridSlice();
                        mask = dataManager.getIscDataAccess().getCompositeGrid(
                                new GridID(this.parm, this.curTime
                                        .getValidPeriod().getStart()), true,
                                vectorSlice);
                    }

                    Grid2DFloat maskedGrid = new Grid2DFloat(vectorSlice
                            .getMagGrid().getXdim(), vectorSlice.getMagGrid()
                            .getYdim(), Float.NaN);
                    maskedGrid.copyWithMask(vectorSlice.getMagGrid(), mask);
                    FloatBuffer mag = maskedGrid.getBuffer();

                    Grid2DFloat dirGrid = new Grid2DFloat(vectorSlice
                            .getMagGrid().getXdim(), vectorSlice.getDirGrid()
                            .getYdim(), Float.NaN);
                    dirGrid.copyWithMask(vectorSlice.getDirGrid(), mask);
                    FloatBuffer dir = dirGrid.getBuffer();

                    if (visTypes.contains(VisualizationType.IMAGE)) {
                        this.gridDisplay = new GriddedImageDisplay(mag,
                                descriptor, this.gridGeometry);
                    }

                    clearVectorDisplays();
                    for (VisualizationType type : visTypes) {
                        switch (type) {
                        case WIND_ARROW:
                            // get the logFactor
                            double logFactor = prefs.getDouble(parm.getParmID()
                                    .compositeNameUI() + "_arrowScaling");
                            if (logFactor < 0.0) {
                                logFactor = 0.0;
                            }
                            // TODO: add logFactor to PointWindDisplay,
                            // GriddedVectorDisplay

                            this.vectorDisplay.add(new GriddedVectorDisplay(
                                    mag, dir, descriptor, MapUtil
                                            .getGridGeometry(gs.getGridInfo()
                                                    .getGridLoc()),
                                    getVectorSize("WindArrowDefaultSize"),
                                    visTypeToDisplayType(type)));
                            break;

                        case WIND_BARB:
                            this.vectorDisplay.add(new GriddedVectorDisplay(
                                    mag, dir, descriptor, MapUtil
                                            .getGridGeometry(gs.getGridInfo()
                                                    .getGridLoc()),
                                    getVectorSize("WindBarbDefaultSize"),
                                    visTypeToDisplayType(type)));
                            break;

                        case IMAGE:
                            break;

                        default:
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Unsupported Visualization Type: "
                                            + type.toString());
                        }
                    }
                } else if (gs instanceof ScalarGridSlice) {
                    ScalarGridSlice scalarSlice = (ScalarGridSlice) gs;
                    Grid2DBit mask = parm.getDisplayAttributes()
                            .getDisplayMask();

                    if (dataManager.getParmManager().iscMode() || iscParm) {
                        scalarSlice = new ScalarGridSlice();
                        mask = dataManager.getIscDataAccess().getCompositeGrid(
                                new GridID(this.parm, this.curTime
                                        .getValidPeriod().getStart()), true,
                                scalarSlice);

                    }
                    Grid2DFloat scalarGrid = scalarSlice.getScalarGrid();
                    if (scalarGrid != null) {
                        Grid2DFloat maskedGrid = new Grid2DFloat(
                                scalarGrid.getXdim(), scalarGrid.getYdim(),
                                Float.NaN);
                        maskedGrid.copyWithMask(scalarGrid, mask);

                        FloatBuffer fb = maskedGrid.getBuffer();
                        if (visTypes.contains(VisualizationType.IMAGE)) {
                            this.gridDisplay = new GriddedImageDisplay(fb,
                                    descriptor, this.gridGeometry);
                        }

                        if (visTypes.contains(VisualizationType.CONTOUR)) {
                            this.contourDisplay = new GriddedContourDisplay(
                                    descriptor, this.gridGeometry, fb);
                        }
                    }
                } else if (gs instanceof DiscreteGridSlice) {
                    DiscreteGridSlice slice = (DiscreteGridSlice) gs;

                    // Dispose all of the outlineShapes and shadedShapes
                    for (IWireframeShape shape : outlineShapes.values()) {
                        shape.dispose();
                    }
                    outlineShapes.clear();

                    for (Collection<IShadedShape> shapeList : shadedShapes
                            .values()) {
                        for (IShadedShape shadedShape : shapeList) {
                            shadedShape.dispose();
                        }
                        shapeList.clear();
                    }
                    shadedShapes.clear();

                    Grid2DBit mask = parm.getDisplayAttributes()
                            .getDisplayMask();

                    if (dataManager.getParmManager().iscMode() || iscParm) {
                        slice = new DiscreteGridSlice();
                        GridID gid = new GridID(parm, this.curTime
                                .getValidPeriod().getStart());
                        mask = dataManager.getIscDataAccess().getCompositeGrid(
                                gid, true, slice);
                    }

                    for (DiscreteKey discreteKey : slice.getKey()) {

                        if (discreteKey.isValid()) {
                            outlineShapes.put(discreteKey, target
                                    .createWireframeShape(false,
                                            this.descriptor));

                            Collection<IShadedShape> shapeList = new ArrayList<IShadedShape>();
                            shadedShapes.put(discreteKey, shapeList);

                            WxValue wxValue = new DiscreteWxValue(discreteKey,
                                    parm);

                            List<ImageAttr> fillAttrs = DiscreteDisplayUtil
                                    .getFillAttributes(wxValue);

                            boolean first = true;
                            for (ImageAttr attr : fillAttrs) {
                                IShadedShape shadedShape = target
                                        .createShadedShape(false,
                                                this.descriptor, true);
                                shapeList.add(shadedShape);

                                IWireframeShape outlineShape = first ? outlineShapes
                                        .get(discreteKey) : null;
                                first = false;

                                JTSCompiler jtsCompiler = new JTSCompiler(
                                        shadedShape, outlineShape,
                                        this.descriptor);

                                byte[] fillPattern = FillPatterns
                                        .getGLPattern(attr.getFillPatternName());

                                RGB fillColor = RGBColors.getRGBColor(attr
                                        .getColorName());

                                Grid2DBit tmpBit = slice.eq(discreteKey).and(
                                        mask);

                                ReferenceData refData = new ReferenceData(gs
                                        .getGridInfo().getGridLoc(),
                                        new ReferenceID("temp"), tmpBit);

                                jtsCompiler.handle(refData
                                        .getPolygons(CoordinateType.LATLON),
                                        fillColor);
                                shadedShape.compile();
                                shadedShape.setFillPattern(fillPattern);
                            }

                            outlineShapes.get(discreteKey).compile();
                        }
                    }
                } else if (gs instanceof WeatherGridSlice) {
                    WeatherGridSlice slice = (WeatherGridSlice) gs;

                    // Dispose all of the outlineShapes and shadedShapes
                    for (IWireframeShape shape : outlineShapes.values()) {
                        shape.dispose();
                    }
                    outlineShapes.clear();

                    for (Collection<IShadedShape> shapeList : shadedShapes
                            .values()) {
                        for (IShadedShape shadedShape : shapeList) {
                            shadedShape.dispose();
                        }
                        shapeList.clear();
                    }
                    shadedShapes.clear();

                    Grid2DBit mask = parm.getDisplayAttributes()
                            .getDisplayMask();

                    if (dataManager.getParmManager().iscMode() || iscParm) {
                        slice = new WeatherGridSlice();
                        GridID gid = new GridID(parm, this.curTime
                                .getValidPeriod().getStart());
                        mask = dataManager.getIscDataAccess().getCompositeGrid(
                                gid, true, slice);
                    }

                    for (WeatherKey weatherKey : slice.getKeys()) {

                        if (weatherKey.isValid()) {
                            outlineShapes.put(weatherKey, target
                                    .createWireframeShape(false,
                                            this.descriptor));

                            Collection<IShadedShape> shapeList = new ArrayList<IShadedShape>();
                            shadedShapes.put(weatherKey, shapeList);

                            WxValue wxValue = new WeatherWxValue(weatherKey,
                                    parm);

                            List<ImageAttr> fillAttrs = DiscreteDisplayUtil
                                    .getFillAttributes(wxValue);

                            boolean first = true;
                            for (ImageAttr attr : fillAttrs) {
                                IShadedShape shadedShape = target
                                        .createShadedShape(false,
                                                this.descriptor, true);
                                shapeList.add(shadedShape);

                                IWireframeShape outlineShape = first ? outlineShapes
                                        .get(weatherKey) : null;
                                first = false;

                                JTSCompiler jtsCompiler = new JTSCompiler(
                                        shadedShape, outlineShape,
                                        this.descriptor);

                                byte[] fillPattern = FillPatterns
                                        .getGLPattern(attr.getFillPatternName());

                                RGB fillColor = RGBColors.getRGBColor(attr
                                        .getColorName());

                                Grid2DBit tmpBit = slice.eq(weatherKey).and(
                                        mask);

                                ReferenceData refData = new ReferenceData(gs
                                        .getGridInfo().getGridLoc(),
                                        new ReferenceID("temp"), tmpBit);

                                jtsCompiler.handle(refData
                                        .getPolygons(CoordinateType.LATLON),
                                        fillColor);
                                shadedShape.compile();
                                shadedShape.setFillPattern(fillPattern);
                            } // next diIdx

                            outlineShapes.get(weatherKey).compile();
                        }
                    }
                }
            }
        } else if (this.curTime == null) {
            if (this.gridDisplay != null) {
                this.gridDisplay.dispose();
                this.gridDisplay = null;
            }
            this.lastDisplayedTime = null;
        }

        float brightness = 1.0f;
        if (hasCapability(ImagingCapability.class)) {
            ImagingCapability imagingCapability = getCapability(ImagingCapability.class);
            brightness = imagingCapability.getBrightness();
        }

        if (visTypes.contains(VisualizationType.IMAGE)) {
            for (Object key : shadedShapes.keySet()) {
                for (IShadedShape shadedShape : shadedShapes.get(key)) {
                    target.drawShadedShape(shadedShape,
                            myPaintProps.getAlpha(), brightness);
                }
            }
        }

        LineStyle lineStyle = parm.getDisplayAttributes().getLineStyle();
        int lineWidth = parm.getDisplayAttributes().getLineWidth();

        if (visTypes.contains(VisualizationType.BOUNDED_AREA)) {
            org.eclipse.swt.graphics.Rectangle screenBounds = myPaintProps
                    .getCanvasBounds();
            IExtent screenExtent = myPaintProps.getView().getExtent();
            Envelope env = descriptor.pixelToWorld(screenExtent,
                    descriptor.getCRS());

            GridLocation gridLoc = parm.getGridInfo().getGridLoc();
            Envelope envGrid = descriptor.pixelToWorld(screenExtent);

            Coordinate[] coords = new Coordinate[2];
            coords[0] = new Coordinate(envGrid.getMinX(), envGrid.getMinY());
            coords[1] = new Coordinate(envGrid.getMaxX(), envGrid.getMaxY());
            MapUtil.latLonToGridCoordinate(coords, PixelOrientation.CENTER,
                    gridLoc);
            Rectangle screenRect = new Rectangle((int) Math.floor(coords[0].x),
                    (int) Math.floor(coords[1].y), (int) Math.ceil(coords[1].x
                            - coords[0].x), (int) Math.ceil(coords[0].y
                            - coords[1].y));

            boolean boundedAreaLabels = true;
            if (prefs.contains("BoundedArea_Labels")) {
                boundedAreaLabels = prefs.getBoolean("BoundedArea_Labels");
            }
            if (boundedAreaLabels && !dataManager.getParmManager().iscMode()) {
                Coordinate scale = new Coordinate(env.getWidth()
                        / screenBounds.width / 1000, env.getHeight()
                        / screenBounds.height / 1000);
                Coordinate cellSize = gridLoc.gridCellSize();
                float multiplier = (float) (scale.x / cellSize.x);

                paintLabels(target, gfeFont, gd[0], multiplier, screenRect);
            }

            boolean boundedAreaBounds = true;
            if (prefs.contains("BoundedArea_Boundary")) {
                boundedAreaBounds = prefs.getBoolean("BoundedArea_Boundary");
            }
            if (boundedAreaBounds) {
                for (Object key : outlineShapes.keySet()) {
                    Object defaultKey = null;
                    if (parm.getGridInfo().getGridType()
                            .equals(GridType.WEATHER)) {
                        defaultKey = WeatherWxValue.defaultValue(parm)
                                .getWeatherKey();
                    } else {
                        defaultKey = DiscreteWxValue.defaultValue(parm)
                                .getDiscreteKey();
                    }
                    if (!key.equals(defaultKey)) {
                        target.drawWireframeShape(
                                outlineShapes.get(key),
                                this.parm.getDisplayAttributes().getBaseColor(),
                                lineWidth, lineStyle);
                    }
                }
            }
        }

        if (this.gridDisplay != null) {
            // Guarantee colormap parameters and gridinfo are in sync
            ColorMapParameters colorMapParameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();

            ImagingCapability imagingCap = getCapability(ImagingCapability.class);
            GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                    myPaintProps, imagingCap.getBrightness(),
                    imagingCap.getContrast(), imagingCap.isInterpolationState());

            this.gridDisplay.setColorMapParameters(colorMapParameters);
            this.gridDisplay.paint(target, giProps);
        }

        if (this.contourDisplay != null) {
            this.contourDisplay.setColor(this.parm.getDisplayAttributes()
                    .getBaseColor());
            this.contourDisplay.setLineStyle(lineStyle);
            this.contourDisplay.setOutlineWidth(lineWidth);
            this.contourDisplay.setDensity(density);
            this.contourDisplay.setMagnification(contourMagnification);
            ContourPreferences contourPrefs = new ContourPreferences();
            LabelingPreferences labelingPrefs = new LabelingPreferences();
            labelingPrefs.setValues(parm.getDisplayAttributes()
                    .getContourValues());
            contourPrefs.setContourLabeling(labelingPrefs);
            this.contourDisplay.setPreferences(contourPrefs);
            this.contourDisplay.paint(target, myPaintProps);
        }

        if (this.vectorDisplay != null) {
            for (GriddedVectorDisplay v : this.vectorDisplay) {
                v.setASync(!(dataManager.getSpatialDisplayManager() instanceof OffscreenSpatialDisplayManager));
                v.setColor(this.parm.getDisplayAttributes().getBaseColor());
                v.setLineWidth(lineWidth);
                v.setLineStyle(lineStyle);
                v.setDensity(density);
                v.paint(target, myPaintProps);
            }
        }
    }

    /**
     * @param key
     * @return
     */
    private int getVectorSize(String key) {
        String suffix = ("_" + key.charAt(0)).toLowerCase() + key.substring(1);
        int size = Activator.getDefault().getPreferenceStore()
                .getInt(parm.getParmID().compositeNameUI() + suffix);
        if (size == 0) {
            size = Activator.getDefault().getPreferenceStore().getInt(key);
        }
        if (size == 0) {
            size = 60;
        }

        size = (int) (size / 0.8);

        int offset = parm.getDisplayAttributes().getFontOffset()
                + Activator.getDefault().getPreferenceStore()
                        .getInt("Contour_font");
        size = (int) (size * (1.00 + (0.33 * offset)));

        size = Math.max(size, 10);
        return size;
    }

    private DisplayType visTypeToDisplayType(VisualizationType type) {
        switch (type) {
        case WIND_ARROW:
            return DisplayType.ARROW;
        case WIND_BARB:
            return DisplayType.BARB;
        default:
            return null;
        }
    }

    /**
     * 
     */
    private void updateRightClickMenu() {
        // suppress/display capability menu items as appropriate

        VisMode visMode = parm.getDisplayAttributes().getVisMode();

        Set<VisualizationType> visTypes = new HashSet<VisualizationType>(parm
                .getDisplayAttributes().getVisualizationType(
                        EditorType.SPATIAL, visMode));

        getCapability(ColorMapCapability.class).setSuppressingMenuItems(
                !visTypes.contains(VisualizationType.IMAGE)
                        || parm.getGridInfo().getGridType()
                                .equals(GridType.WEATHER));

        getCapability(ImagingCapability.class).setSuppressingMenuItems(
                !visTypes.contains(VisualizationType.IMAGE));
        if (CollectionUtils.containsAny(visTypes, DENSITY_TYPES)) {
            getCapabilities().addCapability(
                    new DensityCapability((double) parm.getDisplayAttributes()
                            .getDensity(), new double[] { -3, -2, -1, 0, 1, 2,
                            3 }));
        } else {
            getCapabilities().removeCapability(DensityCapability.class);
        }
        if (CollectionUtils.containsAny(visTypes, MAG_TYPES)) {
            getCapabilities().addCapability(
                    new MagnificationCapability((double) parm
                            .getDisplayAttributes().getFontOffset(),
                            new double[] { -2, -1, 0, 1, 2, 3 }));
        } else {
            getCapabilities().removeCapability(MagnificationCapability.class);
        }
        // NOTE: retainAll modifies visTypes so we do these last
        visTypes.retainAll(OUTLINE_TYPES);

        getCapability(OutlineCapability.class).setSuppressingMenuItems(
                visTypes.isEmpty());

        getCapability(ColorableCapability.class).setSuppressingMenuItems(
                visTypes.isEmpty());

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.ITimeSeqResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        IGridData[] data = parm.getGridInventory();

        this.dataTimes = new ArrayList<DataTime>();
        for (IGridData d : data) {
            TimeRange tr = d.getGridTime();
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(tr.getStart());
            this.dataTimes.add(new DataTime(cal, tr));
        }

        return this.dataTimes.toArray(new DataTime[this.dataTimes.size()]);
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
        this.pixelCoverage = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            ParmDisplayAttributes dspAttr = this.parm.getDisplayAttributes();
            if (object instanceof ColorableCapability) {
                RGB color = ((ColorableCapability) object).getColor();
                dspAttr.setBaseColor(color);
                if (this.contourDisplay != null) {
                    this.contourDisplay.setColor(color);
                }
                for (GriddedVectorDisplay v : this.vectorDisplay) {
                    v.setColor(color);
                }
            } else if (object instanceof OutlineCapability) {
                OutlineCapability outline = (OutlineCapability) object;
                dspAttr.setLineWidth(outline.getOutlineWidth());
                dspAttr.setLineStyle(outline.getLineStyle());
                if (this.contourDisplay != null) {
                    this.contourDisplay.setOutlineWidth(outline
                            .getOutlineWidth());
                    this.contourDisplay.setLineStyle(outline.getLineStyle());
                }

                for (GriddedVectorDisplay v : this.vectorDisplay) {
                    v.setLineWidth(outline.getOutlineWidth());
                }
            } else if (object instanceof DensityCapability) {
                int density = ((DensityCapability) object).getDensity()
                        .intValue();
                dspAttr.setDensity(density);
            } else if (object instanceof MagnificationCapability) {
                int mag = ((MagnificationCapability) object).getMagnification()
                        .intValue();
                dspAttr.setFontOffset(mag);
            } else if (object instanceof ColorMapCapability) {
                DiscreteDisplayUtil.deleteParmColorMap(this.parm);
                lastDisplayedTime = null;
                issueRefresh();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {

        Parm activatedParm = this.dataManager.getSpatialDisplayManager()
                .getActivatedParm();

        if (this.parm != activatedParm) {
            return;
        }

        String[] tools = dataManager.getSmartToolInterface().listTools(parm);
        if (GFEPreference.contains("AllEditActionsOnPopUp")
                && !GFEPreference.getBooleanPreference("AllEditActionsOnPopUp")) {
            // Offer the user the tools in PopUpEditActions
            // But don't offer tools that aren't OK for the parm
            String[] putools = Activator.getDefault().getPreferenceStore()
                    .getStringArray("PopUpEditActions");
            List<String> toolList = new ArrayList<String>(Arrays.asList(tools));
            List<String> puToolList = Arrays.asList(putools);
            toolList.retainAll(puToolList);
            tools = toolList.toArray(new String[toolList.size()]);
        }

        for (final String tool : tools) {
            AbstractRightClickAction action = new AbstractRightClickAction() {

                @Override
                public String getText() {
                    return tool;
                }

                @Override
                public void run() {
                    SmartUtil.runTool(tool);
                }
            };
            menuManager.add(action);
        }
        menuManager.add(new Separator());
        menuManager.add(new SelectContiguousAction(true));
        menuManager.add(new SelectContiguousAction(false));

        switch (this.parm.getGridInfo().getGridType()) {
        case DISCRETE:
        case WEATHER:
            menuManager.add(new ChangeCombineMode(this.parm));
            break;

        case VECTOR:
            menuManager.add(new VectorEditModeAction());
        case SCALAR:
            menuManager.add(new FuzzValueAction());
        }
    }

    private class FuzzValueAction extends AbstractRightClickAction {
        public FuzzValueAction() {
            super("Set Fuzz Value...");

        }

        @Override
        public void run() {
            FuzzValueDialog.openDialog(dataManager);
        }
    }

    /**
     * Provides the select homogeneous and deselect contiguous right click
     * actions for gfe resources
     * 
     */
    private class SelectContiguousAction extends AbstractRightClickAction {

        private final boolean select;

        public SelectContiguousAction(boolean select) {
            this.select = select;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            if (select) {
                return "Select Homogenous Area";
            }

            return "Deselect Contiguous Area";
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            IDisplayPane pane = getResourceContainer().getActiveDisplayPane();

            int x = pane.getLastClickX();
            int y = pane.getLastClickY();

            Coordinate coord = getResourceContainer().translateClick(x, y);
            if (coord != null) {
                selectContiguousArea(coord, this.select);
            }
        }

        // Called when the user requests to select or deselect a contiguous
        // area.
        // The select parameter determines whether the area is selected or
        // deselected.
        // -- implementation
        // ---------------------------------------------------------
        // Find the gridpoint that was selected and get the contiguous area
        // (select)
        // or get the selected area (deselect).
        // ---------------------------------------------------------------------------
        protected void selectContiguousArea(final Coordinate coord,
                final boolean select) {
            IGridData[] gd = parm.getGridInventory(curTime.getValidPeriod());

            if (gd.length != 1) {
                return;
            }

            final IGridData grid = gd[0];

            // Convert to a gridpoint and get the contiguous area from grid

            final GridLocation gridLocation = grid.getParm().getGridInfo()
                    .getGridLoc();
            Coordinate gridCoord = MapUtil.latLonToGridCoordinate(coord,
                    PixelOrientation.CENTER, gridLocation);

            if (gridCoord.x < 0 || gridCoord.y < 0
                    || gridCoord.x >= gridLocation.getNx()
                    || gridCoord.y >= gridLocation.getNy()) {
                return;
            }

            final Point pt = new Point((int) Math.round(gridCoord.x),
                    (int) Math.round(gridCoord.y));

            BusyIndicator.showWhile(Display.getCurrent(), new Runnable() {

                @Override
                public void run() {
                    if (select) {

                        Grid2DBit gridCells = grid.getContiguousArea(curTime
                                .getValidPeriod().getStart(), pt);
                        ReferenceData refData = new ReferenceData(gridLocation,
                                new ReferenceID("contiguous"), gridCells);
                        dataManager.getRefManager().incomingRefSet(refData,
                                RefSetMode.USE_CURRENT);

                    } else // deselect
                    {
                        // Get the refSet and the contiguous area for this
                        // intCoord
                        final Grid2DBit refSet = dataManager.getRefManager()
                                .getActiveRefSet().getGrid();
                        Grid2DBit editInfluence = refSet.contiguousBitArray(pt);
                        if (editInfluence.isAnyBitsSet()) {
                            ReferenceData refData = new ReferenceData(
                                    gridLocation,
                                    new ReferenceID("contiguous"),
                                    editInfluence);
                            // Send the message that changes the current
                            // reference set
                            dataManager.getRefManager().incomingRefSet(refData,
                                    RefSetMode.SUBTRACT);
                        }
                    }
                }

            });

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#okToUnload()
     */
    @Override
    public boolean okToUnload() {
        if (parm.isModified()) {
            return false;
        }

        return super.okToUnload();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#unload(com.raytheon.
     * uf.viz.core.rsc.ResourceList)
     */
    @Override
    public void unload(ResourceList list) {
        dataManager.getParmManager().deleteParm(parm);
        super.unload(list);
    }

    // BoundedAreaVisual::paintLabels
    //
    // Paint the labels on the BoundedAreaVisual.
    //
    // -- implementation
    // ---------------------------------------------------------
    //
    // Call findLabelLoc() to get a series of grid points. For each grid point,
    // see if a label can be drawn there. This is done by figuring out what
    // label will be printed there, figuring out what the size of this label (in
    // grid cells) will be. (Figuring out the size is done via functions in
    // the Graphics class.) That many adjacent grid cells will be examined and
    // if all of them have the same data as the label's originating grid cell,
    // then the label will be drawn.
    // ---------------------------------------------------------------------------
    private void paintLabels(IGraphicsTarget target, IFont font,
            IGridData grid, float multiplier, Rectangle screenRect) {
        // Calculate the positions of where labels should go.
        // get a Point[] and we'll try to put a label at each
        // point returned.

        Grid2DByte byteData = null;
        if (grid instanceof DiscreteGridData) {
            byteData = ((DiscreteGridData) grid).getDiscreteSlice()
                    .getDiscreteGrid();
        } else if (grid instanceof WeatherGridData) {
            byteData = ((WeatherGridData) grid).getWeatherSlice()
                    .getWeatherGrid();
        }
        if (byteData == null) {
            return;
        }

        Point gridDim = grid.getParm().getGridInfo().getGridLoc().gridSize();
        Rectangle gDomain = new Rectangle(0, 0, gridDim.x, gridDim.y);
        Rectangle rect = gDomain.intersection(screenRect);

        Point[] points = findLabelLoc(gridDim, rect, multiplier);

        // now consider every label point returned.
        for (int i = 0; i < points.length; i++) {
            try {
                int xGrid = points[i].x;
                int yGrid = points[i].y;

                // figure out how many adjacent grid cells I need to draw
                // this label. first get the string for the label.
                String label = grid.getWxValue(xGrid, yGrid).toString();

                // the largest pixel size for a label's x dimension is
                // maxLabelLength. If this label is wider than that,
                // truncate it.
                label = GfeUiUtil.truncateLabelToFit(target, font, label,
                        maxLabelLength, true);

                // now, using this string's size in pixels, figure out how
                // many grid cells it needs.
                DrawableString ds = new DrawableString(label, parm
                        .getDisplayAttributes().getBaseColor());
                ds.font = font;
                Rectangle2D labelExtent = target.getStringsBounds(ds);

                int xLabelGrid = (int) (labelExtent.getWidth() * multiplier) + 1;
                int yLabelGrid = (int) (labelExtent.getHeight() * multiplier) + 1;

                // now see if adjacent grid cells exist containing the
                // same data.
                if ((xGrid + xLabelGrid > gridDim.x) || (xGrid - 1 < 0)) {
                    continue;
                }
                if ((yGrid + yLabelGrid > gridDim.y) || (yGrid - 1 < 0)) {
                    continue;
                }

                byte weatherByteVal = byteData.get(xGrid, yGrid);

                boolean printLabel = true;
                for (int ii = xGrid - 1; ii < xGrid + xLabelGrid; ii++) {
                    for (int j = yGrid - 1; j < yGrid + yLabelGrid; j++) {
                        if (!parm.getDisplayAttributes().getDisplayMask()
                                .getAsBoolean(ii, j)
                                || byteData.get(ii, j) != weatherByteVal) {
                            printLabel = false;
                            break;
                        }
                    }
                }
                if (printLabel) {
                    ReferencedCoordinate c = new ReferencedCoordinate(
                            new Coordinate(xGrid, yGrid),
                            MapUtil.getGridGeometry(parm.getGridInfo()
                                    .getGridLoc()), Type.GRID_CENTER);
                    Coordinate coord = c.asPixel(descriptor.getGridGeometry());
                    ds.setCoordinates(coord.x, coord.y);
                    ds.horizontalAlignment = HorizontalAlignment.CENTER;
                    ds.verticallAlignment = VerticalAlignment.MIDDLE;
                    ds.rotation = 0.0;
                    target.drawStrings(ds);
                }
                printLabel = true;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error displaying label", e);
            }
        }
    }

    // Return (x,y) grid points that are within the current domain and thus
    // are candidates for having labels drawn in them.
    //
    // -- implementation
    // ---------------------------------------------------------
    // Generate a sort-of random but sort-of patterned group of points to put
    // labels in. See algorithm in the code - it's pretty easy to figure out,
    // it involves figuring out a grid interval that's essentially some
    // number of pixels (pixelDistance, a static const) apart.
    // Then a fraction of that interval is the offset that each label
    // point will vary from row to row, column to column. The row pattern and
    // column patterns will reset for each 4 labels.
    private Point[] findLabelLoc(Point gridDim, Rectangle gDomain,
            float multiplier) {
        // accumulate all label points in a SeqOf<points>, construct the SeqOf.
        List<Point> points = new ArrayList<Point>();

        // Start in loop from the lower-left grid point, once you identify
        // a grid cell to put a label in, see if it's within the gridDomain.

        // first, using the multiplier, figure out the interval in grid
        // cells between each label. Spacing will be one label every some
        // pixels (with some modifications added). The Y spacing will be
        // the same as the X spacing, so we only need an int, calculated
        // from the X spacing dimension of the multiplier.
        int gridInterval = (int) Math.max(1, pixelDistance * multiplier);
        int gridOffset;
        if (gridInterval <= 11) {
            gridOffset = 2;
        } else {
            gridOffset = gridInterval / 4;
        }

        int xpos, ypos; // grid cell indices where labels are attempted
        int yCounter = 0;

        for (xpos = gridOffset; xpos < gridDim.x; xpos += gridInterval) {
            yCounter++;
            int xCounter = 0;
            for (ypos = yCounter % 4 + 1; ypos < gridDim.y - gridOffset; ypos += gridInterval) {
                xCounter++;
                int xGrid = xpos + (xCounter % 4 * gridOffset);
                if (xGrid > gridDim.x) {
                    break;
                }
                int yGrid = ypos;
                if (yGrid > gridDim.y) {
                    break;
                }

                // make sure this label position is within the currently-
                // being-painted gridDomain.
                Point p = new Point(xGrid, yGrid);
                if (gDomain.contains(p)) {
                    points.add(p);
                }
            }
        }
        return points.toArray(new Point[points.size()]);
    }

    @Override
    public String toString() {
        return this.parm.getParmID().toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowISCGridsMsg) {
            showIScGrid((ShowISCGridsMsg) message);
        }
    }

    /**
     * @param message
     */
    private void showIScGrid(ShowISCGridsMsg message) {
        Date date = this.dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();
        GridID gid = new GridID(this.parm, date);
        gid = this.dataManager.getIscDataAccess().getISCGridID(gid, true);
        if (gid != null) {
            issueRefresh();
        }

        Parm iscParm = this.dataManager.getIscDataAccess()
                .getISCParm(this.parm);
        if (iscParm != null) {
            if (message.show()) {
                // iscParm.getListeners().addParmInventoryChangedListener(
                // this.parmInventoryChanged);
                iscParm.getListeners().addGridChangedListener(this.gridChanged);
            } else {
                // iscParm.getListeners().removeParmInventoryChangedListener(
                // this.parmInventoryChanged);
                iscParm.getListeners().removeGridChangedListener(
                        this.gridChanged);
            }
        }
    }

}
