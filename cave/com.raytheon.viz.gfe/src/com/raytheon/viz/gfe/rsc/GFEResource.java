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
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
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
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
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
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ContourLabelingPreferences;
import com.raytheon.uf.common.style.ValuesLabelingPreferences;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay.GriddedImagePaintProperties;
import com.raytheon.uf.viz.core.grid.display.GriddedVectorDisplay;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
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
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.actions.ChangeCombineMode;
import com.raytheon.viz.gfe.actions.VectorEditModeAction;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.DiscreteDataObject;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarDataObject;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorDataObject;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherDataObject;
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
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;

/**
 * Resource for displaying renderables for a particular Parm.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 01, 2008           chammack  Initial Creation.
 * Aug 20, 2008           dglazesk  Update for the ColorMap interface change
 * Nov 23, 2011           mli       set vector lineStyle
 * May 11, 2012           njensen   Allow rsc to be recycled
 * Nov 08, 2012  1298     rferrel   Changes for non-blocking FuzzValueDialog.
 * Mar 04, 2013  1637     randerso  Fix time matching for ISC grids
 * Aug 27, 2013  2287     randerso  Fixed scaling and direction of wind arrows
 * Sep 23, 2013  2363     bsteffen  Add more vector configuration options.
 * Oct 31, 2013  2508     randerso  Change to use DiscreteGridSlice.getKeys()
 * Dec 11, 2013  2621     randerso  Removed conditional from getParm so it never
 *                                  returns null
 * Jan 23, 2014  2703     bsteffen  Allow construction using a resourceData,
 *                                  paint using the time in paintProps and
 *                                  remove dead code in paintInternal
 * Apr 03, 2014  2737     randerso  Uncommented out listers for iscParm
 *                                  inventory changed
 * May 20, 2014  15814    zhao      Make image display for model Parm not
 *                                  affected by ISC mode
 * Jan 13, 2015  3995     randerso  Correctly fixed display of model Parms in
 *                                  ISC mode so ISC will work when editing Topo.
 * Sep 14, 2016  3241     bsteffen  Update deprecated JTSCompiler method calls
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Jan 03, 2018  7178     randerso  Changes to support IDataObject
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Nov 15, 2018  58492    edebebe   Enabled configurable 'Wind Barb' properties
 * Feb 13, 2019  7732     randerso  Added code to catch null/invalid
 *                                  DiscreteKeys, replace them with <None>, log
 *                                  an error message indicating the issue has
 *                                  occurred and NCF should be notified, and
 *                                  save off the associated grids.
 * May 29, 2019 60162     ksunil    changes to absorb new Contour Label structure
 *
 * </pre>
 *
 * @author chammack
 */
public class GFEResource
        extends AbstractVizResource<GFEResourceData, MapDescriptor> implements
        IResourceDataChanged, IContextMenuContributor, IMessageClient {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEResource.class);

    /* arbitrary value chosen to most closely match A1 */
    private static final double VECTOR_DENSITY_FACTOR = 1.36;

    /* Unknown source, provides acceptable sized barbs. */
    private static final double BARB_SCALE_FACTOR = 0.4;

    /* Unknown source, provides acceptable sized arrows heads. */
    private static final double ARROW_HEAD_RATIO = 1.0 / 7.0;

    /** maximum label size in pixels */
    private static final int maxLabelLength = 100;

    /** distance between labels */
    private static final int pixelDistance = 125;

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

    protected Map<Object, IWireframeShape> outlineShapes = new HashMap<>();

    protected Map<Object, Collection<IShadedShape>> shadedShapes = new HashMap<>();

    private IFont gfeFont = null;

    // Parameters used to construct 'VectorGraphicsConfig'
    private static final String PLUGIN_NAME = "GFEPlugin";

    private static final String CLASS_NAME = "GFEResource";

    private VectorGraphicsConfig vectorConfig = new VectorGraphicsConfig(
            PLUGIN_NAME, CLASS_NAME);

    protected IGridDataChangedListener gridChanged = new IGridDataChangedListener() {
        @Override
        public void gridDataChanged(ParmID incomingParm, TimeRange validTime) {
            resetFrame(validTime);
        }
    };

    protected IParmInventoryChangedListener parmInventoryChanged = new IParmInventoryChangedListener() {

        @Override
        public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
            resetFrame(timeRange);
            TimeMatchingJob.scheduleTimeMatch(getDescriptor());
        }

    };

    protected IParmIDChangedListener parmIdChanged = new IParmIDChangedListener() {

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
        this(new GFEResourceData(parm.getParmID()), new LoadProperties(), parm,
                dataManager);
    }

    /**
     * Same functionality as {@link #GFEResource(Parm, DataManager)} but uses a
     * predefined resourceData.
     *
     * @param resourceData
     *            the resourceData
     * @param loadProps
     *            the load properties
     * @param parm
     *            the parm
     * @param dataManager
     *            the datamanager responsible for it
     */
    public GFEResource(GFEResourceData resourceData, LoadProperties loadProps,
            Parm parm, DataManager dataManager) {
        super(resourceData, loadProps, false);

        this.resourceData.addChangeListener(this);
        this.parm = parm;
        this.dataManager = dataManager;

        this.vectorDisplay = new ArrayList<>();

        // Construct reasonable initial colormap parameters
        // from the parm data
        ColorMapParameters colorMapParameters = DiscreteDisplayUtil
                .buildColorMapParameters(parm);

        getCapability(ColorMapCapability.class)
                .setColorMapParameters(colorMapParameters);

        int lineWidth = parm.getDisplayAttributes().getLineWidth();
        LineStyle style = parm.getDisplayAttributes().getLineStyle();
        getCapability(OutlineCapability.class).setLineStyle(style);
        getCapability(OutlineCapability.class).setOutlineWidth(lineWidth);

        GridParmInfo info = this.parm.getGridInfo();
        this.gridGeometry = MapUtil.getGridGeometry(info.getGridLoc());

        lastIscMode = dataManager.getParmManager().iscMode();

        updateRightClickMenu();
    }

    /**
     * Reset the resource
     */
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
        if ((curTime != null) && timeRange.overlaps(curTime.getValidPeriod())) {
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
        return this.parm;
    }

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
        parm.getListeners()
                .removeParmInventoryChangedListener(parmInventoryChanged);
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

    @Override
    public String getName() {
        return "Parm Resource " + this.parm.getParmID().toString();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        parm.getListeners().addGridChangedListener(this.gridChanged);
        parm.getListeners()
                .addParmInventoryChangedListener(this.parmInventoryChanged);
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

    private void logGridData(GridID gid) {
        String dir = System.getenv("FULL_LOGDIR");
        if (dir == null) {
            dir = FileUtil.join(System.getProperty("user.home"), "caveData",
                    "logs", "consoleLogs", System.getenv("HOSTNAME"));
        }
        String fileName = gid.toString().replace(':', '_') + ".dat";
        String filePath = FileUtil.join(dir, fileName);
        File file = new File(filePath);
        file.getParentFile().mkdirs();

        IGridData gridData = gid.grid();
        if (gridData != null) {
            IGridSlice gridSlice = gridData.getGridSlice();
            try (OutputStream os = new FileOutputStream(filePath)) {
                SerializationUtil.transformToThriftUsingStream(gridSlice, os);
            } catch (IOException | SerializationException e) {
                statusHandler.error("Error saving grid to: " + filePath, e);
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        GFEPaintProperties myPaintProps = (GFEPaintProperties) paintProps;
        this.curTime = myPaintProps.getDataTime();

        // No data to be displayed here
        if (curTime == null) {
            return;
        }

        // translate GFE style mag/density to D2D style
        // -3 to 3 -> 0.5 to 2
        double magnification = Math.pow(1.26,
                parm.getDisplayAttributes().getFontOffset());
        gfeFont.setMagnification((float) magnification);

        // hack to get around target.getDefaultFont()-centeredness in
        // contourDisplay.
        double contourMagnification = (magnification * gfeFont.getFontSize())
                / target.getDefaultFont().getFontSize();

        double density = parm.getDisplayAttributes().getDensity();
        density = Math.pow(1.26, density);

        this.lastGraphicsTarget = target;

        IGridData[] gd = this.parm
                .getGridInventory(this.curTime.getValidPeriod());

        boolean iscParm = this.parm.isIscParm();
        GridID gid = new GridID(this.parm, this.curTime.getRefTime());
        GridID iscGid = dataManager.getIscDataAccess().getISCGridID(gid, true);

        if ((gd.length == 0) && !dataManager.getParmManager().iscMode()) {
            return;
        }

        VisMode visMode = myPaintProps.getVisMode();
        Set<VisualizationType> visTypes = parm.getDisplayAttributes()
                .getVisualizationType(EditorType.SPATIAL, visMode);

        if (!this.curTime.equals(this.lastDisplayedTime)
                || !visMode.equals(this.lastVisMode)
                || (this.lastIscMode != dataManager.getParmManager()
                        .iscMode())) {

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

            IGridData gridData = null;
            if (dataManager.getParmManager().iscMode() && (gd.length == 0)) {
                GridParmInfo gpi = this.parm.getGridInfo();
                GridType gridType = gpi.getGridType();

                IGrid2D dummyGrid = null;
                IGridSlice gs = null;
                switch (gridType) {
                case SCALAR:
                    dummyGrid = new Grid2DFloat(gpi.getGridLoc().getNx(),
                            gpi.getGridLoc().getNy(), Float.NaN);
                    gs = new ScalarGridSlice(this.curTime.getValidPeriod(), gpi,
                            new GridDataHistory[] {}, (Grid2DFloat) dummyGrid);
                    break;
                case VECTOR:
                    dummyGrid = new Grid2DFloat(gpi.getGridLoc().getNx(),
                            gpi.getGridLoc().getNy(), Float.NaN);
                    gs = new VectorGridSlice(this.curTime.getValidPeriod(), gpi,
                            new GridDataHistory[] {}, (Grid2DFloat) dummyGrid,
                            (Grid2DFloat) dummyGrid);
                    break;
                case WEATHER:
                    dummyGrid = new Grid2DByte(gpi.getGridLoc().getNx(),
                            gpi.getGridLoc().getNy());
                    gs = new WeatherGridSlice(this.curTime.getValidPeriod(),
                            gpi, new GridDataHistory[] {},
                            (Grid2DByte) dummyGrid, new WeatherKey[] {});
                    break;
                case DISCRETE:
                    dummyGrid = new Grid2DByte(gpi.getGridLoc().getNx(),
                            gpi.getGridLoc().getNy());
                    gs = new DiscreteGridSlice(this.curTime.getValidPeriod(),
                            gpi, new GridDataHistory[] {},
                            (Grid2DByte) dummyGrid, new DiscreteKey[] {});
                    break;
                default:
                    return;
                }
                gridData = AbstractGridData.makeGridData(this.parm, gs, false);
            } else {
                gridData = gd[0];
            }

            if ((gridData instanceof VectorGridData)
                    || (gridData instanceof ScalarGridData)) {

                if (this.gridDisplay != null) {
                    this.gridDisplay.dispose();
                }

                if (this.contourDisplay != null) {
                    this.contourDisplay.dispose();
                }

                this.gridDisplay = null;
                this.contourDisplay = null;
            }

            if (gridData instanceof VectorGridData) {
                VectorDataObject dataObject = ((VectorGridData) gridData)
                        .getDataObject();
                Grid2DBit mask = parm.getDisplayAttributes().getDisplayMask();

                if ((dataManager.getParmManager().iscMode() || iscParm)
                        && (iscGid != null)) {
                    Pair<Grid2DBit, IGridData> p = dataManager
                            .getIscDataAccess().getCompositeGrid(gid, true);
                    mask = p.getFirst();
                    dataObject = (VectorDataObject) p.getSecond()
                            .getDataObject();
                }

                Grid2DFloat magGrid = dataObject.getMagGrid();

                Grid2DFloat maskedGrid = new Grid2DFloat(magGrid.getXdim(),
                        magGrid.getYdim(), Float.NaN);
                maskedGrid.copyWithMask(dataObject.getMagGrid(), mask);
                FloatBuffer mag = maskedGrid.getBuffer();

                Grid2DFloat dirGrid = new Grid2DFloat(magGrid.getXdim(),
                        magGrid.getYdim(), Float.NaN);
                dirGrid.copyWithMask(dataObject.getDirGrid(), mask);
                FloatBuffer dir = dirGrid.getBuffer();

                if (visTypes.contains(VisualizationType.IMAGE)) {
                    this.gridDisplay = new GriddedImageDisplay(mag, descriptor,
                            this.gridGeometry);
                }

                clearVectorDisplays();

                for (VisualizationType type : visTypes) {
                    switch (type) {
                    case WIND_ARROW:

                        double size = getVectorSize("WindArrowDefaultSize");
                        vectorConfig.setBaseSize(size);
                        vectorConfig.setCalmCircleSizeRatio(
                                vectorConfig.getCalmCircleSizeRatio()
                                        * BARB_SCALE_FACTOR);
                        vectorConfig.setArrowHeadStaffRatio(ARROW_HEAD_RATIO);
                        vectorConfig.alwaysIncludeCalmCircle();
                        vectorConfig.alwaysIncludeVector();
                        // get the logFactor
                        double logFactor = GFEPreference
                                .getDouble(parm.getParmID().compositeNameUI()
                                        + "_arrowScaling");
                        double maxVal = parm.getGridInfo().getMaxValue();
                        if (logFactor <= 0.0) {
                            vectorConfig
                                    .setLinearArrowScaleFactor(size / maxVal);
                        } else {
                            vectorConfig.setArrowScaler(new LogArrowScalar(size,
                                    logFactor, maxVal));
                        }

                        this.vectorDisplay.add(new GriddedVectorDisplay(mag,
                                dir, descriptor,
                                MapUtil.getGridGeometry(
                                        gridData.getGridInfo().getGridLoc()),
                                VECTOR_DENSITY_FACTOR, false,
                                visTypeToDisplayType(type), vectorConfig));
                        break;

                    case WIND_BARB:

                        vectorConfig.setBaseSize(
                                getVectorSize("WindBarbDefaultSize")
                                        * BARB_SCALE_FACTOR);
                        vectorConfig.alwaysIncludeCalmCircle();
                        vectorConfig.alwaysIncludeVector();
                        this.vectorDisplay.add(new GriddedVectorDisplay(mag,
                                dir, descriptor,
                                MapUtil.getGridGeometry(
                                        gridData.getGridInfo().getGridLoc()),
                                VECTOR_DENSITY_FACTOR / BARB_SCALE_FACTOR,
                                false, visTypeToDisplayType(type),
                                vectorConfig));
                        break;

                    case IMAGE:
                        break;

                    default:
                        statusHandler.handle(Priority.PROBLEM,
                                "Unsupported Visualization Type: "
                                        + type.toString());
                    }
                }
            } else if (gridData instanceof ScalarGridData) {
                ScalarDataObject dataObject = ((ScalarGridData) gridData)
                        .getDataObject();
                Grid2DBit mask = parm.getDisplayAttributes().getDisplayMask();

                if ((dataManager.getParmManager().iscMode() || iscParm)
                        && (iscGid != null)) {
                    dataObject = dataObject.copy();
                    Pair<Grid2DBit, IGridData> p = dataManager
                            .getIscDataAccess().getCompositeGrid(gid, true);
                    mask = p.getFirst();
                    dataObject = (ScalarDataObject) p.getSecond()
                            .getDataObject();
                }
                Grid2DFloat scalarGrid = dataObject.getScalarGrid();
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
            } else if (gridData instanceof DiscreteGridData) {
                DiscreteDataObject dataObject = ((DiscreteGridData) gridData)
                        .getDataObject();

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

                Grid2DBit mask = parm.getDisplayAttributes().getDisplayMask();

                if ((dataManager.getParmManager().iscMode() || iscParm)
                        && (iscGid != null)) {
                    Pair<Grid2DBit, IGridData> p = dataManager
                            .getIscDataAccess().getCompositeGrid(gid, true);
                    mask = p.getFirst();
                    dataObject = (DiscreteDataObject) p.getSecond()
                            .getDataObject();
                }

                // validate keys
                DiscreteKey[] keys = dataObject.getKeys();
                List<Integer> badKeys = new ArrayList<>(keys.length);
                for (int i = 0; i < keys.length; i++) {
                    if (keys[i] == null || !keys[i].isValid()) {
                        // bad key!
                        badKeys.add(i);
                    }
                }

                // if invalid key found
                if (!badKeys.isEmpty()) {
                    // log grid data for our grid and the isc grid
                    logGridData(gid);
                    logGridData(iscGid);

                    // attempt fix up
                    ParmID id = this.getParm().getParmID();
                    DiscreteKey defaultKey = DiscreteKey
                            .defaultKey(id.getDbId().getSiteId(), id);
                    for (int i : badKeys) {
                        String badKey = "null";
                        if (keys[i] != null) {
                            badKey = keys[i].getOrigStr();
                        }
                        statusHandler.error(String.format(
                                "Invalid key found: \"%s\", replacing with \"%s\". Please notify NCF.",
                                badKey, defaultKey));
                        keys[i] = defaultKey;
                    }
                    dataObject.setKeys(keys);
                }

                for (DiscreteKey discreteKey : dataObject.getKeys()) {

                    if (discreteKey.isValid()) {
                        outlineShapes.put(discreteKey, target
                                .createWireframeShape(false, this.descriptor));

                        Collection<IShadedShape> shapeList = new ArrayList<>();
                        shadedShapes.put(discreteKey, shapeList);

                        WxValue wxValue = new DiscreteWxValue(discreteKey,
                                parm);

                        List<ImageAttr> fillAttrs = DiscreteDisplayUtil
                                .getFillAttributes(wxValue);

                        boolean first = true;
                        for (ImageAttr attr : fillAttrs) {
                            IShadedShape shadedShape = target.createShadedShape(
                                    false, this.descriptor.getGridGeometry());
                            shapeList.add(shadedShape);

                            IWireframeShape outlineShape = first
                                    ? outlineShapes.get(discreteKey) : null;
                            first = false;

                            JTSCompiler jtsCompiler = new JTSCompiler(
                                    shadedShape, outlineShape, this.descriptor);

                            byte[] fillPattern = FillPatterns
                                    .getGLPattern(attr.getFillPatternName());

                            RGB fillColor = RGBColors
                                    .getRGBColor(attr.getColorName());
                            JTSGeometryData jtsData = jtsCompiler
                                    .createGeometryData();
                            jtsData.setGeometryColor(fillColor);

                            Grid2DBit tmpBit = dataObject.eq(discreteKey)
                                    .and(mask);

                            ReferenceData refData = new ReferenceData(
                                    gridData.getGridInfo().getGridLoc(),
                                    new ReferenceID("temp"), tmpBit);

                            jtsCompiler.handle(
                                    refData.getPolygons(CoordinateType.LATLON),
                                    jtsData);
                            shadedShape.compile();
                            shadedShape.setFillPattern(fillPattern);
                        }

                        outlineShapes.get(discreteKey).compile();
                    }
                }
            } else if (gridData instanceof WeatherGridData) {
                WeatherDataObject dataObject = ((WeatherGridData) gridData)
                        .getDataObject();

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

                Grid2DBit mask = parm.getDisplayAttributes().getDisplayMask();

                if ((dataManager.getParmManager().iscMode() || iscParm)
                        && (iscGid != null)) {
                    Pair<Grid2DBit, IGridData> p = dataManager
                            .getIscDataAccess().getCompositeGrid(gid, true);
                    mask = p.getFirst();
                    dataObject = (WeatherDataObject) p.getSecond()
                            .getDataObject();
                }

                for (WeatherKey weatherKey : dataObject.getKeys()) {

                    if (weatherKey.isValid()) {
                        outlineShapes.put(weatherKey, target
                                .createWireframeShape(false, this.descriptor));

                        Collection<IShadedShape> shapeList = new ArrayList<>();
                        shadedShapes.put(weatherKey, shapeList);

                        WxValue wxValue = new WeatherWxValue(weatherKey, parm);

                        List<ImageAttr> fillAttrs = DiscreteDisplayUtil
                                .getFillAttributes(wxValue);

                        boolean first = true;
                        for (ImageAttr attr : fillAttrs) {
                            IShadedShape shadedShape = target.createShadedShape(
                                    false, this.descriptor.getGridGeometry());
                            shapeList.add(shadedShape);

                            IWireframeShape outlineShape = first
                                    ? outlineShapes.get(weatherKey) : null;
                            first = false;

                            JTSCompiler jtsCompiler = new JTSCompiler(
                                    shadedShape, outlineShape, this.descriptor);

                            byte[] fillPattern = FillPatterns
                                    .getGLPattern(attr.getFillPatternName());

                            RGB fillColor = RGBColors
                                    .getRGBColor(attr.getColorName());
                            JTSGeometryData jtsData = jtsCompiler
                                    .createGeometryData();
                            jtsData.setGeometryColor(fillColor);

                            Grid2DBit tmpBit = dataObject.eq(weatherKey)
                                    .and(mask);

                            ReferenceData refData = new ReferenceData(
                                    gridData.getGridInfo().getGridLoc(),
                                    new ReferenceID("temp"), tmpBit);

                            jtsCompiler.handle(
                                    refData.getPolygons(CoordinateType.LATLON),
                                    jtsData);
                            shadedShape.compile();
                            shadedShape.setFillPattern(fillPattern);
                        }

                        outlineShapes.get(weatherKey).compile();
                    }
                }
            }
        }

        float brightness = 1.0f;
        if (hasCapability(ImagingCapability.class)) {
            ImagingCapability imagingCapability = getCapability(
                    ImagingCapability.class);
            brightness = imagingCapability.getBrightness();
        }

        if (visTypes.contains(VisualizationType.IMAGE)) {
            for (Collection<IShadedShape> shapes : shadedShapes.values()) {
                for (IShadedShape shadedShape : shapes) {
                    target.drawShadedShape(shadedShape, myPaintProps.getAlpha(),
                            brightness);
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
                    (int) Math.floor(coords[1].y),
                    (int) Math.ceil(coords[1].x - coords[0].x),
                    (int) Math.ceil(coords[0].y - coords[1].y));

            boolean boundedAreaLabels = GFEPreference
                    .getBoolean("BoundedArea_Labels", true);
            if (boundedAreaLabels && !dataManager.getParmManager().iscMode()) {
                Coordinate scale = new Coordinate(
                        env.getWidth() / screenBounds.width / 1000,
                        env.getHeight() / screenBounds.height / 1000);
                Coordinate cellSize = gridLoc.gridCellSize();
                float multiplier = (float) (scale.x / cellSize.x);

                paintLabels(target, gfeFont, gd[0], multiplier, screenRect);
            }

            boolean boundedAreaBounds = GFEPreference
                    .getBoolean("BoundedArea_Boundary", true);
            if (boundedAreaBounds) {
                for (Entry<Object, IWireframeShape> entry : outlineShapes
                        .entrySet()) {
                    Object defaultKey = null;
                    if (parm.getGridInfo().getGridType()
                            .equals(GridType.WEATHER)) {
                        defaultKey = WeatherWxValue.defaultValue(parm)
                                .getWeatherKey();
                    } else {
                        defaultKey = DiscreteWxValue.defaultValue(parm)
                                .getDiscreteKey();
                    }
                    if (!entry.getKey().equals(defaultKey)) {
                        target.drawWireframeShape(entry.getValue(),
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

            ImagingCapability imagingCap = getCapability(
                    ImagingCapability.class);
            GriddedImagePaintProperties giProps = new GriddedImagePaintProperties(
                    myPaintProps, imagingCap.getBrightness(),
                    imagingCap.getContrast(),
                    imagingCap.isInterpolationState());

            this.gridDisplay.setColorMapParameters(colorMapParameters);
            this.gridDisplay.paint(target, giProps);
        }

        if (this.contourDisplay != null) {
            this.contourDisplay
                    .setColor(this.parm.getDisplayAttributes().getBaseColor());
            this.contourDisplay.setLineStyle(lineStyle);
            this.contourDisplay.setOutlineWidth(lineWidth);
            this.contourDisplay.setDensity(density);
            this.contourDisplay.setMagnification(contourMagnification);
            ContourPreferences contourPrefs = new ContourPreferences();
            ContourLabelingPreferences labelingPrefs = new ContourLabelingPreferences();
            ValuesLabelingPreferences pref = new ValuesLabelingPreferences();
            pref.setValues(parm.getDisplayAttributes().getContourValues());
            labelingPrefs.setValues(new ArrayList<>(Arrays.asList(pref)));
            contourPrefs.setContourLabeling(labelingPrefs);
            this.contourDisplay.setPreferences(contourPrefs);
            this.contourDisplay.paint(target, myPaintProps);
        }

        if (this.vectorDisplay != null) {
            for (GriddedVectorDisplay v : this.vectorDisplay) {
                v.setASync(!(dataManager
                        .getSpatialDisplayManager() instanceof OffscreenSpatialDisplayManager));
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
        int size = GFEPreference
                .getInt(parm.getParmID().compositeNameUI() + suffix);
        if (size == 0) {
            size = GFEPreference.getInt(key);
        }
        if (size == 0) {
            size = 60;
        }

        int offset = parm.getDisplayAttributes().getFontOffset()
                + GFEPreference.getInt("Contour_font");
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

        Set<VisualizationType> visTypes = new HashSet<>(
                parm.getDisplayAttributes()
                        .getVisualizationType(EditorType.SPATIAL, visMode));

        getCapability(ColorMapCapability.class).setSuppressingMenuItems(
                !visTypes.contains(VisualizationType.IMAGE) || parm
                        .getGridInfo().getGridType().equals(GridType.WEATHER));

        getCapability(ImagingCapability.class).setSuppressingMenuItems(
                !visTypes.contains(VisualizationType.IMAGE));
        if (CollectionUtils.containsAny(visTypes, DENSITY_TYPES)) {
            getCapabilities().addCapability(new DensityCapability(
                    (double) parm.getDisplayAttributes().getDensity(),
                    new double[] { -3, -2, -1, 0, 1, 2, 3 }));
        } else {
            getCapabilities().removeCapability(DensityCapability.class);
        }
        if (CollectionUtils.containsAny(visTypes, MAG_TYPES)) {
            getCapabilities().addCapability(new MagnificationCapability(
                    (double) parm.getDisplayAttributes().getFontOffset(),
                    new double[] { -2, -1, 0, 1, 2, 3 }));
        } else {
            getCapabilities().removeCapability(MagnificationCapability.class);
        }
        // NOTE: retainAll modifies visTypes so we do these last
        visTypes.retainAll(OUTLINE_TYPES);

        getCapability(OutlineCapability.class)
                .setSuppressingMenuItems(visTypes.isEmpty());

        getCapability(ColorableCapability.class)
                .setSuppressingMenuItems(visTypes.isEmpty());

    }

    @Override
    public DataTime[] getDataTimes() {
        IGridData[] data = parm.getGridInventory();

        this.dataTimes.clear();
        for (IGridData d : data) {
            TimeRange tr = d.getGridTime();
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(tr.getStart());
            this.dataTimes.add(new DataTime(cal, tr));
        }

        return this.dataTimes.toArray(new DataTime[this.dataTimes.size()]);
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        this.pixelCoverage = null;
    }

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
                    this.contourDisplay
                            .setOutlineWidth(outline.getOutlineWidth());
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

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {

        Parm activatedParm = this.dataManager.getSpatialDisplayManager()
                .getActivatedParm();

        if (this.parm != activatedParm) {
            return;
        }

        String[] tools = dataManager.getSmartToolInterface().listTools(parm);
        if (!GFEPreference.getBoolean("AllEditActionsOnPopUp", true)) {
            // Offer the user the tools in PopUpEditActions
            // But don't offer tools that aren't OK for the parm
            String[] putools = GFEPreference.getStringArray("PopUpEditActions");
            List<String> toolList = new ArrayList<>(Arrays.asList(tools));
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
            menuManager.add(new FuzzValueAction());
            break;

        case SCALAR:
            menuManager.add(new FuzzValueAction());
            break;
        default:
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

        @Override
        public String getText() {
            if (select) {
                return "Select Homogenous Area";
            }

            return "Deselect Contiguous Area";
        }

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

            if ((gridCoord.x < 0) || (gridCoord.y < 0)
                    || (gridCoord.x >= gridLocation.getNx())
                    || (gridCoord.y >= gridLocation.getNy())) {
                return;
            }

            final Point pt = new Point((int) Math.round(gridCoord.x),
                    (int) Math.round(gridCoord.y));

            BusyIndicator.showWhile(Display.getCurrent(), new Runnable() {

                @Override
                public void run() {
                    if (select) {

                        Grid2DBit gridCells = grid.getContiguousArea(
                                curTime.getValidPeriod().getStart(), pt);
                        ReferenceData refData = new ReferenceData(gridLocation,
                                new ReferenceID("contiguous"), gridCells);
                        dataManager.getRefManager().incomingRefSet(refData,
                                RefSetMode.USE_CURRENT);

                    } else {
                        // deselect

                        // Get the refSet and the contiguous area for this
                        // intCoord
                        final Grid2DBit refSet = dataManager.getRefManager()
                                .getActiveRefSet().getGrid();
                        Grid2DBit editInfluence = refSet.contiguousBitArray(pt);
                        if (editInfluence.isAnyBitsSet()) {
                            ReferenceData refData = new ReferenceData(
                                    gridLocation, new ReferenceID("contiguous"),
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

    @Override
    public boolean okToUnload() {
        if (parm.isModified()) {
            return false;
        }

        return super.okToUnload();
    }

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
    private void paintLabels(IGraphicsTarget target, IFont font, IGridData grid,
            float multiplier, Rectangle screenRect) {
        // Calculate the positions of where labels should go.
        // get a Point[] and we'll try to put a label at each
        // point returned.

        Grid2DByte byteData = null;
        if (grid instanceof DiscreteGridData) {
            byteData = ((DiscreteGridData) grid).getDataObject()
                    .getDiscreteGrid();
        } else if (grid instanceof WeatherGridData) {
            byteData = ((WeatherGridData) grid).getDataObject()
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
                DrawableString ds = new DrawableString(label,
                        parm.getDisplayAttributes().getBaseColor());
                ds.font = font;
                Rectangle2D labelExtent = target.getStringsBounds(ds);

                int xLabelGrid = (int) (labelExtent.getWidth() * multiplier)
                        + 1;
                int yLabelGrid = (int) (labelExtent.getHeight() * multiplier)
                        + 1;

                // now see if adjacent grid cells exist containing the
                // same data.
                if (((xGrid + xLabelGrid) > gridDim.x) || ((xGrid - 1) < 0)) {
                    continue;
                }
                if (((yGrid + yLabelGrid) > gridDim.y) || ((yGrid - 1) < 0)) {
                    continue;
                }

                byte weatherByteVal = byteData.get(xGrid, yGrid);

                boolean printLabel = true;
                for (int ii = xGrid - 1; ii < (xGrid + xLabelGrid); ii++) {
                    for (int j = yGrid - 1; j < (yGrid + yLabelGrid); j++) {
                        if (!parm.getDisplayAttributes().getDisplayMask()
                                .getAsBoolean(ii, j)
                                || (byteData.get(ii, j) != weatherByteVal)) {
                            printLabel = false;
                            break;
                        }
                    }
                }
                if (printLabel) {
                    ReferencedCoordinate c = new ReferencedCoordinate(
                            new Coordinate(xGrid, yGrid),
                            MapUtil.getGridGeometry(
                                    parm.getGridInfo().getGridLoc()),
                            Type.GRID_CENTER);
                    Coordinate coord = c.asPixel(descriptor.getGridGeometry());
                    ds.setCoordinates(coord.x, coord.y);
                    ds.horizontalAlignment = HorizontalAlignment.CENTER;
                    ds.verticallAlignment = VerticalAlignment.MIDDLE;
                    ds.rotation = 0.0;
                    target.drawStrings(ds);
                }
                printLabel = true;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error displaying label",
                        e);
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
        List<Point> points = new ArrayList<>();

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

        // grid cell indices where labels are attempted
        int xpos, ypos;
        int yCounter = 0;

        for (xpos = gridOffset; xpos < gridDim.x; xpos += gridInterval) {
            yCounter++;
            int xCounter = 0;
            for (ypos = (yCounter % 4) + 1; ypos < (gridDim.y
                    - gridOffset); ypos += gridInterval) {
                xCounter++;
                int xGrid = xpos + ((xCounter % 4) * gridOffset);
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

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowISCGridsMsg) {
            showIScGrid((ShowISCGridsMsg) message);
        }
    }

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
                iscParm.getListeners().addParmInventoryChangedListener(
                        this.parmInventoryChanged);
                iscParm.getListeners().addGridChangedListener(this.gridChanged);
            } else {
                iscParm.getListeners().removeParmInventoryChangedListener(
                        this.parmInventoryChanged);
                iscParm.getListeners()
                        .removeGridChangedListener(this.gridChanged);
            }
        }
    }
}
