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
package com.raytheon.viz.gfe.rsc.colorbar;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.actions.ChangeColorTableAction;
import com.raytheon.viz.gfe.actions.SetDiscretePickupAction;
import com.raytheon.viz.gfe.actions.SetDiscreteWxPickupAction;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.IDisplayModeChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.IPickupValueChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.EditorType;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.dialogs.SetDeltaDialog;
import com.raytheon.viz.gfe.dialogs.SetValueDialog;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.rsc.GFEResourceData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuProvider;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * A Colorbar resource that provides an interactive colormap
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/24/2008              chammack    Initial Creation.
 * 06/24/2008              ebabin      Added WX session menus.
 *                                      Moved fuzz value menu item to GFEResource.
 * 05Aug2008    #1405       ebabin      Fix fo delta not displaying after first use.
 * 06/03/2011   #8919      rferrel     No longer display color bar when
 *                                     VisMode is GRAPHIC
 * 11/13/20112  #1298      rferrel     Changes for non-blocking SetDeltaDialog.
 *                                     Changes for non-blocking SetValueDialog.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class GFEColorbarResource extends
        AbstractVizResource<GFEResourceData, IMapDescriptor> implements
        IContextMenuProvider, IPickupValueChangedListener,
        IDisplayModeChangedListener, IMessageClient,
        IDisplayedParmListChangedListener {

    public static final double HEIGHT = 25.0;

    private class GFEColorbarInputHandler extends InputAdapter {

        private boolean inDrag;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (mouseButton == 1 | mouseButton == 2) {
                com.raytheon.uf.viz.core.PixelExtent pe = getExtent();
                if (pe == null) {
                    return false;
                }

                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    double[] v = container.getActiveDisplayPane().screenToGrid(
                            x, y, 0);
                    if (pe.contains(v[0], v[1]) && isDisplaying()) {
                        this.inDrag = true;
                        setPickup(v, mouseButton);
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (inDrag) {
                com.raytheon.uf.viz.core.PixelExtent pe = getExtent();

                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    double[] v = container.getActiveDisplayPane().screenToGrid(
                            x, y, 0);
                    if (v[0] < pe.getMinX()) {
                        v[0] = pe.getMinX();
                    }
                    if (v[0] > pe.getMaxX()) {
                        v[0] = pe.getMaxX();
                    }
                    setPickup(v, mouseButton);
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (this.inDrag) {
                this.inDrag = false;
                return true;
            }
            return false;
        }

        private void setPickup(double[] v, int mouseButton) {
            WxValue val = colorbarDisplay.getValueAt(v, mouseButton);
            Parm parm = getParm();
            if (parm == null) {
                throw new IllegalStateException("Parm is null from colorbar");
            }

            parm.getParmState().setPickUpValue(val);
        }
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEColorbarResource.class);

    public static final RGB COLORBAR_GRAY = new RGB(192, 192, 192);

    private final DataManager dManager;

    private static DecimalFormat format = new DecimalFormat();

    private PixelExtent lastExtent;

    // the currently displayed parm
    private Parm currentParm;

    // the normal parm (not quick view)
    private Parm normalParm;

    private IFont colorbarScaleFont;

    private IFont colorbarWxLabelFont;

    private IFont pickupFont;

    private IColorBarDisplay colorbarDisplay;

    // for WX type
    private static final String SET_TO_RECENT_VALUES = "Set to Recent Values";

    private static final String SET_TO_SESSION_VALUES = "Set to Session Values";

    public static final String SET_TO_COMMON_VALUES = "Set to Common Values";

    // private int lastIndex;

    // private IGraphicsTarget target;

    private IInputHandler handler = new GFEColorbarInputHandler();

    protected static RGB seColorBarTickColor = new RGB(255, 255, 255);

    protected static RGB seColorBarTextColor = new RGB(255, 255, 255);

    protected static RGB seColorBarFgPickupColor = new RGB(255, 255, 255);

    protected static RGB seColorBarBgPickupColor = new RGB(0, 0, 0);

    protected static RGB seColorBarFgWxPickupColor = new RGB(255, 255, 255);

    protected static RGB seColorBarBgWxPickupColor = new RGB(255, 0, 255);

    AbstractGraphicsFactoryAdapter graphicsAdapter = null;

    protected Set<ParmID> fittedParms;

    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                IPreferenceStore prefs = Activator.getDefault()
                        .getPreferenceStore();
                String color = null;

                if (prefs.contains("SEColorBar_tickColor")) {
                    color = prefs.getString("SEColorBar_tickColor");
                    if (!color.equals("")) {
                        seColorBarTickColor = RGBColors.getRGBColor(color);
                    }
                }

                if (prefs.contains("SEColorBar_fgTextColor")) {
                    color = prefs.getString("SEColorBar_fgTextColor");
                    if (!color.equals("")) {
                        seColorBarTextColor = RGBColors.getRGBColor(color);
                    }
                }

                if (prefs.contains("SEColorBar_fgPickUpColor")) {
                    color = prefs.getString("SEColorBar_fgPickUpColor");
                    if (!color.equals("")) {
                        seColorBarFgPickupColor = RGBColors.getRGBColor(color);
                    }
                }

                if (prefs.contains("SEColorBar_bgPickUpColor")) {
                    color = prefs.getString("SEColorBar_bgPickUpColor");
                    if (!color.equals("")) {
                        seColorBarBgPickupColor = RGBColors.getRGBColor(color);
                    }
                }

                if (prefs.contains("SEColorBar_fgWxPickUpColor")) {
                    color = prefs.getString("SEColorBar_fgWxPickUpColor");
                    if (!color.equals("")) {
                        seColorBarFgWxPickupColor = RGBColors
                                .getRGBColor(color);
                    }
                }

                if (prefs.contains("SEColorBar_bgWxPickUpColor")) {
                    color = prefs.getString("SEColorBar_bgWxPickUpColor");
                    if (!color.equals("")) {
                        seColorBarBgWxPickupColor = RGBColors
                                .getRGBColor(color);
                    }
                }
            }
        }.run();
    }

    public GFEColorbarResource(DataManager dManager) {
        super(new GFEResourceData(), new LoadProperties());
        this.dManager = dManager;
        fittedParms = new HashSet<ParmID>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void disposeInternal() {
        if (currentParm != null) {
            currentParm.getListeners().removePickupValueChangedListener(this);
        }
        dManager.getParmManager().removeDisplayedParmListChangedListener(this);
        dManager.getSpatialDisplayManager().removeDisplayModeChangedListener(
                this);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(handler);
        }

        Message.unregisterInterest(this, ShowQuickViewDataMsg.class);

        if (colorbarScaleFont != null) {
            colorbarScaleFont.dispose();
        }

        if (colorbarWxLabelFont != null) {
            colorbarWxLabelFont.dispose();
        }

        if (pickupFont != null) {
            pickupFont.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        return "GFE Internal Colorbar Resource";
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
        dManager.getSpatialDisplayManager().addDisplayModeChangedListener(this);
        dManager.getParmManager().addDisplayedParmListChangedListener(this);

        Message.registerInterest(this, ShowQuickViewDataMsg.class);

        colorbarScaleFont = GFEFonts.makeGFEIFont(target, "ColorBarScale_font",
                1);
        colorbarWxLabelFont = GFEFonts.makeGFEIFont(target,
                "ColorBarWxLabel_font", 2);
        pickupFont = GFEFonts.makeGFEIFont(target, "ColorBarPickUp_font", 3);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(handler, InputPriority.PERSPECTIVE);
        }

        if (currentParm != null) {
            currentParm.getListeners().addPickupValueChangedListener(this);
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
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // this.target = target;

        IExtent screenExtent = paintProps.getView().getExtent();

        // Construct a bar that is HEIGHT pixels high
        double height = HEIGHT * screenExtent.getHeight()
                / paintProps.getCanvasBounds().height;

        PixelExtent pe = new PixelExtent(screenExtent.getMinX(),
                screenExtent.getMaxX(), screenExtent.getMinY(),
                screenExtent.getMinY() + height);

        // Set the extent of the colorbar back on the parent resource so it
        // knows the size for things like mouse actions
        setExtent(pe);

        target.clearClippingPlane();

        // erase the whole colorbar to a black background
        target.drawShadedRect(pe, ColorUtil.BLACK, 1.0, null);
        target.drawRect(pe, GFEColorbarResource.COLORBAR_GRAY, 2.0f, 1.0f);

        if (currentParm == null) {
            return;
        }

        if (/*
             * !currentParm.getDisplayAttributes().getVisMode().equals(VisMode.
             * GRAPHIC ) &&
             */(colorbarDisplay != null)) {
            colorbarDisplay.paint(target, paintProps);
        }

        if (graphicsAdapter == null) {
            graphicsAdapter = GraphicsFactory.getGraphicsAdapter();
        }

        target.setupClippingPlane(graphicsAdapter.constructExtent(descriptor
                .getGridGeometry().getGridRange()));

        // this.lastIndex = curIndex;

    }

    protected void setExtent(PixelExtent pixelExtent) {
        lastExtent = pixelExtent;
    }

    /**
     * Is a label within a certain threshold
     * 
     * @param startDA
     * @param endDA
     * @param pt
     * @param labelLength
     * @return
     */
    public static boolean isLabelWithin(double startDA, double endDA,
            double pt, double labelLength) {
        if ((pt - labelLength / 2 > startDA) && (pt + labelLength / 2 < endDA)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Determines whether or not whether a line between startBox and endBox is
     * within (even partly) startDA and endDA.
     * 
     * @param startDA
     *            The starting point of the line between startDA and endDA. Must
     *            be less than endDA.
     * @param endDA
     *            The ending point of the line between startDA and endDA. Must
     *            be greater than startDA.
     * @param startBox
     *            The starting point of the line between startBox and endBox.
     *            Must be less than endBox.
     * @param endBox
     *            The ending point of the line between startBox and endBox. Must
     *            be greater than startBox.
     * @return True, if any point in the line described by startBox and endBox
     *         is contained within the line described by startDA and endDA.
     *         Else, false.
     */
    public static boolean isWithin(double startDA, double endDA,
            double startBox, double endBox) {
        return (!((startBox > endDA) || (endBox < startDA)));
    }

    /**
     * Format a string
     * 
     * @param val
     * @param precision
     * @return
     */
    public static synchronized String formatString(float val, int precision) {
        format.setMinimumFractionDigits(precision);
        format.setMaximumFractionDigits(precision);
        format.setGroupingUsed(false);
        return format.format(val);
    }

    /**
     * Get the extent
     * 
     * @return the extent
     */
    public PixelExtent getExtent() {
        return lastExtent;
    }

    /**
     * Get the map descriptor
     * 
     * @return
     */
    public IMapDescriptor getMapDescriptor() {
        return descriptor;
    }

    public Parm getParm() {
        return currentParm;
    }

    public IFont getColorbarScaleFont() {
        return colorbarScaleFont;
    }

    public IFont getColorbarWxLabelFont() {
        return colorbarWxLabelFont;
    }

    public IFont getPickupValueFont() {
        return pickupFont;
    }

    public boolean isDisplaying() {
        if (currentParm != null) {
            return true;
        }

        return false;
    }

    private void showDeltaDialog() {
        SetDeltaDialog.openDialog();
    }

    private void showPickupDialog() {
        SetValueDialog.openDialog();
    }

    private class SetPickupAction extends AbstractRightClickAction {

        public SetPickupAction() {
            super("Set Pickup Value...");

        }

        @Override
        public void run() {
            showPickupDialog();
        }
    }

    private class SetDeltaAction extends AbstractRightClickAction {

        public SetDeltaAction() {
            super("Set Delta Value...");

        }

        @Override
        public void run() {
            showDeltaDialog();
        }
    }

    @Override
    public void pickupValueChanged(Parm parm, WxValue pickupValue) {
        issueRefresh();
    }

    @Override
    public void displayModeChanged(EditorType editorType, Parm newParm,
            VisMode visMode) {
        if (!editorType.equals(EditorType.SPATIAL)) {
            return;
        }

        if (visMode.equals(VisMode.GRAPHIC)) {
            newParm = null;
        } else if (!visMode.equals(VisMode.IMAGE)) {
            return;
        }

        normalParm = newParm;
        updateColorbar(newParm);
    }

    protected void updateColorbar(Parm newParm) {
        if (currentParm != newParm) {
            if (currentParm != null) {
                currentParm.getListeners().removePickupValueChangedListener(
                        this);
            }

            if (colorbarDisplay != null) {
                colorbarDisplay.dispose();
                colorbarDisplay = null;
            }

            currentParm = newParm;

            if (newParm == null) {
                getProperties().setVisible(false);
                issueRefresh();
                return;
            }

            newParm.getListeners().addPickupValueChangedListener(this);

            // Construct a new colorbar

            switch (newParm.getGridInfo().getGridType()) {
            case SCALAR:
            case VECTOR:
                if (!fittedParms.contains(currentParm.getParmID())) {
                    checkFitToData();
                    fittedParms.add(currentParm.getParmID());
                }
                colorbarDisplay = new ContinuousColorbar(dManager, newParm,
                        this);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarFgPickupColor(seColorBarFgPickupColor);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarBgPickupColor(seColorBarBgPickupColor);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarFgWxPickupColor(seColorBarFgWxPickupColor);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarBgWxPickupColor(seColorBarBgWxPickupColor);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarTextColor(seColorBarTextColor);
                ((ContinuousColorbar) colorbarDisplay)
                        .setSeColorBarTickColor(seColorBarTickColor);
                break;
            case DISCRETE:
            case WEATHER:
                colorbarDisplay = new DiscreteColorbar(newParm, this);
                ((DiscreteColorbar) colorbarDisplay)
                        .setSeColorBarFgPickupColor(seColorBarFgPickupColor);
                ((DiscreteColorbar) colorbarDisplay)
                        .setSeColorBarFgWxPickupColor(seColorBarFgWxPickupColor);
                ((DiscreteColorbar) colorbarDisplay)
                        .setSeColorBarBgWxPickupColor(seColorBarBgWxPickupColor);
                ((DiscreteColorbar) colorbarDisplay)
                        .setSeColorBarTextColor(seColorBarTextColor);
                ((DiscreteColorbar) colorbarDisplay)
                        .setSeColorBarTickColor(seColorBarTickColor);
                break;
            default:
                statusHandler.handle(Priority.PROBLEM,
                        "Unhandled type in colorbar: "
                                + newParm.getGridInfo().getGridType());

            }

            getProperties().setVisible(true);
            issueRefresh();
        }
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
        if (message instanceof ShowQuickViewDataMsg) {
            Parm parm = normalParm;
            GridID gridId = ((ShowQuickViewDataMsg) message).getGridId();

            if (gridId != null) {
                parm = gridId.getParm();
            }
            updateColorbar(parm);
        }
    }

    /**
     * See if the parm has a fit to data color table entry in the config file.
     * If it does, invoke fitToData() in the appropriate mode to set the
     * continuous colorbar range.
     */
    protected void checkFitToData() {
        String parmName = currentParm.getParmID().getParmName();
        String fitToDataPref = parmName + "_fitToDataColorTable";
        if (GFEPreference.contains(fitToDataPref)) {
            String ftdv = GFEPreference.getPreference(fitToDataPref);

            if ("none".equalsIgnoreCase(ftdv)) {
                ; // do nothing
            } else {
                ReferenceData refData = null;
                Date gridTime = null;
                GridID gid = null;
                FitToData fitToData = new FitToData(dManager, currentParm);
                try {
                    if ("all grids".equalsIgnoreCase(ftdv)) {
                        fitToData.fitToData();
                    } else if ("all grids over area".equalsIgnoreCase(ftdv)) {
                        refData = dManager.getRefManager().getActiveRefSet();
                        fitToData.fitToData(refData);
                    } else if ("single grid".equalsIgnoreCase(ftdv)) {
                        gridTime = dManager.getSpatialDisplayManager()
                                .getSpatialEditorTime();
                        gid = new GridID(currentParm, gridTime);
                        fitToData.fitToData(gid);
                    } else if ("single grid over area".equalsIgnoreCase(ftdv)) {
                        gridTime = dManager.getSpatialDisplayManager()
                                .getSpatialEditorTime();
                        gid = new GridID(currentParm, gridTime);
                        // AWIPS I ignores edit area for this branch
                        // refData =
                        // dManager.getRefManager().getActiveRefSet();
                        // fitToData.fitToData(gid, refData);
                        fitToData.fitToData(gid);
                    } else {
                        statusHandler.handle(Priority.PROBLEM, String.format(
                                "Unknown value for %s (\"%s\")--ignored.",
                                fitToDataPref, ftdv));

                    }
                } catch (GFEOperationFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);

                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuProvider#addContextMenuItems(org
     * .eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void provideContextMenuItems(IMenuManager menuManager, int x, int y) {
        IDisplayPane displayPane = getResourceContainer()
                .getActiveDisplayPane();
        org.eclipse.swt.graphics.Rectangle bounds = displayPane.getBounds();
        IExtent extent = displayPane.getRenderableDisplay().getExtent();
        double correctedX = x * (extent.getMaxX() - extent.getMinX())
                / bounds.width + extent.getMinX();
        double correctedY = y * (extent.getMaxY() - extent.getMinY())
                / bounds.height + extent.getMinY();

        if (lastExtent != null) {
            if (lastExtent.contains(correctedX, correctedY)) {
                GridType type = getParm().getGridInfo().getGridType();
                if (GridType.WEATHER.equals(type)
                        || GridType.DISCRETE.equals(type)) {
                    Parm parm = getParm();
                    ParmState parmState = parm.getParmState();
                    menuManager.add(new SetPickupAction());

                    if (!parmState.getRecentPickupValues().isEmpty()) {
                        menuManager.add(new SetDiscreteWxPickupAction(
                                SET_TO_RECENT_VALUES, parm, parmState
                                        .getRecentPickupValues().toArray(
                                                new WxValue[0])));
                    }
                    if (!parmState.getSessionPickupValues().isEmpty()) {
                        menuManager.add(new SetDiscreteWxPickupAction(
                                SET_TO_SESSION_VALUES, parm, parmState
                                        .getSessionPickupValues().toArray(
                                                new WxValue[0])));

                    }

                    String compName = parm.getParmID().compositeNameUI();
                    String[] commonItems = GFEPreference
                            .getArrayPreference(compName + "_commonValues");

                    if ((commonItems != null) && (commonItems.length > 0)) {
                        menuManager.add(new SetDiscretePickupAction(
                                SET_TO_COMMON_VALUES, commonItems, parm));
                    }
                    ISpatialDisplayManager spatialMgr = dManager
                            .getSpatialDisplayManager();
                    ResourcePair pair = spatialMgr.getResourcePair(currentParm);
                    ChangeColorTableAction ccta = new ChangeColorTableAction(
                            currentParm);
                    ccta.setSelectedRsc(pair);
                    menuManager.add(ccta);
                } else {
                    ISpatialDisplayManager spatialMgr = dManager
                            .getSpatialDisplayManager();
                    ResourcePair pair = spatialMgr.getResourcePair(currentParm);

                    menuManager.add(new FitToDataAction(dManager, currentParm));
                    FullDefaultRangeAction fdra = new FullDefaultRangeAction(
                            currentParm);
                    fdra.setSelectedRsc(pair);
                    menuManager.add(fdra);
                    ChangeColorMapRangeAction ccra = new ChangeColorMapRangeAction(
                            currentParm);
                    ccra.setSelectedRsc(pair);
                    ccra.setContainer(getResourceContainer());
                    menuManager.add(ccra);
                    menuManager.add(new SetPickupAction());
                    menuManager.add(new SetDeltaAction());
                    ChangeColorTableAction ccta = new ChangeColorTableAction(
                            currentParm);
                    ccta.setSelectedRsc(pair);
                    ccta.setContainer(this.getResourceContainer());
                    menuManager.add(ccta);
                }
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener#
     * displayedParmListChanged(com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[])
     */
    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        if (!Arrays.asList(parms).contains(normalParm)) {
            normalParm = null;
            updateColorbar(normalParm);
        }
    }
}
