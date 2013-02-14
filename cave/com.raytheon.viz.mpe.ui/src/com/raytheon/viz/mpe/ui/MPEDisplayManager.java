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
package com.raytheon.viz.mpe.ui;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.FileImageOutputStream;

import org.eclipse.core.commands.Command;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.RadioState;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwresult;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.mpe.util.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.hydrocommon.actions.SetProjection;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEProcessGrib;
import com.raytheon.viz.mpe.ui.dialogs.ChooseDataPeriodDialog;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.GageTableDlg;
import com.raytheon.viz.mpe.ui.dialogs.polygon.PolygonDataManager;
import com.raytheon.viz.mpe.ui.perspective.MPEPerspective;
import com.raytheon.viz.mpe.ui.rsc.DisplayMeanArealPrecipResource;
import com.raytheon.viz.mpe.ui.rsc.MPEGageResource;
import com.raytheon.viz.mpe.ui.rsc.MPEPolygonResource;
import com.raytheon.viz.mpe.ui.rsc.TimeLapseResource;
import com.raytheon.viz.mpe.ui.rsc.TimeLapseResourceData;
import com.raytheon.viz.mpe.ui.rsc.XmrgResource;
import com.raytheon.viz.mpe.ui.rsc.XmrgResourceData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * The MPE Display Manager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2008            randerso     Initial creation
 * Dec 1, 2008  1748      snaples      Added enum for Gage Color
 * Aug 11, 2010  5733	  jwang        Removed Polygon Edit and Gages
 * 									   legend from MPE perspective
 * Aug 8, 2012   15271	  snaples      Updated hourly slot
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MPEDisplayManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEDisplayManager.class);

    public static enum DisplayMode {
        Image, Contour
    };

    public static enum GageDisplay {
        Ids, Values, Triangles
    };

    public static enum GageColor {
        Solid, Contrast, ByQC, ByValue
    }

    public static enum GageMissingOptions {
        MissingNone, MissingReported, MissingAll
    }

    public static final String APPLICATION_NAME = "hmapmpe";

    private static final String PROCESS_FLAG_LOCAL = "MPM01   ";

    private static final String PROCESS_FLAG_SBN = "QPE01   ";

    private static final Cursor CURSOR_WAIT = new Cursor(Display.getDefault(),
            SWT.CURSOR_WAIT);

    private static final List<NamedColorUseSet> pColorSetGroup = MPEColors
            .build_mpe_colors();

    private static final Map<IRenderableDisplay, MPEDisplayManager> instanceMap = new HashMap<IRenderableDisplay, MPEDisplayManager>();

    private static AppsDefaults appsDefaults;

    private static final SimpleDateFormat yyyyMMddHH = new SimpleDateFormat(
            "yyyyMMddHH");

    private static final SimpleDateFormat MMddyyyyHH = new SimpleDateFormat(
            "MMddyyyyHH");

    private static final SimpleDateFormat OBS_TIME_SDF = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private static SimpleDateFormat ST3_SDF;

    /* Define the MPE product generation rules. */
    public static final String[] mpe_qpe_fields = { "rMosaic", "avgrMosaic",
            "maxrMosaic", "bMosaic", "lMosaic", "gageOnly", "mMosaic",
            "mlMosaic", "satPre", "lsatPre", "srMosaic", "sgMosaic",
            "srgMosaic", "p3lMosaic", "Xmrg", "rfcMosaic", "rfcbMosaic",
            "rfcmMosaic", "qmosaic", "lqmosaic", "mlqmosaic", "localField1",
            "localField2", "localField3" };

    /** pcpn time step */
    public static int pcpn_time_step = -1;

    /** Flag for new area */
    public static int new_area_flag = 0;

    private final Command fontCommand;

    private boolean dataSaved;

    /** QPF Flag **/
    private boolean qpf = false;

    /** Freezing Flag **/
    private boolean zflag = false;

    /** MaxMin Flag **/
    private boolean maxmin = false;

    /** Group Edit Flag **/
    private boolean groupedt = false;

    /** Hours before current time for time lapse mode */
    private int timeLapseHours = 0;

    /** Flag for time lapse mode or not */
    private boolean timeLapseMode = false;

    /** The time lapse resource */
    private TimeLapseResource timeLapseRsc = null;

    private GageTableDlg gageTableDlg = null;

    /**
     * The previously displayed resource.
     */
    private AbstractVizResource<?, ?> previousResource = null;

    private final IRenderableDisplay display;

    private Date currentDate;

    private DisplayFieldData displayFieldType;

    private final Set<DisplayMode> displayMode;

    private final Set<GageDisplay> gageDisplay;

    private GageColor gageColor;

    private GageMissingOptions gageMissing;

    private final boolean gageTriangles = false;

    private XmrgResource xmrgRsc;

    private AbstractVizResource<?, ?> displayedResource;

    private boolean mpeDqcOption = false;

    private boolean hrFirstTime = true;

    private int accum_interval = 1;

    private DisplayFieldData otherDispType = null;

    private int dqcDays = Integer.parseInt(appsDefaults
            .getToken("mpe_dqc_num_days"));

    /**
     * Get the MPEDisplayManager instance given the pane
     * 
     * @param pane
     * @return
     */
    public static synchronized MPEDisplayManager getInstance(IDisplayPane pane) {
        if (pane == null) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                pane = container.getActiveDisplayPane();
            }
        }

        if (pane == null) {
            return null;
        }

        return getInstance(pane.getRenderableDisplay());
    }

    /**
     * Get the MPEDisplayManager instance given the display
     * 
     * @param pane
     *            the display to get the instance for
     * @return the instance
     */
    public static synchronized MPEDisplayManager getInstance(
            IRenderableDisplay display) {
        if (display == null) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                IDisplayPane pane = container.getActiveDisplayPane();
                if (pane != null) {
                    display = pane.getRenderableDisplay();
                }
            }
        }

        if (display == null) {
            return null;
        }

        MPEDisplayManager instance = instanceMap.get(display);

        if (appsDefaults == null) {
            appsDefaults = AppsDefaults.getInstance();
            yyyyMMddHH.setTimeZone(TimeZone.getTimeZone("GMT"));
            MMddyyyyHH.setTimeZone(TimeZone.getTimeZone("GMT"));
            OBS_TIME_SDF.setTimeZone(TimeZone.getTimeZone("GMT"));

            String date_form = appsDefaults.getToken("st3_date_form");
            if ((date_form == null) || date_form.isEmpty()
                    || date_form.equals("mdY")) {
                ST3_SDF = new SimpleDateFormat("MMddyyyyHH");
                ST3_SDF.setTimeZone(TimeZone.getTimeZone("GMT"));
            } else {
                ST3_SDF = yyyyMMddHH;
            }
        }

        if (instance == null) {
            instance = new MPEDisplayManager(display);
            instanceMap.put(display, instance);
        }
        return instance;
    }

    /**
     * Get the current display manager on the active editor
     * 
     * @return
     */
    public static MPEDisplayManager getCurrent() {
        MPEDisplayManager instance = null;
        IEditorPart editor = EditorUtil.getActiveEditor();
        if ((editor != null) && (editor instanceof IDisplayPaneContainer)) {
            IDisplayPane pane = null;
            if (editor instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
                if (multiPane.displayedPaneCount() > 1) {
                    pane = multiPane
                            .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                }
            }

            if (pane == null) {
                pane = ((IDisplayPaneContainer) editor).getActiveDisplayPane();
            }
            instance = lookupManager(pane);
        }
        return instance;
    }

    /**
     * TODO: I don't believe depopulating is needed anymore, only populating
     * when the perspective is *first* opened, look into this
     * 
     * Depopulate the MPE perspective on the active window
     * 
     * @throws VizException
     */
    public static void dePopulate(IWorkbenchWindow window) throws VizException {
        AbstractEditor[] editors = UiUtil.getEditors(window,
                MPEPerspective.ID_PERSPECTIVE);

        for (AbstractEditor editor : editors) {
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                MPEDisplayManager mgr = lookupManager(pane);
                if (mgr != null) {
                    mgr.dePopulateInternal();
                }
            }
            editor.refresh();
        }
    }

    /**
     * Populate mpe on the given window
     * 
     * @param window
     * @param pro
     * @param first
     * @throws VizException
     */
    public static void populate(IWorkbenchWindow window, boolean pro,
            boolean first) throws VizException {
        AbstractEditor[] editors = UiUtil.getEditors(window,
                MPEPerspective.ID_PERSPECTIVE);
        for (AbstractEditor editor : editors) {

            // TODO: set the current projection instead of the default
            if (pro == true) {
                SetProjection.setDefaultProjection(editor, "mpe");
            }
            editor.refresh();
        }
        if (first == true) {
            Shell shell = window.getShell();
            ChooseDataPeriodDialog dialog = new ChooseDataPeriodDialog(shell);
            dialog.open();
        }
    }

    /**
     * Populate mpe on the given window
     * 
     * @param window
     * @param pro
     * @throws VizException
     */
    public static void populate(IWorkbenchWindow window, boolean pro)
            throws VizException {
        populate(window, pro, false);
    }

    /**
     * Look up the display manager for the given pane
     * 
     * @param pane
     * @return
     */
    private static MPEDisplayManager lookupManager(IDisplayPane pane) {
        if (pane != null) {
            return instanceMap.get(pane.getRenderableDisplay());
        }
        return null;
    }

    private MPEDisplayManager(IRenderableDisplay display) {
        this.display = display;
        fontCommand = ((ICommandService) VizWorkbenchManager.getInstance()
                .getCurrentWindow().getService(ICommandService.class))
                .getCommand("com.raytheon.viz.mpe.ui.actions.setfont");
        Command colorCommand = ((ICommandService) VizWorkbenchManager
                .getInstance().getCurrentWindow()
                .getService(ICommandService.class))
                .getCommand("com.raytheon.viz.mpe.ui.actions.toggleGageColor");
        // dataSaved = true;
        currentDate = MPEDataManager.getInstance().getLatestDate();
        displayFieldType = DisplayFieldData.mMosaic;
        displayMode = EnumSet.noneOf(DisplayMode.class);
        gageDisplay = EnumSet.noneOf(GageDisplay.class);
        gageMissing = GageMissingOptions.MissingNone;
        gageColor = GageColor.Solid;

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                MPEDisplayManager.this.toggleDisplayMode(DisplayMode.Image);
                MPEDisplayManager.this.toggleGageMissing(gageMissing);
            }
        });
    }

    /**
     * @return the fontSize
     */
    public String getFontState() {
        return String.valueOf(fontCommand.getState(RadioState.STATE_ID)
                .getValue());
    }

    /**
     * @return the qpf
     */
    public boolean isQpf() {
        return qpf;
    }

    /**
     * @param qpf
     *            the qpf to set
     */
    public void setQpf(boolean qpf) {
        this.qpf = qpf;
    }

    /**
     * @return the groupedt
     */
    public boolean isGroupedt() {
        return groupedt;
    }

    /**
     * @param groupedt
     *            the groupedt to set
     */
    public void setGroupedt(boolean groupedt) {
        this.groupedt = groupedt;
    }

    /**
     * @return the zflag
     */
    public boolean isZflag() {
        return zflag;
    }

    /**
     * @param zflag
     *            the zflag to set
     */
    public void setZflag(boolean zflag) {
        this.zflag = zflag;
    }

    /**
     * @return the maxmin
     */
    public boolean isMaxmin() {
        return maxmin;
    }

    /**
     * @param maxmin
     *            the maxmin to set
     */
    public void setMaxmin(boolean maxmin) {
        this.maxmin = maxmin;
    }

    public IRenderableDisplay getRenderableDisplay() {
        return display;
    }

    public Date getCurrentDate() {
        return currentDate;
    }

    public void setCurrentDate(Date newDate) {
        MPEDataManager dm = MPEDataManager.getInstance();

        // check for date in valid range
        if (newDate.before(dm.getEarliestDate())
                || newDate.after(dm.getLatestDate())) {
            return;
        }

        if (!isDataSaved()) {
            if (!okToProceed()) {
                return;
            }
        } else {
            // if saved, then reset to false since it isn't saved for the next
            // time
            setDataSaved(false);
        }

        currentDate = newDate;
        ChangeTimeProvider.update();

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        Cursor prev = shell.getCursor();
        shell.setCursor(CURSOR_WAIT);
        clearMPEData();
        try {
            dm.readRadarData(newDate);
            dm.readGageData(newDate, newDate);

            if (gageTableDlg != null) {
                gageTableDlg.updateDate(newDate);
            }

            setOtherDispType(displayFieldType);
            display_mpe_data(displayFieldType);

        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM, "Unexpected error", e);
        } finally {
            shell.setCursor(prev);
        }
    }

    public boolean okToProceed() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        return MessageDialog.openConfirm(shell, "Data not Saved",
                "Data Not Saved - OK to Proceed?");
    }

    private void display_mpe_data(DisplayFieldData dataType)
            throws VizException {
        switch (dataType) {
        case avgrMosaic:
        case bMosaic:
        case gageOnly:
        case Height:
        case Index:
        case lMosaic:
        case Locbias:
        case Locspan:
        case satPre:
        case lsatPre:
        case localField1:
        case localField2:
        case localField3:
        case maxrMosaic:
        case mlMosaic:
        case mMosaic:
        case p3lMosaic:
        case rfcbMosaic:
        case rfcmMosaic:
        case rMosaic:
        case sgMosaic:
        case srgMosaic:
        case srMosaic:
        case qmosaic:
        case lqmosaic:
        case mlqmosaic: {
            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = FileUtil.join(dirname,
                    cv_use + yyyyMMddHH.format(getCurrentDate()) + "z");
            // System.out.println(fname);
            display_field(fname);
            break;
        }

        case Xmrg: {
            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = FileUtil.join(dirname, cv_use.toLowerCase()
                    + MMddyyyyHH.format(getCurrentDate()) + "z");

            display_field(fname);
            break;
        }
        case Prism:
            display_prism(dataType, "mean_precip");
            break;

        case maxtempPrism:
            display_prism(dataType, "max_temp");
            break;

        case mintempPrism:
            display_prism(dataType, "min_temp");
            break;

        case rfcMosaic:
            String cv_use = dataType.getCv_use();
            String dirname = appsDefaults.getToken(dataType.getDirToken());
            String fname = FileUtil.join(dirname,
                    cv_use + "01" + yyyyMMddHH.format(getCurrentDate()) + "z");

            display_field(fname);
            break;

        default:
            statusHandler.handle(Priority.PROBLEM,
                    "In routine display_mpe_data: Unrecognized MPE display type: "
                            + dataType.name());
            break;
        }
    }

    private void display_prism(DisplayFieldData dataType, String prismType)
            throws VizException {
        String dirname = appsDefaults.getToken(dataType.getDirToken());
        String mpe_site_id = appsDefaults.getToken("mpe_site_id");

        SimpleDateFormat sdf = new SimpleDateFormat("MMM");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String month_name = sdf.format(getCurrentDate()).toLowerCase();

        String fname = FileUtil.join(dirname, "prism_" + prismType + "_"
                + mpe_site_id + "_" + month_name);

        display_field(fname);
    }

    private void display_field(String fname) {
        // get colors/levels for appropriate field
        IDescriptor descriptor = display.getDescriptor();

        MPEDisplayManager displayManager = MPEDisplayManager.getCurrent();

        clearMPEData();

        String user_id = System.getProperty("user.name");
        String app_name = APPLICATION_NAME;

        List<Colorvalue> pColorSet = GetColorValues.get_colorvalues(user_id,
                app_name, displayFieldType.getCv_use(),
                accum_interval * 60 * 60, "E", pColorSetGroup);
        // displayFieldType.getCv_duration(), "E", pColorSetGroup);

        switch (displayFieldType) {
        case rMosaic:
        case avgrMosaic:
        case maxrMosaic:
        case bMosaic:
        case lMosaic:
        case localField1:
        case localField2:
        case localField3:
        case mMosaic:
        case mlMosaic:
        case p3lMosaic:
        case rfcMosaic:
        case Xmrg:
        case gageOnly:
        case subValue:
        case Height:
        case Index:
        case Locspan:
        case Locbias:
        case satPre:
        case lsatPre:
        case srMosaic:
        case sgMosaic:
        case srgMosaic:
        case rfcbMosaic:
        case rfcmMosaic:
        case Prism:
        case mintempPrism:
        case maxtempPrism:
        case qmosaic:
        case lqmosaic:
        case mlqmosaic:
            ResourcePair rp = new ResourcePair();
            XmrgFile xmrg = new XmrgFile(fname);
            XmrgResourceData xmrgRscData = new XmrgResourceData(this,
                    displayFieldType, xmrg, pColorSet);
            rp.setResourceData(xmrgRscData);
            descriptor.getResourceList().add(rp);
            descriptor.getResourceList().instantiateResources(descriptor, true);
            xmrgRsc = (XmrgResource) rp.getResource();
            displayedResource = xmrgRsc;

            PolygonDataManager polyManager = PolygonDataManager.getInstance();
            polyManager.getPolygons(displayManager.getDisplayFieldType(),
                    displayManager.getCurrentDate());

            List<MPEGageResource> rscs = descriptor.getResourceList()
                    .getResourcesByTypeAsType(MPEGageResource.class);
            for (MPEGageResource rsc : rscs) {
                rsc.reloadGages();
            }
            break;

        default:
            statusHandler.error(displayFieldType.name() + "is not handled");
        }

        display.getContainer().refresh();
    }

    /**
     * Display the time lapse
     */
    public void displayTimeLapse() {
        IDescriptor descriptor = display.getDescriptor();
        if (descriptor.getResourceList().containsRsc(timeLapseRsc)) {
            descriptor.getResourceList().removeRsc(timeLapseRsc);
            timeLapseRsc.dispose();
        }
        String user_id = System.getProperty("user.name");
        String app_name = APPLICATION_NAME;

        List<Colorvalue> pColorSet = GetColorValues.get_colorvalues(user_id,
                app_name, displayFieldType.getCv_use(),
                displayFieldType.getCv_duration(), "E", pColorSetGroup);

        previousResource = getDisplayedResource();
        TimeLapseResourceData rdata = new TimeLapseResourceData(
                displayFieldType, pColorSet);

        ResourcePair rp = new ResourcePair();
        rp.setResourceData(rdata);
        ResourceList resourceList = descriptor.getResourceList();
        resourceList.add(rp);
        descriptor.getResourceList().instantiateResources(descriptor, true);
        timeLapseRsc = (TimeLapseResource) rp.getResource();

        descriptor.setNumberOfFrames(getTimeLapseHours());
        MPEDisplayManager.getCurrent().setDisplayedResource(timeLapseRsc);
        try {
            descriptor
                    .setFramesInfo(new FramesInfo(timeLapseRsc.getDataTimes()));
            descriptor.redoTimeMatching();
        } catch (VizException e) {
            statusHandler.error("Error while redoing Time Matching ", e);
        }
    }

    public DisplayFieldData getDisplayFieldType() {
        return displayFieldType;
    }

    public void setDisplayFieldType(DisplayFieldData displayFieldType) {
        this.displayFieldType = displayFieldType;
        try {
            display_mpe_data(displayFieldType);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * This function removes the displayed resource as well as any XmrgResource
     * and DisplayMeanArealPrecipResource loaded
     */
    public void clearMPEData() {
        if (displayedResource != null) {
            IDescriptor descriptor = display.getDescriptor();
            Iterator<?> it = descriptor.getResourceList().iterator();

            while (it.hasNext()) {
                ResourcePair rp = (ResourcePair) it.next();
                if (rp.getResource() instanceof XmrgResource) {
                    descriptor.getResourceList().removeRsc(rp.getResource());
                    rp.getResource().dispose();
                    break;
                }
            }
            if (descriptor.getResourceList().containsRsc(displayedResource)) {
                descriptor.getResourceList().removeRsc(displayedResource);
            }

            it = descriptor.getResourceList().iterator();
            while (it.hasNext()) {
                ResourcePair rp = (ResourcePair) it.next();
                if (rp.getResource() instanceof DisplayMeanArealPrecipResource) {
                    descriptor.getResourceList().removeRsc(rp.getResource());
                    rp.getResource().dispose();
                    break;
                }
            }
        }
        displayedResource = null;
        // dataSaved = true;
    }

    /**
     * @return the displayMode
     */
    public Set<DisplayMode> getDisplayMode() {
        return displayMode;
    }

    /**
     * @param displayMode
     *            the displayMode to set
     */
    public void toggleDisplayMode(DisplayMode displayMode) {
        if (this.displayMode.contains(displayMode)) {
            this.displayMode.remove(displayMode);
        } else {
            this.displayMode.add(displayMode);
        }

        final ICommandService service = (ICommandService) PlatformUI
                .getWorkbench().getService(ICommandService.class);

        service.refreshElements(
                "com.raytheon.viz.mpe.ui.actions.toggleDisplayMode", null);

    }

    /**
     * @return the gageDisplay
     */
    public Set<GageDisplay> getGageDisplay() {
        return gageDisplay;
    }

    /**
     * @return the gageColor
     */
    public GageColor getGageColor() {
        return gageColor;
    }

    /**
     * @return the gageMissing
     */
    public GageMissingOptions getGageMissing() {
        return gageMissing;
    }

    /**
     * @param gageDisplay
     *            the gageDisplay to set
     */
    public void toggleGageDisplay(GageDisplay gageDisplay) {
        if (this.gageDisplay.contains(gageDisplay)) {
            this.gageDisplay.remove(gageDisplay);
        } else {
            this.gageDisplay.add(gageDisplay);
        }
        display.refresh();
    }

    /**
     * 
     * @param miss
     */
    public void toggleGageMissing(GageMissingOptions miss) {
        gageMissing = miss;

        final ICommandService service = (ICommandService) PlatformUI
                .getWorkbench().getService(ICommandService.class);

        service.refreshElements(
                "com.raytheon.viz.mpe.ui.actions.toggleGageMissing", null);
    }

    /**
     * 
     * @param gc
     */
    public void setGageColor(GageColor gc) {
        gageColor = gc;

        final ICommandService service = (ICommandService) PlatformUI
                .getWorkbench().getService(ICommandService.class);

        service.refreshElements(
                "com.raytheon.viz.mpe.ui.actions.toggleGageColor", null);
    }

    /**
     * @return the displayedResource
     */
    public AbstractVizResource<?, ?> getDisplayedResource() {
        return displayedResource;
    }

    public void setDisplayedResource(AbstractVizResource<?, ?> rsc) {
        displayedResource = rsc;
    }

    /**
     * @return the mpeDqcOption
     */
    public boolean isMpeQcOptionEnabled() {
        if (appsDefaults.getToken("mpe_dqc_options").equalsIgnoreCase("on")) {
            mpeDqcOption = true;
        }
        return mpeDqcOption;
    }

    /**
     * @return the hrFirstTime
     */
    public boolean isHrFirstTime() {
        return hrFirstTime;
    }

    /**
     * @param hrFirstTime
     *            the hrFirstTime to set
     */
    public void setHrFirstTime(boolean hrFTime) {
        hrFirstTime = hrFTime;
    }

    /**
     * @return the dqcDays
     */
    public int getDqcDays() {
        return dqcDays;
    }

    /**
     * @param dqcDays
     *            the dqcDays to set
     */
    public void setDqcDays(int qcDays) {
        dqcDays = qcDays;
        MPEDataManager.getInstance().setDQCDays(dqcDays);
    }

    /**
     * @return the gageTriangles
     */
    public boolean isGageTriangles() {
        return gageTriangles;
    }

    /**
     * @return colormap for gages
     */
    public List<Colorvalue> getGageColorMap() {
        String user_id = System.getProperty("user.name");
        String app_name = APPLICATION_NAME;
        List<Colorvalue> gColorSet = GetColorValues.get_colorvalues(user_id,
                app_name, displayFieldType.getCv_use(),
                displayFieldType.getCv_duration(), "E", pColorSetGroup);
        return gColorSet;
    }

    private void dePopulateInternal() throws VizException {

    }

    /**
     * @return the dataSaved
     */
    public boolean isDataSaved() {
        return dataSaved;
    }

    public void setDataSaved(boolean save) {
        dataSaved = save;
    }

    public void save_rfcwide() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        Cursor cursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);
        Cursor prevCursor = shell.getCursor();
        shell.setCursor(cursor);

        /*----------------------------------------------------------*/
        /* create date in desired format for use in xmrg filename */
        /*----------------------------------------------------------*/
        String cdate = ST3_SDF.format(currentDate);

        /*----------------------------------------------------------*/
        /* create full pathname of binary file (input to MAPX) */
        /* filename_xmrg is used by grib encoder */
        /*----------------------------------------------------------*/
        String fileName = String.format("%s/xmrg%sz",
                appsDefaults.getToken("rfcwide_xmrg_dir"), cdate);

        String fileNameXmrg = String.format("xmrg%sz", cdate);

        /*----------------------------------------------------------*/
        /* update RWResult table */
        /* write merged (mosaicked) field to file */
        /* replace missing values with 0.0 */
        /*----------------------------------------------------------*/

        String rfc = MPEDataManager.getInstance().getRFC();

        update_rwr_save(rfc, currentDate, displayFieldType.cv_use);

        save_merged_RFCW(fileName, PROCESS_FLAG_LOCAL);

        /*
         * Is the mpe_send_qpe_to_sbn token set to ON? If so create a second QPE
         * file with the proc_flag set to QPE01.
         */
        if (appsDefaults.getToken("mpe_send_qpe_to_sbn").equalsIgnoreCase("On")) {
            String filename = String.format("%s/xmrg%sz",
                    appsDefaults.getToken("mpe_qpe_sbn_dir"), cdate);
            save_merged_RFCW(filename, PROCESS_FLAG_SBN);
        }

        /*-----------------------------------------------------------*/
        /* generate and save files depending on values of */
        /* mpe_save_... tokens read from .Apps_defaults */
        /*                                                           */
        /* create filenames for netCDF, gif, grib and jpeg files */
        /* if mpe_xxxx_id token not found or blank, then no string */
        /* is prepended to filename */
        /* in all cases, filenames contain date in form yyyymmddhh */
        /*-----------------------------------------------------------*/

        cdate = yyyyMMddHH.format(currentDate);

        /*--------------------------------------*/
        /* generate and save gif image */
        /*--------------------------------------*/

        String save_flag = appsDefaults.getToken("mpe_save_gif");

        if (save_flag.equalsIgnoreCase("save")) {
            String gif_dir = appsDefaults.getToken("mpe_gif_dir");
            String ftype = appsDefaults.getToken("mpe_gif_id");
            String fnamgif;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamgif = String.format("%s/%sz.gif", gif_dir, cdate);
            } else {
                fnamgif = String.format("%s/%s%sz.gif", gif_dir, ftype, cdate);
            }

            mpegui_save_image("gif", fnamgif);
        } else {
            statusHandler.handle(Priority.VERBOSE, "gif file not saved");
        }

        /*--------------------------------------*/
        /* generate and save jpeg image */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_jpeg");

        if (save_flag.equalsIgnoreCase("save")) {
            String jpeg_dir = appsDefaults.getToken("mpe_jpeg_dir");
            String ftype = appsDefaults.getToken("mpe_jpeg_id");
            String fnamjpeg;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamjpeg = String.format("%s/%sz.jpeg", jpeg_dir, cdate);
            } else {
                fnamjpeg = String.format("%s/%s%sz.jpeg", jpeg_dir, ftype,
                        cdate);
            }

            mpegui_save_image("jpeg", fnamjpeg);
        } else {
            statusHandler.handle(Priority.VERBOSE, "jpeg file not saved \n");
        }

        /*--------------------------------------*/
        /* generate and save netCDF file */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_netcdf");

        if (save_flag.equalsIgnoreCase("save")) {
            // String ftype = appsDefaults.getToken("mpe_netcdf_id");
            // String fnamnet;
            // if ((ftype == null) || ftype.isEmpty()) {
            // fnamnet = String.format("%sz.nc", cdate);
            // } else {
            // fnamnet = String.format("%s%sz.nc", ftype, cdate);
            // }
            //
            // String dirname = appsDefaults.getToken("mpe_netcdf_dir");

            // UFStatus.handle(Priority.VERBOSE, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null, String.format(
            // "Saving netcdf file in %s/%s", dirname, fnamnet));
            //
            // len = strlen(fnamnet);
            //
            // save_netcdf(fnamnet, &len, &MAXX, &MAXY, filename, &lenfn, &
            // int_irc);
            //
            // irc = int_irc;
            //
            // if(irc != 0)
            // {
            // if(irc == 1)
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null,
            // "malloc for precip array failed - netCDF file not saved");
            // else if(irc == 2)
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null, String.format(
            // "error reading %s -- netCDF file not saved",
            // filename));
            // else if(irc == 3)
            // {
            // UFStatus
            // .handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null,
            // "error attempting to create netCDF file (from nc_create routine)");
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null,
            // " -- netCDF file not saved\n");
            // }
            // }
        } else {
            statusHandler.handle(Priority.VERBOSE, "netCDF file not saved");
        }
        /*--------------------------------------*/
        /* generate and save grib file */
        /*--------------------------------------*/

        save_flag = appsDefaults.getToken("mpe_save_grib");

        if (save_flag.equalsIgnoreCase("save")) {
            String ftype = appsDefaults.getToken("mpe_grib_id");
            String fnamgrib;
            if ((ftype == null) || ftype.isEmpty()) {
                fnamgrib = String.format("%sz.grib", cdate);
            } else {
                fnamgrib = String.format("%s%sz.grib", ftype, cdate);
            }

            String dirname = appsDefaults.getToken("mpe_grib_dir");

            statusHandler.handle(Priority.VERBOSE, String.format(
                    "Saving grib encoded file in %s/%s\n", dirname, fnamgrib));
            MPEProcessGrib mpgr = new MPEProcessGrib();
            mpgr.saveGrib(fileNameXmrg, fnamgrib);
            //
            // pGribCommand = save_grib(fileNameXmrg, &lenfnx, fnamgrib,
            // &lenfng);
            //
            // if ( pGribCommand != NULL )
            // {
            // UFStatus
            // .handle(
            // Priority.VERBOSE,
            // Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE,
            // null,
            // String
            // .format(
            // "process_grib_files script called using command '%s'.",
            // pGribCommand));
            // free ( pGribCommand );
            // pGribCommand = NULL;
            // }
        } else {
            statusHandler.handle(Priority.VERBOSE,
                    "grib encoded file not saved \n");
        }

        /* Check if the RFC Bias needs to be sent across the WAN. */
        boolean transmit_rfc_bias = appsDefaults.getBoolean(
                "mpe_transmit_bias", true);
        boolean transmit_bias_on_save = appsDefaults.getBoolean(
                "transmit_bias_on_save", true);
        if (transmit_rfc_bias && transmit_bias_on_save) {
            // sprintf ( command_string, "%s/transmit_rfc_bias %s",
            // precip_proc_bin_dir, cdate );
            // UFStatus.handle(Priority.VERBOSE, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_MPE, null,
            // String.format("Invoking transmit_rfc_bias script using command:\n"
            // "%s\n", command_string ));
            // system ( command_string );
        }

        // num_prev_poly = 0;
        // num_draw_precip_poly = 0;
        // deletepoly_flag = 0;
        shell.setCursor(prevCursor);
        cursor.dispose();
    }

    private void mpegui_save_image(String format, String path) {
        statusHandler.handle(Priority.VERBOSE,
                String.format("Saving %s file in %s\n", format, path));

        Iterator<ImageWriter> iter = ImageIO
                .getImageWritersByFormatName(format);
        ImageWriter writer = iter.next();

        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
        BufferedImage bi = editor.screenshot();

        try {
            writer.setOutput(new FileImageOutputStream(new File(path)));
            writer.write(bi);
        } catch (IOException e) {
            statusHandler.error("Error creating file ", e);
        }
    }

    private void update_rwr_save(String rfc, Date dt, String fldtype) {

        String asave = "F";
        String drpr = "F";

        // RWA: this flag doesn't appear to be set anywhere
        // if ( applyprecip_flag == 1 )
        // {
        // drpr = "T";
        // }

        /* Build the obstime time string. */
        String datetime_obs_xmrg = OBS_TIME_SDF.format(dt);

        /*
         * Build the where clause. This can be used for both the select and the
         * update.
         */
        String where = String.format("WHERE id.rfc='%s' AND id.obstime='%s'",
                rfc, datetime_obs_xmrg);

        /* Get the record to update from the RWResult table. */
        List<Rwresult> pRWResultHead = IHFSDbGenerated.GetRWResult(where);

        if (pRWResultHead.size() == 0) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            String.format(
                                    "In routine 'update_rwr_save': Could not select a record from the RWResult table for query '%s'.\n",
                                    where));
        } else {
            Rwresult pRWResultNode = pRWResultHead.get(0);

            /* Update the elements in the RWResult node. */
            pRWResultNode.setMapxFieldType(fldtype);
            pRWResultNode.setAutoSave(asave);
            pRWResultNode.setDrawPrecip(drpr);
            pRWResultNode.setLastSaveTime(SimulatedTime.getSystemTime()
                    .getTime());

            /* Update the record in the database. */
            int status = IHFSDbGenerated.UpdateRWResult(pRWResultNode);

            if (status == -1) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                String.format(
                                        "In routine 'update_rwr_save': could not update record in RWResult for query '%s'.",
                                        where));
            }

            /* Free the memory used by the linked list of RWResult structures. */
            pRWResultHead.clear();
        }

    }

    private void save_merged_RFCW(String fileName, String processFlag) {
        XmrgFile xmrg = ((XmrgResource) displayedResource).getXmrgFile();
        if (xmrg == null) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            MessageBox box = new MessageBox(shell, SWT.ERROR);
            box.setText("Cannot Save");
            box.setMessage("No Data Available, cannot save");
            box.open();
            return;
        }
        xmrg.setData(((XmrgResource) displayedResource).getData());
        short[] data = xmrg.getData();

        for (int i = 0; i < data.length; i++) {
            if (data[i] < 0) {
                data[i] = 0;
            }
        }
        xmrg.setData(data);
        XmrgHeader header = xmrg.getHeader();
        header.setProcessFlag(processFlag);
        try {
            xmrg.save(fileName);

            dataSaved = true;
        } catch (IOException e) {
            statusHandler.error("Error saving xmrg file ", e);
            e.printStackTrace();
        }
    }

    /**
     * Remove the MPEPolygonResource from the display's resource list
     */
    public void removePolyResource() {
        ResourceList rscList = display.getDescriptor().getResourceList();
        List<AbstractVizResource<?, ?>> polyRscs = rscList
                .getResourcesByType(MPEPolygonResource.class);
        for (AbstractVizResource<?, ?> rsc : polyRscs) {
            rscList.removeRsc(rsc);
        }
    }

    /**
     * @param accum_interval
     *            the accum_interval to set
     */
    public void setAccum_interval(int accum_interval) {
        this.accum_interval = accum_interval;
    }

    /**
     * @return the accum_interval
     */
    public int getAccum_interval() {
        return accum_interval;
    }

    /**
     * @param otherDispType
     *            the otherDispType to set
     */
    public void setOtherDispType(DisplayFieldData otherDispType) {
        this.otherDispType = otherDispType;
    }

    /**
     * @return the otherDispType
     */
    public DisplayFieldData getOtherDispType() {
        if (otherDispType == null) {
            otherDispType = getDisplayFieldType();
        }
        return otherDispType;
    }

    /**
     * @return the pColorSetGroup
     */
    public List<NamedColorUseSet> getColorSetGroup() {
        return pColorSetGroup;
    }

    /**
     * @return the timeLapseHours
     */
    public int getTimeLapseHours() {
        return timeLapseHours;
    }

    /**
     * @param timeLapseHours
     *            the timeLapseHours to set
     */
    public void setTimeLapseHours(int timeLapseHours) {
        this.timeLapseHours = timeLapseHours;
    }

    /**
     * @return the timeLapseMode
     */
    public boolean isTimeLapseMode() {
        return timeLapseMode;
    }

    /**
     * @param timeLapseMode
     *            the timeLapseMode to set
     */
    public void setTimeLapseMode(boolean timeLapseMode) {
        this.timeLapseMode = timeLapseMode;
        if (timeLapseRsc == null) {
            timeLapseRsc = new TimeLapseResource(this, displayFieldType,
                    getGageColorMap());
            display.getDescriptor().getResourceList().add(timeLapseRsc);
        }

        timeLapseRsc.setKeepLooping(timeLapseMode);
    }

    /**
     * Reference to currently opened Gage Table Dialog.
     * 
     * @param gageTableDlg
     *            the gageTableDlg to set Set to null when Gage Table Dialog is
     *            not displayed
     */
    public void setGageTableDlgReference(GageTableDlg gageTableDlg) {
        this.gageTableDlg = gageTableDlg;
    }

    /**
     * @return the previousResource
     */
    public AbstractVizResource<?, ?> getPreviousResource() {
        return previousResource;
    }

    /**
     * @param previousResource
     *            the previousResource to set
     */
    public void setPreviousResource(AbstractVizResource<?, ?> previousResource) {
        this.previousResource = previousResource;
    }
}
