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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.awt.Rectangle;
import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.util.DPAFile;
import com.raytheon.uf.common.mpe.util.RadarCoverageFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.DPAResource;
import com.raytheon.viz.mpe.ui.rsc.DPAResource.SingleSiteRadarType;
import com.raytheon.viz.mpe.ui.rsc.RadarCoverageResource;
import com.raytheon.viz.mpe.ui.rsc.RadarGageOverlayRsc;
import com.raytheon.viz.mpe.ui.rsc.RadarGageOverlayRscData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Review Hourly Radar Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2009 2675       mpduff     Initial creation
 * Aug 13, 2009 2675       mpduff     TIM changes added
 * Nov 08, 2009 3232       mpduff     Implement the precip gage overlay
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ReviewHourlyRadarDlg extends CaveSWTDialog implements
        IMultiPaneEditor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReviewHourlyRadarDlg.class);

    /** Apps Default DPA Grid Directory token. */
    private static final String DPA_GRID_DIR_TOKEN = "dpa_grid_dir";

    /** Apps Default Radar Coverage Directory token. */
    private static final String RADAR_COVERAGE_DIR_TOKEN = "rfcwide_misbin_dir";

    /** Newline character */
    private static final String newLine = "\n";

    /** Bundle file location */
    private static final String BUNDLE_LOC = "bundles/MPE/reviewHourlyRadarBundle.xml";

    /** Popup shell. */
    private Shell popupShell;

    private LoopProperties loopProps = new LoopProperties();

    // Menu items
    private MenuItem rfcBoundaryMI;

    private MenuItem statesMI;

    private MenuItem countiesMI;

    private MenuItem citiesMI;

    private MenuItem basinBoundariesMI;

    private MenuItem riversMI;

    private MenuItem gagesMI;

    private MenuItem radarUmbrellaMI;

    private Label biasLbl;

    /** The radar Id */
    private String radId = null;

    /** Radar bias value */
    private String biasValue = null;

    /** Raw Radar resource */
    private DPAResource rawRadarRsc = null;

    /** Bias Radar resource */
    private DPAResource biasRadarRsc = null;

    /** Radar Climo resource */
    private DPAResource climoRadarRsc = null;

    /** Radar Misbin resource */
    private RadarCoverageResource misbinRadarRsc = null;

    private Date dpaDate = null;

    /** Popup displayed flag */
    private boolean popupOpen = false;

    private final PaneManager paneManager = new PaneManager();

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell
     * @param radId
     *            The radar id
     */
    public ReviewHourlyRadarDlg(Shell parentShell, String radId) {
        super(parentShell);
        setText("Single Radar Site");

        this.radId = radId;

        biasValue = getBiasValue(radId);
        // try {
        dpaDate = MPEDisplayManager.getCurrent().getCurrentEditDate();

        // Changed to comply with DR 11395

        // dpaDate =
        // RadarDataManager.getInstance().getLatestObstimeDpaRadar(
        // radId, MPEDisplayManager.getCurrent().getCurrentDate());
        // }
        // catch (VizException e) {
        // e.printStackTrace();
        // dpaDate = MPEDisplayManager.getCurrent().getCurrentDate();
        // }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.horizontalSpacing = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layout
        initializeComponents();
    }

    /**
     * Initialize the gui widgets
     */
    private void initializeComponents() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 1;
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        comp.setLayout(gl);
        GridData gd = new GridData(600, SWT.DEFAULT);
        comp.setLayoutData(gd);

        createMenus();
        createFourPanel(comp);

        createLowerLabels(comp);
    }

    /**
     * Create the menus
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createControlMenu(menuBar);
        createOverlayMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the control menu
     * 
     * @param menuBar
     *            The menubar
     */
    private void createControlMenu(Menu menuBar) {
        RadarDataManager radarDataManager = RadarDataManager.getInstance();

        // Create the Control menu
        MenuItem controlMI = new MenuItem(menuBar, SWT.CASCADE);
        controlMI.setText("Control");

        Menu controlMenu = new Menu(menuBar);
        controlMI.setMenu(controlMenu);

        // Close menu item
        MenuItem closeMI = new MenuItem(controlMenu, SWT.NONE);
        closeMI.setText("&Close\tCtrl+C");
        closeMI.setAccelerator(SWT.CTRL + 'C');
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        // Create the Options menu
        MenuItem optionsMI = new MenuItem(menuBar, SWT.CASCADE);
        optionsMI.setText("Options");

        Menu optionsMenu = new Menu(menuBar);
        optionsMI.setMenu(optionsMenu);

        // Edit Bias Value menu item
        MenuItem editBiasValueMI = new MenuItem(optionsMenu, SWT.NONE);
        editBiasValueMI.setText("Edit Bias Value");
        editBiasValueMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                EditBiasDlg dlg = new EditBiasDlg(shell, radId, biasValue);
                double bias = (Double) dlg.open();
                biasValue = String.format("%-1.2f", bias);
                biasLbl.setText("  Bias Value: " + biasValue + "  ");
            }
        });

        // Ignore Radar menu item
        boolean ignoreRadar = false;
        try {
            ignoreRadar = radarDataManager.getIgnoreRadar(radId, dpaDate);
        } catch (VizException e) {
            System.err.println("Error getting Ignore Radar Flag from IHFS");
        }
        MenuItem ignoreRadarMI = new MenuItem(optionsMenu, SWT.CHECK);
        ignoreRadarMI.setText("Ignore Radar");
        ignoreRadarMI.setSelection(ignoreRadar);
        ignoreRadarMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem ir = (MenuItem) event.getSource();
                boolean ignoreRadar = ir.getSelection();

                try {
                    int status = RadarDataManager.getInstance()
                            .updateIgnoreRadar(radId, dpaDate, ignoreRadar);
                    if (status == 0) {
                        System.err.println("Update not successful");
                    } else {
                        MPEDataManager.getInstance().setRadarEditFlag(true);
                    }
                } catch (VizException e) {
                    System.err
                            .println("Error updating ignore radar flag in IHFS");
                }

                for (IDisplayPane pane : getDisplayPanes()) {
                    List<DPAResource> rscs = pane.getDescriptor()
                            .getResourceList()
                            .getResourcesByTypeAsType(DPAResource.class);
                    for (DPAResource rsc : rscs) {
                        rsc.update();
                    }
                }
            }
        });

        // Display Adaptable Param menu item
        MenuItem displayAdaptableParamMI = new MenuItem(optionsMenu, SWT.NONE);
        displayAdaptableParamMI.setText("Display Adaptable Param");
        displayAdaptableParamMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DisplayDlg dlg = new DisplayDlg(shell, radId, dpaDate,
                        ((MenuItem) event.getSource()).getText());
                dlg.open();
            }
        });

        // Display Supplemental Data menu item
        MenuItem displaySuppDataMI = new MenuItem(optionsMenu, SWT.NONE);
        displaySuppDataMI.setText("Display Supplemental Data");
        displaySuppDataMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DisplayDlg dlg = new DisplayDlg(shell, radId, dpaDate,
                        ((MenuItem) event.getSource()).getText());
                dlg.open();
            }
        });
    }

    /**
     * Create the overlays menu
     * 
     * @param menuBar
     *            The MenuBar
     */
    private void createOverlayMenu(Menu menuBar) {
        // Create the Overlays menu
        MenuItem overlayMI = new MenuItem(menuBar, SWT.CASCADE);
        overlayMI.setText("Overlays");

        Menu overlayMenu = new Menu(menuBar);
        overlayMI.setMenu(overlayMenu);

        // RFC Boundaries menu item
        rfcBoundaryMI = new MenuItem(overlayMenu, SWT.CHECK);
        rfcBoundaryMI.setText("RFC Boundaries");
        rfcBoundaryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // States menu item
        statesMI = new MenuItem(overlayMenu, SWT.CHECK);
        statesMI.setText("States");
        statesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Counties menu item
        countiesMI = new MenuItem(overlayMenu, SWT.CHECK);
        countiesMI.setText("Counties");
        countiesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Cities menu item
        citiesMI = new MenuItem(overlayMenu, SWT.CHECK);
        citiesMI.setText("Cities/Towns");
        citiesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Basin Boundaries menu item
        basinBoundariesMI = new MenuItem(overlayMenu, SWT.CHECK);
        basinBoundariesMI.setText("Basin Boundaries");
        basinBoundariesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Rivers menu item
        riversMI = new MenuItem(overlayMenu, SWT.CHECK);
        riversMI.setText("Rivers");
        riversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Precip Gages menu item
        gagesMI = new MenuItem(overlayMenu, SWT.CHECK);
        gagesMI.setText("Precip Gages");
        gagesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Radar Umbrella menu item
        radarUmbrellaMI = new MenuItem(overlayMenu, SWT.CHECK);
        radarUmbrellaMI.setText("Radar Umbrella");
        radarUmbrellaMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });
    }

    /**
     * Create the 4 panel display
     * 
     * @param mainComp
     *            The main composite of the dialog
     */
    private void createFourPanel(Composite mainComp) {
        Composite paneComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 600;
        gd.heightHint = 600;
        paneComp.setLayoutData(gd);

        paneManager.initializeComponents(this, paneComp);
        initializeMaps();
    }

    /**
     * Create the labels along the bottom
     * 
     * @param mainComp
     *            The main composite
     */
    private void createLowerLabels(Composite mainComp) {
        Composite labelComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        labelComp.setLayout(gl);
        labelComp.setLayoutData(gd);

        Label radarLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        radarLbl.setText("  Radar: " + radId + "  ");
        radarLbl.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        Date currentDate = MPEDisplayManager.getCurrent().getCurrentEditDate();
        Label dateLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        dateLbl.setText("  "
                + MPEDateFormatter.format_MMM_dd_yyyy_HH(currentDate) + "z  ");
        dateLbl.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        biasLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        biasLbl.setText("  Bias Value: " + biasValue + "  ");
        biasLbl.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
    }

    /**
     * Initialize the maps.
     * 
     * @param comp
     *            The composite to hold the 4 panels
     */
    private void initializeMaps() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        RadarDataManager radarManager = RadarDataManager.getInstance();
        File defaultBundle = PathManagerFactory.getPathManager().getStaticFile(
                BUNDLE_LOC);
        try {
            LoadSerializedXml.loadTo(this,
                    Bundle.unmarshalBundle(defaultBundle, getSubstitutions()));

            IDisplayPane[] panes = getDisplayPanes();

            String user_id = System.getProperty("user.name");
            String app_name = "hmapmpe";
            List<NamedColorUseSet> pColorSetGroup = MPEDisplayManager
                    .getCurrent().getColorSetGroup();

            // TODO: Replace with resources looking up color maps themselves!!!
            List<Colorvalue> colorSet = GetColorValues.get_colorvalues(user_id,
                    app_name, "RMOSAIC", 3600, "E", pColorSetGroup);

            List<Colorvalue> radCovColorSet = GetColorValues.get_colorvalues(
                    user_id, app_name, "RADCOV", 3600, "E", pColorSetGroup);
            // Get the DPA Data
            String dpaFilename = radarManager.getDPAFileName(radId, dpaDate);
            String dirname = appsDefaults.getToken(DPA_GRID_DIR_TOKEN);
            DPAFile dpaFile = new DPAFile(dirname + "/" + dpaFilename);
            dpaFile.setBiasValue(Double.parseDouble(biasValue));

            // Get the Radar Coverage Data
            String radCovFilename = "misbin." + radId;
            String radCovDirname = appsDefaults
                    .getToken(RADAR_COVERAGE_DIR_TOKEN);
            RadarCoverageFile radCovFile = new RadarCoverageFile(radCovDirname
                    + "/" + radCovFilename);

            int ngrd = radarManager.getNgrd(radId);
            rawRadarRsc = new DPAResource(dpaFile, colorSet, radId,
                    SingleSiteRadarType.RAW_RADAR, ngrd);
            panes[0].getDescriptor().getResourceList().add(rawRadarRsc);

            // Load the gage overlay data to the map
            RadarGageOverlayRscData rscData = new RadarGageOverlayRscData(
                    "Precip Gages", null);
            RadarGageOverlayRsc gageRsc = rscData.construct(
                    new LoadProperties(), EditorUtil.getActiveVizContainer()
                            .getActiveDisplayPane().getDescriptor());
            ResourceProperties gageResourceProperties = new ResourceProperties();
            gageResourceProperties.setMapLayer(true);
            gageResourceProperties.setSystemResource(false);
            gageResourceProperties.setVisible(false);
            gageResourceProperties.setResource(gageRsc);
            panes[0].getDescriptor().getResourceList()
                    .add(gageRsc, gageResourceProperties);

            climoRadarRsc = new DPAResource(dpaFile, colorSet, radId,
                    SingleSiteRadarType.RADAR_CLIMATOLOGY, ngrd);

            // Load the dpa data to the map
            panes[1].getDescriptor().getResourceList().add(climoRadarRsc);
            panes[1].getDescriptor().getResourceList()
                    .add(gageRsc, gageResourceProperties);

            biasRadarRsc = new DPAResource(dpaFile, colorSet, radId,
                    SingleSiteRadarType.MEAN_FIELD_BIAS_CORRECTED_RADAR, ngrd);

            // Load the dpa data to the map
            panes[2].getDescriptor().getResourceList().add(biasRadarRsc);
            panes[2].getDescriptor().getResourceList()
                    .add(gageRsc, gageResourceProperties);

            misbinRadarRsc = new RadarCoverageResource(radCovFile,
                    radCovColorSet, radId, ngrd);

            // Load the dpa data to the map
            panes[3].getDescriptor().getResourceList().add(misbinRadarRsc);
            panes[3].getDescriptor().getResourceList()
                    .add(gageRsc, gageResourceProperties);

            initOverlays();

            registerMouseHandler(new InputAdapter() {

                @Override
                public boolean handleMouseDown(int x, int y, int mouseButton) {
                    if (mouseButton == 3) {
                        createPopup(x, y);
                        return true;
                    }
                    return super.handleMouseDown(x, y, mouseButton);
                }

                @Override
                public boolean handleMouseUp(int x, int y, int mouseButton) {
                    if (popupOpen) {
                        closePopup();
                    }
                    return super.handleMouseUp(x, y, mouseButton);
                }

            });
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error initializing panes",
                    e);
            shell.dispose();
        }
    }

    /**
     * @return
     */
    private Map<String, String> getSubstitutions() {
        Map<String, String> subs = new HashMap<String, String>();
        RadarDataManager rdm = RadarDataManager.getInstance();
        double[] latlon = rdm.getLatLon(radId);
        subs.put("mapCenter", latlon[0] + " " + latlon[1] + " 0.0");
        return subs;
    }

    /**
     * Toggle the overlays off/on. The mapName in the bundle (resource name)
     * must be equal to the text of the menu.
     * 
     * @param mi
     *            The menuItem selected
     */
    private void toggleOverlay(MenuItem mi) {
        boolean visible = mi.getSelection();
        for (IDisplayPane pane : getDisplayPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getProperties().isMapLayer()
                        && rp.getResource().getName().equals(mi.getText())) {
                    rp.getProperties().setVisible(visible);
                }
            }
        }
    }

    /**
     * Initialize the overlays.
     */
    private void initOverlays() {
        boolean first = true;
        for (IDisplayPane pane : getDisplayPanes()) {
            if (first) {
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    if (rp.getProperties().isVisible()
                            && rp.getProperties().isMapLayer()) {
                        setOverlayMenu(rp.getResource());
                    }
                }
                first = false;
            }
        }
    }

    /**
     * Sets the initial state of the overlay menus.
     * 
     * @param rsc
     */
    private void setOverlayMenu(AbstractVizResource<?, ?> rsc) {
        if (rsc.getName().equalsIgnoreCase(citiesMI.getText())) {
            citiesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(countiesMI.getText())) {
            countiesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(gagesMI.getText())) {
            gagesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(radarUmbrellaMI.getText())) {
            radarUmbrellaMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(rfcBoundaryMI.getText())) {
            rfcBoundaryMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(riversMI.getText())) {
            riversMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(statesMI.getText())) {
            statesMI.setSelection(true);
        }
    }

    /**
     * Create the Location Data popup.
     * 
     * @param e
     *            The MouseEvent for the click
     */
    private void createPopup(int x, int y) {
        // Lat/Lon of mouse click
        Coordinate coord = translateClick(x, y);

        /* calculate national HRAP coordinates from lat/lon */
        Coordinate hp = new Coordinate(0, 0);
        try {
            hp = HRAP.getInstance().latLonToGridCoordinate(coord,
                    PixelOrientation.LOWER_LEFT);

            /* Get local hrap coordinate */
            Rectangle localHrapExtent = rawRadarRsc.getExtent();

            int localX = (int) (hp.x - localHrapExtent.x);
            int localY = (int) (hp.y - localHrapExtent.y);

            /* Get data values */
            StringBuilder buffer = new StringBuilder();
            buffer.append("National HRAP: x=");
            buffer.append((int) hp.x);
            buffer.append(" y=");
            buffer.append((int) hp.y);
            buffer.append(newLine);

            buffer.append("Latitude: " + String.format("%.2f", coord.x));
            buffer.append("  Longitude: " + String.format("%.2f", coord.y));
            buffer.append(newLine);

            buffer.append("Local HRAP: x=" + localX + " y=" + localY);
            buffer.append(newLine);

            /*
             * value divided by 100 and divided by 25.4 to convert to inches in
             * locator popup
             */
            double value = rawRadarRsc.getData()[localX][localY] / 100 / 25.4;
            if (value >= 0) {
                buffer.append("Raw Radar: " + String.format("%.3f", value)
                        + " in");
            } else {
                buffer.append("Raw Radar: missing");
            }

            buffer.append(newLine);

            value = biasRadarRsc.getData()[localX][localY] / 100 / 25.4;
            if (value >= 0) {
                buffer.append("Unbiased Radar: " + String.format("%.3f", value)
                        + " in");
            } else {
                buffer.append("Unbiased Radar: missing");
            }
            buffer.append(newLine);

            // code ported directly from OB9.X
            // create_ss_interface_rfcwide.c line 1326
            /* value = misbinRadarRsc.getData()[localX][localY]; */
            value = 0;
            if (value >= 0) {
                buffer.append("Misbin: " + String.format("%.3f", value));
            } else {
                buffer.append("Misbin: missing");
            }
            buffer.append(newLine);

            // code ported directly from OB9.X
            // create_ss_interface_rfcwide.c line 1326
            /* value = climoRadarRsc.getData()[localX][localY]; */
            value = 0;
            if (value >= 0) {
                buffer.append("Rad Clim: " + String.format("%.3f", value));
            } else {
                buffer.append("Rad Clim: missing");
            }

            showPopup(buffer.toString());
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting showing popup dialog", ex);
        }

    }

    /**
     * Show the popup dialog.
     * 
     * @param text
     *            The text to display
     */
    private void showPopup(String text) {
        Shell parent = getShell();
        popupShell = new Shell(parent, SWT.DIALOG_TRIM);
        popupShell.setText("Single Radar Site");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.horizontalSpacing = 1;
        mainLayout.verticalSpacing = 1;
        popupShell.setLayout(mainLayout);

        Composite comp = new Composite(popupShell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 1;
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        comp.setLayout(gl);
        GridData gd = new GridData(225, SWT.DEFAULT);
        comp.setLayoutData(gd);

        Label label = new Label(comp, SWT.NONE);
        label.setText(text);

        popupShell.pack();

        popupShell.open();
        popupOpen = true;
    }

    /**
     * Close the popup.
     */
    private void closePopup() {
        popupOpen = false;
        popupShell.dispose();
    }

    /**
     * Get the bias value used.
     * 
     * @param radId
     *            The Radar Id
     */
    private String getBiasValue(String radId) {
        MPERadarData radarData = MPEDataManager
                .getInstance()
                .readRadarData(
                        MPEDisplayManager.getCurrent().getCurrentEditDate())
                .get(radId);
        return String.format("%-1.2f", radarData.getRwBiasValUsed());
    }

    public InputManager getMouseManager() {
        return paneManager.getMouseManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler,
     * com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        paneManager.registerMouseHandler(handler, priority);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#unregisterMouseHandler
     * (com.raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return loopProps;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#setLoopProperties(com.
     * raytheon.uf.viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        loopProps = loopProperties;
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        IDisplayPane pane = getActiveDisplayPane();
        // Convert the screen coordinates to grid space
        double[] world = pane.screenToGrid(x, y, 0);
        IExtent extent = pane.getRenderableDisplay().getExtent();
        // Verify grid space is within the extent, otherwiser return null
        if (world == null || extent.contains(world) == false) {
            return null;
        }
        // use descriptor to convert pixel world to CRS world space
        world = pane.getDescriptor().pixelToWorld(world);
        // Check for null
        if (world == null) {
            return null;
        }
        return new Coordinate(world[0], world[1], world[2]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateInverseClick(
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public double[] translateInverseClick(Coordinate c) {
        if (c == null) {
            return null;
        }
        IDisplayPane pane = getActiveDisplayPane();
        double[] grid = pane.getDescriptor().worldToPixel(
                new double[] { c.x, c.y, c.z });
        if (grid == null) {
            return null;
        }
        return pane.gridToScreen(grid);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return paneManager.getDisplayPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        paneManager.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#getNumberofPanes()
     */
    @Override
    public int getNumberofPanes() {
        return paneManager.getNumberofPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#setSelectedPane(java.lang
     * .String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        paneManager.setSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPane(java.lang
     * .String)
     */
    @Override
    public IDisplayPane getSelectedPane(String action) {
        return paneManager.getSelectedPane(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPanes(java.lang
     * .String)
     */
    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#isSelectedPane(java.lang.
     * String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return paneManager.isSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.addSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removeSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.removeSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removePane(com.raytheon.uf
     * .viz.core.IDisplayPane)
     */
    @Override
    public void removePane(IDisplayPane pane) {
        paneManager.removePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#hidePane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void hidePane(IDisplayPane pane) {
        paneManager.hidePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#showPane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void showPane(IDisplayPane pane) {
        paneManager.showPane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#displayedPaneCount()
     */
    @Override
    public int displayedPaneCount() {
        return paneManager.displayedPaneCount();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#clear()
     */
    @Override
    public void clear() {
        paneManager.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * removeRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * notifyRenderableDisplayChangedListeners
     * (com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {

    }
}
