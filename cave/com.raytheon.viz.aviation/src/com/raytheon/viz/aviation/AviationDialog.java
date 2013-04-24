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

package com.raytheon.viz.aviation;

import java.io.File;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.aviation.climatology.ClimateMenuDlg;
import com.raytheon.viz.aviation.model.ForecastModel;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.utility.IBackupRestart;
import com.raytheon.viz.aviation.xml.AviationForecasterConfig;
import com.raytheon.viz.aviation.xml.ForecasterConfig;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.AvnConfigFileUtil;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * The Aviation Dialog class that displays the start up menu for AvnFPS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/21/2008    817         grichard    Initial creation.
 * 4/9/2008     934         grichard    Added modeless dialogs.
 * 8/11/2008    1314        grichard    Used PathManager for pathnames.
 * 9/12/2008    1444        grichard    Accommodate separate message logs.
 * 5/11/2009    1982        grichard    Added backup/restart monitor feature.
 * 7/09/2010    5078        rferrel     Use AvnConfigFileUtil to find
 *                                      configuration files.
 * 8/01/2010    4345        rferrel     Monitor shows default product sites.
 * 10/06/2010   6009        rferrel     Changes to use product.
 * 10/27/2010   7383        rferrel     Restart now reloads setup information
 *                                      from configuration.
 * 12/06/2010   5342        rferrel     Change getForecasterConfig to only
 *                                      return login user name configuration.
 * 12/14/2010   5782        rferrel     Restart reloads all resource information.
 * 3/14/2011    8588        rferrel     Allow monitoring of multiple products.
 * 3/31/2011    8774        rferrel     The shell.disposed() handled correctly
 *                                      for both stand alone and from Cave.
 * 8/31/2011    10837       rferrel     Added checks to see if the avnImage
 *                                      file exists.
 * 10/02/2012   1229        rferrel     Made dialog non-blocking.
 * 10/09/2012   1229        rferrel     Changes for non-blocking TafMonitorDlg.
 * 04/10/2013   1735        rferrel     Changes for taf monitor speed up.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
public class AviationDialog extends CaveSWTDialog implements IBackupRestart {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AviationDialog.class);

    private static String FORECAST_CONFIG_FILE = "aviation" + File.separator
            + "avnwatch" + File.separator + "aviationForecasterConfig.xml";

    /**
     * Label font.
     */
    private Font font;

    /**
     * Font list to dispose of.
     */
    java.util.List<Font> fontList;

    /**
     * Dialog image.
     */
    private Image avnImage;

    /**
     * Dialog image.
     */
    private Image eclipseImage;

    /**
     * The station map.
     */
    private Map<String, java.util.List<String>> stationMap;

    /**
     * Products whose sites will display in the monitor.
     */
    private java.util.List<String> productDisplayList;

    /**
     * The Taf Monitor Dialog
     */
    private TafMonitorDlg tafMonitorDlg;

    /**
     * Climate menu dialog.
     */
    private ClimateMenuDlg climateMenuDlg;

    /**
     * Selected user id number in the forecaster list.
     */
    public static int USERID = 0;

    /**
     * Selected user name in the forecaster list control.
     */
    public static String USERNAME = "";

    /**
     * Selected user name in the forecaster list control.
     */
    public static boolean USERTRANSMIT = true;

    /**
     * List control containing forecaster names.
     */
    private List forecasterList;

    /**
     * Number of dialogs currently open,
     */
    private final AtomicInteger dlgCount = new AtomicInteger(0);

    /**
     * Create a non-blocking dialog.
     * 
     * @param parent
     */
    public AviationDialog(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Menu");

        ForecastModel.getInstance().setBackupRestartUtility(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
        for (Font f : fontList) {
            f.dispose();
        }
        fontList.clear();
        fontList = null;

        if (avnImage != null) {
            avnImage.dispose();
            avnImage = null;
        }

        if (eclipseImage != null) {
            eclipseImage.dispose();
            eclipseImage = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        fontList = new ArrayList<Font>();

        font = new Font(getDisplay(), "Monospace", 8, SWT.NORMAL);
        fontList.add(font);

        String path = loadGifPath("avn");
        if (path != null) {
            avnImage = new Image(getDisplay(), path);
        }

        path = loadGifPath("eclipse");
        if (path != null) {
            eclipseImage = new Image(getDisplay(), path);
        }

        loadTafSiteConfig();

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Set class variables based on information in the Taf Site Configuration
     * File.
     */
    private void loadTafSiteConfig() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            stationMap = config.getAllProducts();
            if (productDisplayList == null) {
                productDisplayList = new ArrayList<String>();
            }
            if (productDisplayList.size() == 0
                    && config.getDefaultProduct() != null) {
                productDisplayList.add(config.getDefaultProduct());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading site config",
                    e);
        }
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createLabel();
        createList();
        createComposite();
        initAcarsSounding();
    }

    /**
     * To speed up the display of the monitor this method starts up the Data
     * Cube for the acarssounding inventory which can take over a second for the
     * initial retrieval. This may cause blocking issues that slows the display
     * of the monitor.
     */
    private void initAcarsSounding() {
        Job job = new Job("AviationDialog") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                DataCubeContainer.getInventory("acarssounding");
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Create the label on the display.
     */
    private void createLabel() {
        createAvnFPSLabel();
    }

    /**
     * Create the list on the display.
     */
    private void createList() {
        createForecasterList();
    }

    /**
     * Create the Composite that will contain the parts of the dialog.
     */
    private void createComposite() {
        // Create the bottom composite widget
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite bottomComposite = new Composite(shell, SWT.NONE);
        GridLayout layoutBC = new GridLayout(2, false);
        bottomComposite.setLayout(layoutBC);
        bottomComposite.setLayoutData(gd);

        // Create the left composite widget
        Composite leftComposite = new Composite(bottomComposite, SWT.NONE);
        GridLayout layoutLC = new GridLayout(1, false);
        leftComposite.setLayout(layoutLC);

        // Create the right composite widget
        Composite rightComposite = new Composite(bottomComposite, SWT.NONE);
        GridLayout layoutRC = new GridLayout(1, false);
        rightComposite.setLayout(layoutRC);

        // Create the "eclipse" label
        Label eclipseLabel = new Label(leftComposite, SWT.CENTER);
        if (eclipseImage != null) {
            eclipseLabel.setImage(eclipseImage);
        } else {
            Font font = new Font(getDisplay(), "sans-serif", 9, SWT.NORMAL);
            fontList.add(font);
            eclipseLabel.setText("No Image\nfound");
            eclipseLabel.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_TITLE_BACKGROUND_GRADIENT));
            eclipseLabel.setFont(font);
        }

        // Create the "TAFs" button
        Button tafsBtn = new Button(rightComposite, SWT.PUSH);
        String tafsBtnTitle = "TAFs";
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 170;
        tafsBtn.setLayoutData(data);
        tafsBtn.setText(tafsBtnTitle);
        tafsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                if (tafMonitorDlg == null
                        || tafMonitorDlg.getShell().isDisposed()) {
                    ResourceConfigMgr configMgr = ResourceConfigMgr
                            .getInstance();
                    configMgr.reloadResourceData();
                    dlgCount.incrementAndGet();
                    loadTafSiteConfig();
                    displayTafMonitorDialog();
                } else {
                    tafMonitorDlg.bringToTop();
                }
            }
        });

        // Create the "Climate" button
        Button climBtn = new Button(rightComposite, SWT.PUSH);
        String climBtnTitle = "Climate";
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 100;
        climBtn.setLayoutData(data);
        climBtn.setText(climBtnTitle);
        climBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                if (climateMenuDlg == null || climateMenuDlg.getShell() == null
                        || climateMenuDlg.isDisposed()) {
                    // Create an array of message types
                    StatusMessageType[] msgTypes = new StatusMessageType[4];
                    msgTypes[0] = StatusMessageType.Metar;
                    msgTypes[1] = StatusMessageType.WindRose;
                    msgTypes[2] = StatusMessageType.CigVis;
                    msgTypes[3] = StatusMessageType.CigVisTrend;

                    // Create the climate menu dialog.
                    dlgCount.incrementAndGet();
                    climateMenuDlg = new ClimateMenuDlg(shell, msgTypes, null);
                    climateMenuDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            climateMenuDlg = null;
                            if (dlgCount.decrementAndGet() == 0) {
                                shell.dispose();
                            }
                        }
                    });
                    climateMenuDlg.open();
                } else {
                    climateMenuDlg.bringToTop();
                }
            }
        });

        // Create the "Cancel" button
        Button cancelBtn = new Button(rightComposite, SWT.PUSH);
        String cancelBtnTitle = "Cancel";
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 100;
        cancelBtn.setLayoutData(data);
        cancelBtn.setText(cancelBtnTitle);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Get fully qualified path name for image
     * 
     * @param imageName
     *            - base image name
     * @return path - fully qualified name or null if file does not exist
     */
    private String loadGifPath(String imageName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = null;
        File file = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), "aviation" + File.separatorChar
                + "avnwatch" + File.separatorChar + imageName + ".gif");
        if (file.canRead()) {
            path = file.getAbsolutePath();
        }
        return path;
    }

    /**
     * Create the AvnFPS label.
     */
    private void createAvnFPSLabel() {
        GridData gd = new GridData(275, 250);
        Label avnLabel = new Label(shell, SWT.CENTER);
        if (avnImage != null) {
            avnLabel.setImage(avnImage);
        } else {
            Font font = new Font(getDisplay(), "sans-serif", 14, SWT.NORMAL);
            fontList.add(font);
            avnLabel.setText("No Image found");
            avnLabel.setBackground(getDisplay().getSystemColor(
                    SWT.COLOR_TITLE_BACKGROUND_GRADIENT));
            avnLabel.setFont(font);
        }
        avnLabel.setLayoutData(gd);
    }

    /**
     * Create the forecaster list.
     */
    private void createForecasterList() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 200;
        forecasterList = new List(shell, SWT.BORDER | SWT.SINGLE);
        forecasterList.setLayoutData(gd);

        forecasterList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateUsername();
            }
        });

        // Load the Forecaster Configuration Information.
        ArrayList<ForecasterConfig> forecasterArray = getForecasterConfig();

        if (forecasterArray == null) {
            return;
        }

        try {
            for (ForecasterConfig forecaster : forecasterArray) {
                forecasterList.add(forecaster.getName());
            }

        } catch (Exception e1) {
            // do nothing
        }

        forecasterList.setSelection(0);

        updateUsername();
    }

    /**
     * Update the user name and the transmit flag when a new users is selected
     * from the list.
     */
    private void updateUsername() {
        USERNAME = forecasterList.getItem(forecasterList.getSelectionIndex());
        if (USERNAME == null) {
            USERID = 0;
            USERNAME = "";
            USERTRANSMIT = false;
            return;
        }

        ArrayList<ForecasterConfig> forecasterArray = getForecasterConfig();

        if (forecasterArray == null) {
            USERID = 0;
            USERTRANSMIT = false;
            return;
        }

        for (ForecasterConfig forecaster : forecasterArray) {
            if (forecaster.getName().compareTo(USERNAME) == 0) {
                USERID = forecaster.getId();
                USERTRANSMIT = forecaster.getXmitPrivilege();
                break;
            }
        }
    }

    /**
     * Close the display.
     */
    public void closeDisplay() {
        shell.dispose();
    }

    /**
     * Get the Forecaster Configuration information for current user.
     */
    public static ArrayList<ForecasterConfig> getForecasterConfig() {
        File f = AvnConfigFileUtil.getStaticFile(FORECAST_CONFIG_FILE);
        ArrayList<ForecasterConfig> fcList = null;
        ForecasterConfig fc = null;
        String loginName = LocalizationManager.getInstance().getCurrentUser();

        try {
            if (f == null) {
                fcList = null;
            } else {
                fcList = JAXB.unmarshal(f, AviationForecasterConfig.class)
                        .getForecasterConfig();
            }
        } catch (RuntimeException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error loading forecaster config no transmit privileges granted",
                            e);
            fcList = null;
        }

        if (fcList == null) {
            // Unable to determine transmit privileges
            fc = new ForecasterConfig();
            fc.setName(loginName);
            fc.setXmit(false);
            fcList = new ArrayList<ForecasterConfig>();
            fcList.add(fc);
        } else {
            fc = null;
            for (ForecasterConfig tmpFc : fcList) {
                if (loginName.equals(tmpFc.getName())) {
                    fc = tmpFc;
                    break;
                }
            }

            if (fc == null) {
                // Unknown user
                fc = new ForecasterConfig();
                fc.setName(loginName);
                fc.setXmit(false);
            }
            fcList.clear();
            fcList.add(fc);
        }
        return fcList;
    }

    /**
     * Overridden method to restart the Taf Monitor.
     */
    @Override
    public void restartTafMonitor() {
        // This prevents tafMonitorDlg from closing this shell when closing the
        // TaMonitorDlg prior to the restart.
        dlgCount.incrementAndGet();
        if (tafMonitorDlg.closeDisplay() == false) {
            // adjust the count.
            dlgCount.decrementAndGet();
            return;
        }

        // Reload the Station Configuration Information.
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        TafSiteConfigFactory.clearInstance();
        configMgr.reloadResourceData();
        loadTafSiteConfig();
        displayTafMonitorDialog();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.aviation.utility.IBackupRestart#backupTafMonitor(java
     * .lang.String)
     */
    @Override
    public void backupTafMonitor(java.util.List<String> productDisplayList,
            Map<String, java.util.List<String>> stationMap) {
        this.stationMap = stationMap;
        this.productDisplayList = productDisplayList;
        boolean emptyStationList = true;
        for (String product : productDisplayList) {
            if (stationMap.get(product).size() > 0) {
                emptyStationList = false;
                break;
            }
        }

        if (!emptyStationList) {
            // This prevents tafMonitorDlg from closing this shell when closing
            // the TaMonitorDlg prior to the restart.
            dlgCount.incrementAndGet();
            if (tafMonitorDlg.closeDisplay() == false) {
                // adjust the count.
                dlgCount.decrementAndGet();
                return;
            }

            displayTafMonitorDialog();
        } else {
            restartTafMonitor();
        }
    }

    /**
     * Display the TAF monitor dialog.
     * 
     */
    private void displayTafMonitorDialog() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (configMgr.isResourceLoaded() == false) {
            configMgr.reloadResourceData();
        }

        java.util.List<String> stationList = new ArrayList<String>();
        for (String product : productDisplayList) {
            for (String site : stationMap.get(product)) {
                stationList.add(site);
            }
        }

        if (stationList.size() == 0) {
            if (productDisplayList.isEmpty()) {
                statusHandler
                        .handle(Priority.PROBLEM, "No stations configured");
            }
            for (String product : productDisplayList) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error no stations configured for " + product);
            }
        } else {
            if (tafMonitorDlg == null || tafMonitorDlg.getShell() == null
                    || tafMonitorDlg.isDisposed()) {
                tafMonitorDlg = new TafMonitorDlg(shell, stationList,
                        productDisplayList);
                tafMonitorDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        tafMonitorDlg = null;
                        if (dlgCount.decrementAndGet() == 0
                                && !productDisplayList.isEmpty()) {
                            shell.dispose();
                        }
                    }
                });
                tafMonitorDlg.open();
            } else {
                tafMonitorDlg.bringToTop();
            }
        }
    }
}
