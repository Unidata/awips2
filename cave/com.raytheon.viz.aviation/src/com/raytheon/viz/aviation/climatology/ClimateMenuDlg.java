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

package com.raytheon.viz.aviation.climatology;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Climate Menu Dialog class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/24/2008    817         grichard   Initial creation.
 * 03/27/2008    1033        grichard   Added bottom message control.
 * 04/07/2008    934         grichard   Unselected message viewer button.
 * 04/10/2008    934         grichard   Populated site lists with icaos.
 * 05/07/2008    1121        grichard   Hot fix to upper case current site.
 * 05/12/2008    1119        grichard   Convert 3-letter site to conus 4-letter site.
 * 09/12/2008    1444        grichard   Accommodate separate message logs.
 * 07/09/2010    5078        rferrel    Added catch for FileNotFound
 *                                       in initializeComponents
 * 10/12/2010   6009        rferrel     Code clean up from making TafSiteConfig
 *                                       a singleton
 * 10/04/2012   1229        rferrel     Made non-blocking.
 * 10/08/2012   1229        rferrel     Changes for non-blocking WindRosePlotDlg.
 * 10/09/2012   1229        rferrel     Changes for non-blocking MetarDisplayDialog.
 * 10/09/2012   1229        rferrel     Changes for non-blocking CigVisTrendDlg.
 * 10/15/2012   1229        rferrel     Changes for non-blocking HelpUsageDlg.
 * 09/15/2015   4880        njensen     Removed dead code
 * 03/15/2016   5481        randerso    Fix GUI sizing problems
 * May 15, 2019 20693    mgamazaychikov TafSiteConfigFactory refactor
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
public class ClimateMenuDlg extends CaveSWTDialog {

    /**
     * The site name.
     */
    private String siteName;

    /**
     * The station list.
     */
    private List<String> stationList = new ArrayList<String>();

    /**
     * Array of message types.
     */
    private StatusMessageType[] statusMsgTypes;

    /**
     * Background color for the message status comp (may be null).
     */
    private RGB statusCompRGB;

    private MetarDisplayDialog metarDlg;

    private WindRosePlotDlg windRose;

    private CigVisDistributionDlg cigVisDist;

    private CigVisTrendDlg cigVisTrend;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param statusMsgTypes
     *            Array of message type.
     * @param statusCompRGB
     *            Message status composite background color.
     */
    public ClimateMenuDlg(Shell parent, StatusMessageType[] statusMsgTypes,
            RGB statusCompRGB) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Climate Menu");

        this.statusMsgTypes = statusMsgTypes;
        this.statusCompRGB = statusCompRGB;
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Set the Site Name based on localization.
        siteName = LocalizationManager.getInstance().getCurrentSite()
                .toUpperCase();

        if (siteName.length() == 3) {
            siteName = SiteMap.getInstance().getSite4LetterId(siteName);
        }

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (configMgr.isResourceLoaded() == false) {
            configMgr.reloadResourceData();
        }
        createMenus();
        createButtonsComposite();
        createBottomMessageControls();
        populateStationsOfInterest();
    }

    /**
     * Create the menus on the display.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenus(menuBar);
        createHelpMenus(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the file menus on the display.
     */
    private void createFileMenus(Menu menuBar) {
        // -------------------------------------
        // Create all the items in the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // --------------------------------------------------
        // Create Quit menu item
        // --------------------------------------------------
        MenuItem quitMenuItem = new MenuItem(fileMenu, SWT.NONE);
        quitMenuItem.setText("Quit");
        quitMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the help menus on the display.
     */
    private void createHelpMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the help menu
        // ----------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("Help");

        // Create the Help menu item with a Help "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // --------------------------------------------------
        // Create Usage menu item
        // --------------------------------------------------
        MenuItem usageMenuItem = new MenuItem(helpMenu, SWT.NONE);
        usageMenuItem.setText("Usage...");
        usageMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS Climate Menu - Usage";

                    String helpText = "This master menu GUI is used to launch applications that display NCDC\nclimatological data for observation sites in a variety of formats.\n\nButton description:\n\nMETARs:       use to display reconstructed METARs for a user-defined\n              span of days\nWind Rose:    displays Wind Rose for selected dates, times and flight\n              category conditions\nCigVis Dist:  displays ceiling, visibility and flight category\n              distributions by month, hour and wind direction\nCigVis Trend: displays 3-12 hour ceiling, visibility and flight\n              category forecast based on initial conditions";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the buttons composite on the display.
     */
    private void createButtonsComposite() {

        // Create the top composite widget
        Composite topComposite = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        topComposite.setLayout(layout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        topComposite.setLayoutData(gd);

        // Create the "METARs" button
        Button metarsBtn = new Button(topComposite, SWT.PUSH);
        String metarsBtnTitle = "METARs";
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        metarsBtn.setLayoutData(gd);
        metarsBtn.setText(metarsBtnTitle);
        metarsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((metarDlg == null) || (metarDlg.getShell() == null)
                        || metarDlg.isDisposed()) {
                    metarDlg = new MetarDisplayDialog(shell, stationList,
                            statusMsgTypes[0], statusCompRGB);
                    metarDlg.open();
                } else {
                    metarDlg.bringToTop();
                }
            }
        });

        // Create the "Wind Rose" button
        Button windRoseBtn = new Button(topComposite, SWT.PUSH);
        String windRoseBtnTitle = "Wind Rose";
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        windRoseBtn.setLayoutData(gd);
        windRoseBtn.setText(windRoseBtnTitle);
        windRoseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((windRose == null) || (windRose.getShell() == null)
                        || windRose.isDisposed()) {
                    windRose = new WindRosePlotDlg(shell, stationList,
                            statusMsgTypes[1], statusCompRGB);
                    windRose.open();
                } else {
                    windRose.bringToTop();
                }
            }
        });

        // Create the "Ceiling and Visibility Distribution Climatology" button
        Button distBtn = new Button(topComposite, SWT.PUSH);
        String distBtnTitle = "CigVis Dist";
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        distBtn.setLayoutData(gd);
        distBtn.setText(distBtnTitle);
        distBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((cigVisDist == null) || (cigVisDist.getShell() == null)
                        || cigVisDist.isDisposed()) {
                    cigVisDist = new CigVisDistributionDlg(shell, stationList,
                            statusMsgTypes[2], statusCompRGB);
                    cigVisDist.open();
                } else {
                    cigVisDist.bringToTop();
                }
            }
        });

        // Create the "Ceiling and Visibility Trend Climatology" button
        Button trendBtn = new Button(topComposite, SWT.PUSH);
        String trendBtnTitle = "CigVis Trend";
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        trendBtn.setLayoutData(gd);
        trendBtn.setText(trendBtnTitle);
        trendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((cigVisTrend == null) || (cigVisTrend.getShell() == null)
                        || cigVisTrend.isDisposed()) {
                    cigVisTrend = new CigVisTrendDlg(shell, stationList,
                            statusMsgTypes[3], statusCompRGB);
                    cigVisTrend.open();
                } else {
                    cigVisTrend.bringToTop();
                }
            }
        });
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        MessageStatusComp statusComp = new MessageStatusComp(shell, null, null);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        // Set width to try to make full dialog title visible
        GC gc = new GC(statusComp);
        layoutData.widthHint = gc.getFontMetrics().getAverageCharWidth() * 30;
        gc.dispose();
        statusComp.setLayoutData(layoutData);
    }

    /**
     * Close the display.
     */
    private void closeDisplay() {

        shell.dispose();
    }

    public void showDialog() {
        if (shell.isVisible() == false) {
            shell.setVisible(true);
        }

        shell.setActive();
    }

    /**
     * Populate the stations of interest.
     */
    private void populateStationsOfInterest() {

        try {
            stationList = TafSiteConfigFactory.getInstance().getIdsSiteList();
        } catch (Exception e) {

        }
    }

}
