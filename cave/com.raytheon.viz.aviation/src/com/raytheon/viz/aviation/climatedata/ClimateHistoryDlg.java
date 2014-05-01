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
package com.raytheon.viz.aviation.climatedata;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the climate history dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009 #3438      lvenable     Initial creation
 * Oct 06, 2012 #1229      rferrel     Made non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ClimateHistoryDlg extends CaveSWTDialog {

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * "Ident" (station) list control.
     */
    private List identList;

    /**
     * Tab folder that displays the graphs.
     */
    private TabFolder graphTabFolder;

    /**
     * Climate history data displayed on the GUI (station name, USAF-WBAN id and
     * the graph data).
     */
    private ClimateHistoryData climateHistData;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param climateHistData
     *            Climate histroy data.
     */
    public ClimateHistoryDlg(Shell parentShell,
            ClimateHistoryData climateHistData) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Climate History");

        this.climateHistData = climateHistData;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        return gl;
    }

    @Override
    protected void disposed() {
        textFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the data, controls, and layouts
        textFont = new Font(getDisplay(), "Courier", 10, SWT.BOLD);

        // intialize controls
        createFileMenu();

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        createIdentsList(mainComp);
        createChartTabFolder(mainComp);

        /*
         * Use climate history data to complete the GUI
         */
        populateIdentsList();
        createTabsForStation();
    }

    @Override
    protected boolean shouldOpen() {
        if (climateHistData == null) {
            MessageBox mb = new MessageBox(shell, SWT.OK);
            mb.setText("Warning");
            mb.setMessage("Climate History data is not available.");
            mb.open();
            return false;
        }
        return true;
    }

    /**
     * Create the file menu.
     */
    private void createFileMenu() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Quit menu item
        MenuItem quitMI = new MenuItem(fileMenu, SWT.NONE);
        quitMI.setText("&Quit");
        quitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the Ident (station name) list control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createIdentsList(Composite parentComp) {
        Composite identComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        identComp.setLayout(gl);
        identComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label identLbl = new Label(identComp, SWT.CENTER);
        identLbl.setText("Idents");
        identLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.widthHint = 60;
        identList = new List(identComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        identList.setLayoutData(gd);
        identList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                createTabsForStation();
            }
        });
    }

    /**
     * Create the tab folder that will display the individual graphs.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createChartTabFolder(Composite parentComp) {
        graphTabFolder = new TabFolder(parentComp, SWT.NONE);
    }

    /**
     * Populate the Ident (station name) list control.
     */
    private void populateIdentsList() {
        java.util.List<String> stationNames = climateHistData.getStationNames();

        for (String str : stationNames) {
            identList.add(str);
        }

        if (identList.getItemCount() > 0) {
            identList.select(0);
        }
    }

    /**
     * Create the tabs for specified Ident (station name).
     */
    private void createTabsForStation() {
        if (identList.getSelectionIndex() < 0) {
            return;
        }

        if (graphTabFolder.getItemCount() > 0) {
            removeAllTabs();
        }

        String selectedStation = identList.getItem(identList
                .getSelectionIndex());

        java.util.List<StationData> stationDataArray = climateHistData
                .getStationData(selectedStation);

        TabItem ti;

        for (StationData sd : stationDataArray) {
            ti = new TabItem(graphTabFolder, SWT.NONE);
            ti.setText(sd.getUsafWbanId());

            ti.setControl(getClimateCanvas(selectedStation, sd));
        }
    }

    /**
     * Create the graph canvas in a composite.
     * 
     * @param stationName
     *            Station name.
     * @param sd
     *            Station data.
     * @return Composite containing the graph canvas.
     */
    private Control getClimateCanvas(String stationName, StationData sd) {
        Composite canvasComp = new Composite(graphTabFolder, SWT.BORDER);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);

        new ClimateHistoryCanvas(canvasComp, stationName,
                sd.getObsGraphDataArray());

        canvasComp.pack();

        return canvasComp;
    }

    /**
     * Remove all of the tabs from the tab folder.
     */
    private void removeAllTabs() {
        TabItem[] tabs = graphTabFolder.getItems();

        for (int i = 0; i < tabs.length; i++) {
            tabs[i].getControl().dispose();
            tabs[i].dispose();
        }
    }
}