/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.dialog;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.awipstools.Activator;
import com.raytheon.viz.awipstools.ui.action.LapsToolsData;
import com.raytheon.viz.awipstools.ui.action.LapsToolsIO;
import com.raytheon.viz.awipstools.ui.layer.LapsToolLayer;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * LAPS Tools dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class LAPSToolsDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(LAPSToolsDlg.class);

    private final LapsToolsData data;

    /**
     * Main composite.
     */
    private Composite mainComp;

    /**
     * Label indicating which tool is selected.
     */
    private Label selectedToolLbl;

    /**
     * Current Analysis string.
     */
    private final String dataUsedByAnalysis = "Data Used by Current Analysis";

    /**
     * Configure Analysis string.
     */
    private final String configureAnalysis = "Configure Analysis Domain";

    /**
     * Flag indicating which tool is active.
     */
    private boolean isDataUsedByAnalysis = true;

    /**
     * Stack layout composite.
     */
    private Composite stackLayoutComp;

    /**
     * Current Analysis composite.
     */
    private Composite currentAnalysisComp;

    /**
     * Configure Analysis composite.
     */
    private Composite configureAnalysisComp;

    /**
     * Styled text control.
     */
    private StyledText stText;

    /**
     * Label font.
     */
    private Font selectToolLabelFont;

    /*
     * Spinner controls
     */
    private Spinner cenLatSpnr;

    private Spinner cenLonSpnr;

    private Spinner latSpnr;

    private Spinner lat2Spnr;

    private Spinner lonSpnr;

    private Spinner nxSpnr;

    private Spinner nySpnr;

    private Spinner dxmSpnr;

    private Spinner nzSpnr;

    private Spinner dpPaSpnr;

    private Spinner lowPaSpnr;

    /*
     * Settings buttons
     */
    private Button defaultBtn;

    private Button resetBtn;

    /*
     * LAPS Relocator buttons.
     */
    private Button loadBtn;

    private Button applyBtn;

    private Button localizeLapsBtn;

    /**
     * Stack layout.
     */
    private StackLayout stackLayout;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @throws VizException
     */
    public LAPSToolsDlg(Shell parent) throws VizException {
        super(parent, SWT.DIALOG_TRIM);

        try {
            LapsToolsIO.lockLaps();
            String failover = LapsToolsIO.getFailover();
            if (failover != null) {
                MessageDialog.openInformation(shell,
                        "LAPS is running in failover.", failover);
            }
            this.data = LapsToolsIO.loadData();
        } catch (VizException e) {
            MessageDialog
                    .openInformation(shell, "LAPS Tools GUI is not available.",
                            e.getLocalizedMessage());
            throw e;
        } catch (Exception e) {
            throw new VizException("Laps Dialog Failed to open", e);
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;

        return mainLayout;
    }

    /**
     * Method called when the shell get disposed.
     */
    protected void disposed() {
        selectToolLabelFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        selectToolLabelFont = new Font(this.getDisplay(), "Sans", 10, SWT.BOLD
                | SWT.ITALIC);

        mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        gl.horizontalSpacing = 2;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        createMenus();
        createMainControls();
    }

    /**
     * Create the menu bar and the menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createToolsMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
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

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (MessageDialog.openConfirm(shell, "Confirm Exit",
                        "Are you sure you want to exit LAPS Tools?")) {
                    shell.dispose();
                }
            }
        });
    }

    /**
     * Create the Tools menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createToolsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Location menu
        // -------------------------------------
        MenuItem toolsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        toolsMenuItem.setText("Tools");

        // Create the Location menu item with a Location "dropdown" menu
        Menu toolsMenu = new Menu(menuBar);
        toolsMenuItem.setMenu(toolsMenu);

        // -------------------------------------------------
        // Create all the items in the Location dropdown menu
        // -------------------------------------------------

        // Add Data Used by Current Analysis menu item
        MenuItem dataUsedCurrAnalysisMI = new MenuItem(toolsMenu, SWT.NONE);
        dataUsedCurrAnalysisMI.setText(dataUsedByAnalysis);
        dataUsedCurrAnalysisMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                isDataUsedByAnalysis = true;
                handleToolSelection();
            }
        });

        // Add Data Used by Current Analysis menu item
        MenuItem configAnalysisDomainMI = new MenuItem(toolsMenu, SWT.NONE);
        configAnalysisDomainMI.setText(configureAnalysis);
        configAnalysisDomainMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                isDataUsedByAnalysis = false;
                handleToolSelection();
            }
        });
    }

    /**
     * Create the Help menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the Help menu item with a Help "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // ------------------------------------------------------
        // Create all the items in the Help dropdown menu
        // ------------------------------------------------------

        // Administration menu item
        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("About...");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            }
        });
    }

    /**
     * Create the main controls.
     */
    private void createMainControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectedToolLbl = new Label(mainComp, SWT.NONE);
        selectedToolLbl.setText(dataUsedByAnalysis);
        selectedToolLbl.setLayoutData(gd);
        selectedToolLbl.setFont(selectToolLabelFont);
        updateSelectedToolLabel();

        // Add a separator.
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(mainComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the stack layout composite
        stackLayoutComp = new Composite(shell, SWT.NONE);
        stackLayout = new StackLayout();
        stackLayoutComp.setLayout(stackLayout);

        createCurrentAnalysisComp(stackLayoutComp);
        createConfigAnalysisComp(stackLayoutComp);

        stackLayout.topControl = currentAnalysisComp;
        stackLayoutComp.layout();
    }

    /**
     * Create the composite and controls for the current analysis.
     * 
     * @param stackLayoutComp
     *            Stack layout composite.
     */
    private void createCurrentAnalysisComp(Composite stackLayoutComp) {
        currentAnalysisComp = new Composite(stackLayoutComp, SWT.NONE);
        currentAnalysisComp.setLayout(new GridLayout(1, false));
        currentAnalysisComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        Composite controlComp = new Composite(currentAnalysisComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(3, false));
        controlComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        Label selectTypeLbl = new Label(controlComp, SWT.NONE);
        selectTypeLbl.setText("Select Type: ");

        Combo typeCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateTypeCombo(typeCbo);
        typeCbo.select(0);
        typeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Combo c = (Combo) e.widget;
                typeAction((c.getItem(c.getSelectionIndex())));
            }
        });

        Button clearBtn = new Button(controlComp, SWT.PUSH);
        clearBtn.setText(" Clear ");
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clearAction();
            }
        });

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 500;
        gd.heightHint = 300;
        stText = new StyledText(currentAnalysisComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        stText.setLayoutData(gd);
    }

    /**
     * Create the composite and controls for the configuration analysis.
     * 
     * @param stackLayoutComp
     *            Stack layout composite.
     */
    private void createConfigAnalysisComp(Composite stackLayoutComp) {
        configureAnalysisComp = new Composite(stackLayoutComp, SWT.NONE);
        configureAnalysisComp.setLayout(new GridLayout(1, false));
        configureAnalysisComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        createProjectionGroup();
        createGridGroup();
        createSettingsLapsGroups();
        populateSpinners();

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        gd.verticalIndent = 15;
        localizeLapsBtn = new Button(configureAnalysisComp, SWT.PUSH);
        localizeLapsBtn.setText("Localize LAPS");
        localizeLapsBtn.setLayoutData(gd);
        localizeLapsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                localizeLapsAction();
            }
        });
    }

    /**
     * Create the projection group.
     */
    private void createProjectionGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group projectionGroup = new Group(configureAnalysisComp, SWT.NONE);
        projectionGroup.setLayout(new GridLayout(7, false));
        projectionGroup.setLayoutData(gd);
        projectionGroup.setText(" Projection ");

        /*
         * Projection label
         */
        gd = new GridData(120, SWT.DEFAULT);
        Label projectionStrLbl = new Label(projectionGroup, SWT.CENTER);
        projectionStrLbl.setText("PolarStr");
        projectionStrLbl.setLayoutData(gd);

        /*
         * Cen Lat
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 20;
        gd.widthHint = 60;
        Label cenLatLbl = new Label(projectionGroup, SWT.NONE);
        cenLatLbl.setText("Cen Lat: ");
        cenLatLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        cenLatSpnr = new Spinner(projectionGroup, SWT.BORDER);
        cenLatSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                readSpinners();
                populateSpinners();
            }
        });
        cenLatSpnr.setDigits(4);
        cenLatSpnr.setMinimum(-900000);
        cenLatSpnr.setMaximum(900000);
        cenLatSpnr.setIncrement(1000);
        cenLatSpnr.setLayoutData(gd);

        /*
         * Cen Lon
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 5;
        gd.widthHint = 60;
        Label cenLonLbl = new Label(projectionGroup, SWT.NONE);
        cenLonLbl.setText("Cen Lon: ");
        cenLonLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        cenLonSpnr = new Spinner(projectionGroup, SWT.BORDER);
        cenLonSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                readSpinners();
                populateSpinners();
            }
        });
        cenLonSpnr.setDigits(4);
        cenLonSpnr.setMinimum(-1800000);
        cenLonSpnr.setMaximum(1800000);
        cenLonSpnr.setIncrement(1000);
        cenLonSpnr.setLayoutData(gd);

        // 3 Filler Label
        new Label(projectionGroup, SWT.NONE);
        new Label(projectionGroup, SWT.NONE);
        new Label(projectionGroup, SWT.NONE);

        /*
         * Lat
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        Label latLbl = new Label(projectionGroup, SWT.NONE);
        latLbl.setText("Lat: ");
        latLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        latSpnr = new Spinner(projectionGroup, SWT.BORDER);
        latSpnr.setDigits(4);
        latSpnr.setMinimum(-900000);
        latSpnr.setMaximum(900000);
        latSpnr.setIncrement(1000);
        latSpnr.setEnabled(false);
        latSpnr.setLayoutData(gd);

        /*
         * Lat2
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        Label lat2Lbl = new Label(projectionGroup, SWT.NONE);
        lat2Lbl.setText("Lat2: ");
        lat2Lbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        lat2Spnr = new Spinner(projectionGroup, SWT.BORDER);
        lat2Spnr.setDigits(4);
        lat2Spnr.setMinimum(-900000);
        lat2Spnr.setMaximum(900000);
        lat2Spnr.setIncrement(1000);
        lat2Spnr.setEnabled(false);
        lat2Spnr.setLayoutData(gd);

        /*
         * Lon
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.widthHint = 60;
        Label lonLbl = new Label(projectionGroup, SWT.RIGHT);
        lonLbl.setText("Lon: ");
        lonLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        lonSpnr = new Spinner(projectionGroup, SWT.BORDER);
        lonSpnr.setDigits(4);
        lonSpnr.setMinimum(-1800000);
        lonSpnr.setMaximum(1800000);
        lonSpnr.setIncrement(1000);
        lonSpnr.setEnabled(false);
        lonSpnr.setLayoutData(gd);
    }

    /**
     * Create the grid group.
     */
    private void createGridGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group gridGroup = new Group(configureAnalysisComp, SWT.NONE);
        gridGroup.setLayout(new GridLayout(7, false));
        gridGroup.setLayoutData(gd);
        gridGroup.setText(" Grid ");

        /*
         * Horizontal label
         */
        gd = new GridData(120, SWT.DEFAULT);
        Label horizontalLbl = new Label(gridGroup, SWT.CENTER);
        horizontalLbl.setText("Horizontal");
        horizontalLbl.setLayoutData(gd);

        /*
         * Nx
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.horizontalIndent = 20;
        gd.widthHint = 60;
        Label nxLbl = new Label(gridGroup, SWT.RIGHT);
        nxLbl.setText("Nx: ");
        nxLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        nxSpnr = new Spinner(gridGroup, SWT.BORDER);
        nxSpnr.setDigits(0);
        nxSpnr.setMinimum(1);
        nxSpnr.setMaximum(100);
        nxSpnr.setIncrement(1);
        nxSpnr.setEnabled(false);
        nxSpnr.setLayoutData(gd);

        /*
         * Ny
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.horizontalIndent = 5;
        gd.widthHint = 60;
        Label nyLbl = new Label(gridGroup, SWT.RIGHT);
        nyLbl.setText("Ny: ");
        nyLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        nySpnr = new Spinner(gridGroup, SWT.BORDER);
        nySpnr.setDigits(0);
        nySpnr.setMinimum(1);
        nySpnr.setMaximum(100);
        nySpnr.setIncrement(1);
        nySpnr.setEnabled(false);
        nySpnr.setLayoutData(gd);

        /*
         * Dx(m)
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.widthHint = 60;
        Label dxmLbl = new Label(gridGroup, SWT.RIGHT);
        dxmLbl.setText("Dx(m): ");
        dxmLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        dxmSpnr = new Spinner(gridGroup, SWT.BORDER);
        dxmSpnr.setDigits(0);
        dxmSpnr.setMinimum(1000);
        dxmSpnr.setMaximum(100000);
        dxmSpnr.setIncrement(1000);
        dxmSpnr.setEnabled(false);
        dxmSpnr.setLayoutData(gd);

        /*
         * Vertical label
         */
        gd = new GridData(120, SWT.DEFAULT);
        Label verticalLbl = new Label(gridGroup, SWT.CENTER);
        verticalLbl.setText("Vertical");
        verticalLbl.setLayoutData(gd);

        /*
         * Nz
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        Label nzLbl = new Label(gridGroup, SWT.NONE);
        nzLbl.setText("Nz: ");
        nzLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        nzSpnr = new Spinner(gridGroup, SWT.BORDER);
        nzSpnr.setDigits(0);
        nzSpnr.setMinimum(1);
        nzSpnr.setMaximum(100);
        nzSpnr.setIncrement(1);
        nzSpnr.setEnabled(false);
        nzSpnr.setLayoutData(gd);

        /*
         * Dp(Pa)
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        Label dppaLbl = new Label(gridGroup, SWT.NONE);
        dppaLbl.setText("Dp(Pa): ");
        dppaLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        dpPaSpnr = new Spinner(gridGroup, SWT.BORDER);
        dpPaSpnr.setDigits(0);
        dpPaSpnr.setMinimum(0);
        dpPaSpnr.setMaximum(200);
        dpPaSpnr.setIncrement(10);
        dpPaSpnr.setEnabled(false);
        dpPaSpnr.setLayoutData(gd);

        /*
         * Low(Pa)
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        Label lowPaLbl = new Label(gridGroup, SWT.RIGHT);
        lowPaLbl.setText("Low(Pa): ");
        lowPaLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        lowPaSpnr = new Spinner(gridGroup, SWT.BORDER);
        lowPaSpnr.setDigits(0);
        lowPaSpnr.setMinimum(0);
        lowPaSpnr.setMaximum(1050);
        lowPaSpnr.setIncrement(10);
        lowPaSpnr.setEnabled(false);
        lowPaSpnr.setLayoutData(gd);
    }

    /**
     * Create the Setting and LAPS Relocator groups.
     */
    private void createSettingsLapsGroups() {
        int buttonWidth = 120;

        GridLayout gl = new GridLayout(2, true);
        gl.marginWidth = 0;

        Composite groupComp = new Composite(configureAnalysisComp, SWT.NONE);
        groupComp.setLayout(gl);
        groupComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        /*
         * Settings
         */
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group settingsGroup = new Group(groupComp, SWT.NONE);
        settingsGroup.setLayout(new GridLayout(2, true));
        settingsGroup.setLayoutData(gd);
        settingsGroup.setText(" Settings ");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        defaultBtn = new Button(settingsGroup, SWT.PUSH);
        defaultBtn.setText("Default");
        defaultBtn.setLayoutData(gd);
        defaultBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setDefaultDomain();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        resetBtn = new Button(settingsGroup, SWT.PUSH);
        resetBtn.setText("Reset");
        resetBtn.setLayoutData(gd);
        resetBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetDomain();

            }
        });

        /*
         * LAPS Relocator
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group lapsRelocatorGroup = new Group(groupComp, SWT.NONE);
        lapsRelocatorGroup.setLayout(new GridLayout(2, true));
        lapsRelocatorGroup.setLayoutData(gd);
        lapsRelocatorGroup.setText(" LAPS Relocator ");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        loadBtn = new Button(lapsRelocatorGroup, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadAction();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(lapsRelocatorGroup, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.setEnabled(false);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                applyAction();
            }
        });
    }

    /**
     * Handle the selection when switching tools.
     */
    private void handleToolSelection() {
        updateSelectedToolLabel();

        if (isDataUsedByAnalysis) {
            stackLayout.topControl = currentAnalysisComp;
            stackLayoutComp.layout();
        } else {
            stackLayout.topControl = configureAnalysisComp;
            stackLayoutComp.layout();
        }
    }

    /**
     * Update the Tool label indicating which tool is selected.
     */
    private void updateSelectedToolLabel() {
        if (isDataUsedByAnalysis) {
            selectedToolLbl.setText("Tool: " + dataUsedByAnalysis);
        } else {
            selectedToolLbl.setText("Tool: " + configureAnalysis);
        }
    }

    /**
     * Action for the clear button.
     */
    private void clearAction() {
        stText.setText("");
    }

    private void populateTypeCombo(Combo combo) {
        for (String choice : LapsToolsIO.getDataChoices()) {
            combo.add(choice);
        }
    }

    private void typeAction(String type) {
        try {
            stText.append(LapsToolsIO.getLogs(type));
            stText.append("\n");
            stText.append("__________________________________________\n");
            stText.setTopIndex(stText.getLineCount());
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    ex.getLocalizedMessage(), ex);
        }
    }

    private void populateSpinners() {
        configureSpinner(latSpnr, data.getGridCenter().y);
        configureSpinner(lat2Spnr, data.getLat2());
        configureSpinner(lonSpnr, data.getLon());
        configureSpinner(cenLatSpnr, data.getGridCenter().y, data
                .getValidArea().getMinY(), data.getValidArea().getMaxY());
        configureSpinner(cenLonSpnr, data.getGridCenter().x, data
                .getValidArea().getMinX(), data.getValidArea().getMaxX());
        configureSpinner(nxSpnr, data.getNx());
        configureSpinner(nySpnr, data.getNy());
        configureSpinner(dxmSpnr, data.getGridSpacing());
        configureSpinner(nzSpnr, data.getNz());
        configureSpinner(dpPaSpnr, data.getDp());
        configureSpinner(lowPaSpnr, data.getLowp());
    }

    private void readSpinners() {
        data.setGridCenterLat(readSpinner(cenLatSpnr));
        data.setLat(readSpinner(cenLatSpnr));
        data.setGridCenterLon(readSpinner(cenLonSpnr));
    }

    private Double readSpinner(Spinner spinner) {
        double multiplier = Math.pow(10, spinner.getDigits());
        return spinner.getSelection() / multiplier;
    }

    private void configureSpinner(Spinner spinner, Number value) {
        configureSpinner(spinner, value, null, null);
    }

    private void configureSpinner(Spinner spinner, Number value, Double min,
            Double max) {
        double multiplier = Math.pow(10, spinner.getDigits());
        if (value != null) {
            spinner.setSelection((int) (value.doubleValue() * multiplier));
        } else {
            spinner.setSelection(0);
        }
        if (min != null) {
            spinner.setMinimum((int) (min * multiplier));
        }
        if (max != null) {
            spinner.setMaximum((int) (max * multiplier));
        }
    }

    private void setDefaultDomain() {
        boolean ok = MessageDialog
                .openConfirm(getShell(), "Confirm Exit",
                        "This will reset all variables to the default WFO localization values.");
        if (ok) {
            try {
                LapsToolsIO.readDefaultParmsFile(data);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
                e.printStackTrace();
            }
            populateSpinners();
        }
    }

    private void resetDomain() {
        boolean ok = MessageDialog
                .openConfirm(getShell(), "Confirm Exit",
                        "This will reset all variables to the existing localization values.");
        if (ok) {
            try {
                LapsToolsIO.readParmsFile(data);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
            populateSpinners();
        }
    }

    private void applyAction() {
        cenLatSpnr.setEnabled(true);
        cenLonSpnr.setEnabled(true);
        applyBtn.setEnabled(false);
        loadBtn.setEnabled(true);
        resetBtn.setEnabled(true);
        defaultBtn.setEnabled(true);
        localizeLapsBtn.setEnabled(true);
        populateSpinners();

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDisplayPane[] displayPanes = container.getDisplayPanes();
            for (IDisplayPane pane : displayPanes) {
                IDescriptor desc = pane.getDescriptor();
                for (ResourcePair pair : desc.getResourceList()) {
                    if (pair.getResource() instanceof LapsToolLayer) {
                        desc.getResourceList().remove(pair);
                    }
                }
            }
        }
    }

    private void loadAction() {
        cenLatSpnr.setEnabled(false);
        cenLonSpnr.setEnabled(false);
        applyBtn.setEnabled(true);
        loadBtn.setEnabled(false);
        resetBtn.setEnabled(false);
        defaultBtn.setEnabled(false);
        localizeLapsBtn.setEnabled(false);
        readSpinners();
        GenericToolsResourceData<LapsToolLayer> rd = new GenericToolsResourceData<LapsToolLayer>(
                LapsToolLayer.DEFAULT_NAME, LapsToolLayer.class);

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        IDisplayPane[] displayPanes = container.getDisplayPanes();
        if (container instanceof IMultiPaneEditor) {
            IDisplayPane selected = ((IMultiPaneEditor) container)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            if (selected != null) {
                displayPanes = new IDisplayPane[] { selected };
            }
        }

        for (IDisplayPane pane : displayPanes) {
            IDescriptor desc = pane.getDescriptor();
            LoadProperties lp = new LoadProperties();
            lp.getCapabilities().addCapability(EditableCapability.class);
            try {
                LapsToolLayer rsc = rd.construct(lp, desc);
                rsc.setData(data);
                desc.getResourceList().add(rsc);
                rsc.getCapability(EditableCapability.class).setEditable(true);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    private void localizeLapsAction() {
        if (MessageDialog.openQuestion(getShell(), "Confirmation",
                LapsToolsIO.getLocalizationQuestion())) {
            try {
                LapsToolsIO.localize(data);
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Initiated LAPS Localization");
                close();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }
}
