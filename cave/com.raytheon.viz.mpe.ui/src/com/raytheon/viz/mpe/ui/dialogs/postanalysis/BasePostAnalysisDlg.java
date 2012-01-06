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
package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This is the base dialog class for the Post Analysis dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class BasePostAnalysisDlg extends CaveSWTDialog {

    /**
     * Maps composite.
     */
    protected MapsComp mapsComp = null;

    /**
     * Legend canvas.
     */
    protected Canvas legendCanvas = null;

    /**
     * Canvas height.
     */
    protected int canvasHeight = 75;

    /**
     * Canvas width.
     */
    protected int canvasWidth = 700;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public BasePostAnalysisDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void disposed() {
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
        createMenus();
        createMapLabels();
        createMapComposite();
        createLegendCanvas();
        addBottomControls();
    }

    /**
     * Create the menubar and the menu items.
     */
    public void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createControlMenu(menuBar);
        createOptionsMenu(menuBar);
        createOverlaysMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the Control menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createControlMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Control menu
        // -------------------------------------
        MenuItem controlMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        controlMenuItem.setText("&Control");

        // Create the Control menu item with a Control "dropdown" menu
        Menu controlMenu = new Menu(menuBar);
        controlMenuItem.setMenu(controlMenu);

        /*
         * Create the custom control menu items
         */
        createControlMenuItem(controlMenu);

        /*
         * Create all the items in the Control dropdown menu
         */
        MenuItem closeMI = new MenuItem(controlMenu, SWT.NONE);
        closeMI.setText("Close");
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Options menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Options menu
        // -------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("&Options");

        // Create the Options menu item with a Options "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        /*
         * Create all the items in the Options dropdown menu
         */
        MenuItem zoomResetMI = new MenuItem(optionsMenu, SWT.NONE);
        zoomResetMI.setText("Zoom Reset");
        zoomResetMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                mapsComp.resetZoom();
            }
        });

        MenuItem samplingMI = new MenuItem(optionsMenu, SWT.CHECK);
        samplingMI.setText("Sampling");
        samplingMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

            }
        });
    }

    /**
     * Create the Overlay menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOverlaysMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Overlays menu
        // -------------------------------------
        MenuItem overlaysMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        overlaysMenuItem.setText("O&verlays");

        // Create the Overlays menu item with a Overlays "dropdown" menu
        Menu overlaysMenu = new Menu(menuBar);
        overlaysMenuItem.setMenu(overlaysMenu);

        /*
         * Create all the items in the Overlays dropdown menu
         */
        MenuItem statesMI = new MenuItem(overlaysMenu, SWT.CHECK);
        statesMI.setText("States");
        statesMI.setSelection(true);
        statesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem countyMI = new MenuItem(overlaysMenu, SWT.CHECK);
        countyMI.setText("County");
        countyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem citiesTownsMI = new MenuItem(overlaysMenu, SWT.CHECK);
        citiesTownsMI.setText("Cities/Towns");
        citiesTownsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem basinBoundariesMI = new MenuItem(overlaysMenu, SWT.CHECK);
        basinBoundariesMI.setText("Basin Boundaries");
        basinBoundariesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem riversMI = new MenuItem(overlaysMenu, SWT.CASCADE);
        riversMI.setText("Rivers");

        Menu riversSubMenu = new Menu(shell, SWT.DROP_DOWN);
        riversMI.setMenu(riversSubMenu);

        MenuItem allRiversMI = new MenuItem(riversSubMenu, SWT.CHECK);
        allRiversMI.setText("All Rivers");
        allRiversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem majorRiversMI = new MenuItem(riversSubMenu, SWT.CHECK);
        majorRiversMI.setText("Major Rivers");
        majorRiversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });
    }

    /**
     * Create the labels that will appear above the maps.
     */
    private void createMapLabels() {
        String[] lblNames = getMapLabelNames();

        if (lblNames == null || lblNames.length == 0) {
            return;
        }

        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(2, true));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label leftMapLbl = new Label(labelComp, SWT.CENTER);
        leftMapLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label rightMapLbl = new Label(labelComp, SWT.CENTER);
        rightMapLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        if (lblNames.length >= 2) {
            leftMapLbl.setText(lblNames[0]);
            rightMapLbl.setText(lblNames[1]);
        } else {
            leftMapLbl.setText(lblNames[0]);
            rightMapLbl.setText("Unknown");
        }
    }

    /**
     * Create the map composite.
     */
    private void createMapComposite() {
        mapsComp = new MapsComp(shell);
    }

    /**
     * Create the color bar legend canvas.
     */
    private void createLegendCanvas() {
        Composite canvasComp = new Canvas(shell, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));
        canvasComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));

        canvasHeight = canvasHeight * getNumberOfColorLegends();

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = canvasWidth;
        gd.heightHint = canvasHeight;
        legendCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        legendCanvas.setSize(canvasWidth, canvasHeight);
        legendCanvas.setLayoutData(gd);

        legendCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });
    }

    /**
     * Toggle the map overlay.
     * 
     * @param mi
     *            Menu item selected.
     */
    private void toggleOverlay(MenuItem mi) {
        mapsComp.toggleOverlay(mi.getSelection(), mi.getText());
    }

    /**
     * Draw the legend canvas.
     * 
     * @param gc
     */
    private void drawCanvas(GC gc) {
        gc.setAntialias(SWT.ON);
        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        gc.fillRectangle(0, 0, canvasWidth, canvasHeight);
    }

    /**
     * Create custom menu items for the control menu.
     * 
     * @param controlMenu
     *            The control menu.
     */
    abstract protected void createControlMenuItem(Menu controlMenu);

    /**
     * Get the names for the left and right map labels.
     * 
     * @return String aray of names.
     */
    abstract protected String[] getMapLabelNames();

    /**
     * This will allow classes extending this class to add controls/composites
     * at the bottom of the dialog.
     */
    abstract protected void addBottomControls();

    /**
     * Get the number of color bars to determine the height of the color legend.
     * 
     * @return Number of color legend color bars.
     */
    abstract protected int getNumberOfColorLegends();
}
