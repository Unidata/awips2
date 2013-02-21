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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Bandwidth Utilization Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012    1269    mpduff      Initial creation.
 * Dec 13, 2012   1269     lvenable    Fixes and updates.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BandwidthUtilizationDlg extends CaveSWTDialog {

    /** Menu bar */
    private Menu menuBar;

    /** Live update menu */
    private MenuItem liveUpdateMI;

    /** Color by priority menu */
    private MenuItem colorByPriorityMI;

    /** Show subscription lines menu */
    private MenuItem showSubLinesMI;

    /** Graph composite */
    private BandwidthCanvasComp canvasComp;

    /** Graph data utility class */
    private GraphDataUtil graphDataUtil;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell
     */
    public BandwidthUtilizationDlg(Shell parent, GraphDataUtil graphDataUtil) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText("Bandwidth Utilization");

        this.graphDataUtil = graphDataUtil;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.verticalSpacing = 0;
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createMenus();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        GridLayout gl = new GridLayout(1, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        canvasComp = new BandwidthCanvasComp(mainComp, graphDataUtil);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gl = new GridLayout(1, false);
        Composite btnComp = new Composite(mainComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        Button closeBtn = new Button(btnComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(new GridData(75, SWT.DEFAULT));
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Create the menus
     */
    private void createMenus() {
        menuBar = new Menu(shell, SWT.BAR);
        createFileMenu(menuBar);
        createGraphMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the file menu.
     * 
     * @param menuBar
     */
    private void createFileMenu(Menu menuBar) {
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        MenuItem saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("&Save\tCtrl+S");
        saveMI.setAccelerator(SWT.CTRL + 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // saveGraph(); // TODO - implement this
            }
        });

        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("&Quit\tCtrl+Q");
        exitMI.setAccelerator(SWT.CTRL + 'Q');
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Create the graph menu.
     * 
     * @param menuBar
     */
    private void createGraphMenu(Menu menuBar) {
        MenuItem graphMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        graphMenuItem.setText("Graph");

        Menu graphMenu = new Menu(menuBar);
        graphMenuItem.setMenu(graphMenu);

        liveUpdateMI = new MenuItem(graphMenu, SWT.CHECK);
        liveUpdateMI.setText("Live Update");
        liveUpdateMI.setSelection(true);
        liveUpdateMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                canvasComp.setLiveUpdate(liveUpdateMI.getSelection());
            }
        });

        colorByPriorityMI = new MenuItem(graphMenu, SWT.CHECK);
        colorByPriorityMI.setText("Color By Priority");
        colorByPriorityMI.setSelection(true);
        colorByPriorityMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                canvasComp.setColorByPriority(colorByPriorityMI.getSelection());
            }
        });

        new MenuItem(graphMenu, SWT.SEPARATOR);

        showSubLinesMI = new MenuItem(graphMenu, SWT.CHECK);
        showSubLinesMI.setText("Show Subscription Lines");
        showSubLinesMI.setSelection(false);
        showSubLinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                canvasComp.setShowSubscriptionLines(showSubLinesMI
                        .getSelection());
            }

        });
    }

    /**
     * Redraw the graph canvases
     */
    public void redrawGraph() {
        canvasComp.updateCanvases();
    }
}
