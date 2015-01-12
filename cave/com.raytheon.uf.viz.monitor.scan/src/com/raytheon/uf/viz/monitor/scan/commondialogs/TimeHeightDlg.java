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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Time-Height Graph dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010            lvenable     Initial creation
 * Dec 23, 2011	13608	   mgamazay		Updated populateIdentCombo so the drop down menu
 * 										shows the current feature ident instead of being blank.
 * 24 Jul 2013  #2143      skorolev     Changes for non-blocking dialogs.
 * Aug 15, 2013  2143      mpduff       Remove resize.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TimeHeightDlg extends CaveSWTDialog implements ITimeHeightInfo {

    /**
     * The time height graph.
     */
    private TimeHeightGraph timeHeightGraph;

    /**
     * Ident combo control.
     */
    private Combo identCbo;

    /**
     * Attribute combo control.
     */
    private Combo attrCbo;

    /**
     * Check box to toggle the ellipses on/off.
     */
    private Button diamOverlayChk;

    /**
     * Check box to toggle the elevation angles on/off.
     */
    private Button elevAnglesChk;

    /**
     * Check box to toggle the Volume SCAN Poles
     */
    private Button volScanChk;

    /**
     * Check box to toggle the Legend
     */
    private Button legendChk;

    /**
     * Class containing information on which settings to draw on the graph.
     */
    private DrawSettings drawSettings;

    /**
     * Array of idents
     */
    private String[] identArray;

    /**
     * Current ident.
     */
    private String ident;

    /**
     * Current attribute.
     */
    private String attrName;

    /**
     * Scan table identifier.
     */
    private final ScanTables scanTable;

    /**
     * Time-Height graph data.
     */
    private final TreeMap<Long, DMDTableDataRow> graphData;

    /**
     * Callback to request time height data.
     */
    private final IRequestTimeHeightData timeHeightCB;

    /**
     * List of attributes that can display time-height data.
     */
    private ArrayList<String> attrList;

    private TimeHeightMsgBox msgBox = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param scanTable
     *            Scan table identifier.
     * @param ident
     *            Currently selected ident.
     * @param attrName
     *            Currently selected attribute name.
     * @param identArray
     *            Array of available idents.
     * @param graphData
     *            Data to graph.
     * @param timeHeightCB
     *            Callback to request time height data.
     */
    public TimeHeightDlg(Shell parentShell, ScanTables scanTable, String ident,
            String attrName, String[] identArray,
            TreeMap<Long, DMDTableDataRow> graphData,
            IRequestTimeHeightData timeHeightCB) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("DMD Time-Height Trend");

        this.scanTable = scanTable;
        this.identArray = identArray;
        this.ident = ident;
        this.attrName = attrName;
        this.graphData = graphData;
        this.timeHeightCB = timeHeightCB;
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
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createTopControls();
        createTimeHeightCanvas();

        timeHeightGraph.setGraphData(graphData);
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls() {
        drawSettings = timeHeightCB.getDrawSettings();
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(9, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        identCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        identCbo.setLayoutData(gd);
        identCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleIdentAttributeComboAction();
            }
        });

        // Populate the ident combo control
        populateIdentCombo();

        gd = new GridData(140, SWT.DEFAULT);
        attrCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        attrCbo.setLayoutData(gd);
        attrCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleIdentAttributeComboAction();
            }
        });

        // Populate the attributes combo control
        populateAttributesCombo();

        gd = new GridData();
        gd.horizontalIndent = 40;
        diamOverlayChk = new Button(controlComp, SWT.CHECK);
        diamOverlayChk.setText("Diameter Overlay");
        diamOverlayChk.setSelection(drawSettings.diamOverlay);
        diamOverlayChk.setLayoutData(gd);
        diamOverlayChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                diamOverlayAction();
                drawSettings.diamOverlay = diamOverlayChk.getSelection();
                timeHeightCB.setDrawSettings(drawSettings);
            }
        });

        elevAnglesChk = new Button(controlComp, SWT.CHECK);
        elevAnglesChk.setText("Elevation Angles");
        elevAnglesChk.setSelection(drawSettings.elevAngles);
        elevAnglesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                drawSettings.elevAngles = elevAnglesChk.getSelection();
                timeHeightGraph.setDrawSettings(drawSettings);
                timeHeightCB.setDrawSettings(drawSettings);
            }
        });

        volScanChk = new Button(controlComp, SWT.CHECK);
        volScanChk.setText("Vol Scan Poles");
        volScanChk.setSelection(drawSettings.volScanPoles);
        volScanChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                drawSettings.volScanPoles = volScanChk.getSelection();
                timeHeightGraph.setDrawSettings(drawSettings);
                timeHeightCB.setDrawSettings(drawSettings);
            }
        });

        legendChk = new Button(controlComp, SWT.CHECK);
        legendChk.setText("Legend");
        legendChk.setSelection(drawSettings.legend);
        legendChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                drawSettings.legend = legendChk.getSelection();
                timeHeightGraph.setDrawSettings(drawSettings);
                timeHeightCB.setDrawSettings(drawSettings);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Create the time height canvas.
     */
    private void createTimeHeightCanvas() {
        timeHeightGraph = new TimeHeightGraph(shell, drawSettings, this);
    }

    /**
     * Populate the ident combo control.
     */
    private void populateIdentCombo() {

        if (Arrays.asList(identArray).contains(ident)) {
            for (String str : identArray) {
                identCbo.add(str);
            }
            identCbo.select(identCbo.indexOf(ident));
        }

        else {
            identCbo.add(ident);
            identCbo.select(identCbo.indexOf(ident));
        }

    }

    /**
     * Populate the attributes combo control.
     */
    private void populateAttributesCombo() {
        attrList = new ArrayList<String>();

        SCANConfig scanCfg = SCANConfig.getInstance();
        LinkedHashMap<String, String> trendAttrNames = scanCfg
                .getTimeHeightAttributeUnits(scanTable);

        for (String name : trendAttrNames.keySet()) {
            attrList.add(name);
            attrCbo.add(name + " (" + trendAttrNames.get(name) + ")");
        }

        attrCbo.select(attrList.indexOf(attrName));
    }

    /**
     * Handle the action when an attribute is selected from the attribute combo
     * control.
     */
    private void handleIdentAttributeComboAction() {
        ident = identCbo.getItem(identCbo.getSelectionIndex());
        attrName = attrList.get(attrCbo.getSelectionIndex());

        SCANConfigEnums.DMDTable tableCol = SCANConfigEnums.DMDTable
                .valueOf(attrName.toUpperCase());

        TreeMap<Long, DMDTableDataRow> data = timeHeightCB
                .requestTimeHeightData(tableCol, ident);

        this.setGraphData(data);
    }

    /**
     * If the diamOverlay is checked: Draw the ellipses and enable the legend
     * button and if the legend button is checked then draw the legend.
     * 
     * If the diamOverlay is not checked: Do not draw the ellipses and disable
     * the legend button and if the legend button is checked then skip drawing
     * the legend.
     */
    private void diamOverlayAction() {
        drawSettings.diamOverlay = diamOverlayChk.getSelection();

        if (drawSettings.diamOverlay == true) {
            legendChk.setEnabled(true);
            if (legendChk.getSelection() == true) {
                drawSettings.legend = true;
            }
        } else {
            legendChk.setEnabled(false);
            drawSettings.legend = false;
        }

        timeHeightGraph.setDrawSettings(drawSettings);
    }

    /**
     * Get the ident.
     * 
     * @return The ident.
     */
    public String getIdent() {
        return ident;
    }

    /**
     * Get the current attribute (table column).
     * 
     * @return The attribute (table column).
     */
    public SCANConfigEnums.DMDTable getTableColumn() {
        SCANConfigEnums.DMDTable tableCol = SCANConfigEnums.DMDTable
                .valueOf(attrName.toUpperCase());
        return tableCol;
    }

    /**
     * Set the graph data.
     * 
     * @param data
     *            The graph data.
     * @param ident
     *            Selected ident.
     * @param attribute
     *            Selected attribute (table column).
     */
    public void setGraphData(TreeMap<Long, DMDTableDataRow> data, String ident,
            String attribute) {
        this.ident = ident;
        identCbo.select(identCbo.indexOf(this.ident));

        this.attrName = attribute;
        attrCbo.select(attrList.indexOf(this.attrName));

        timeHeightGraph.setGraphData(data);
    }

    /**
     * Set the graph data for the selected attribute and ident.
     * 
     * @param data
     */
    public void setGraphData(TreeMap<Long, DMDTableDataRow> data) {
        timeHeightGraph.setGraphData(data);
    }

    /**
     * Display a Close Dialog message when cell is no longer valid.
     */
    public void displayMessage() {
        if (this.msgBox == null || msgBox.isDisposed()) {
            msgBox = new TimeHeightMsgBox(getShell(), this.ident);
            msgBox.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        if (returnValue.equals("OK")) {
                            close();
                        } else {
                            return;
                        }
                    }
                    msgBox = null;
                }

            });
            msgBox.open();
        } else {
            msgBox.bringToTop();
        }
    }

    /**
     * Check if the shell is disposed.
     * 
     * @return True if the shell is disposed.
     */
    public boolean shellDisposed() {
        return shell.isDisposed();
    }

    /**
     * Get the current attribute name.
     */
    @Override
    public String getCurrentAttribute() {
        return attrName;
    }

    /**
     * Force the graph to redraw.
     */
    public void redrawGraph() {
        if (timeHeightGraph != null) {
            timeHeightGraph.redrawCanvas();
        }
    }

    /**
     * @param identArray
     *            the identArray to set
     */
    public void setIdentArray(String[] identArray) {
        this.identArray = identArray;
        this.identCbo.removeAll();
        populateIdentCombo();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.ITimeHeightInfo#getDialogTime
     * ()
     */
    @Override
    public Date getDialogTime() {
        return timeHeightCB.getDialogTime();
    }
}
