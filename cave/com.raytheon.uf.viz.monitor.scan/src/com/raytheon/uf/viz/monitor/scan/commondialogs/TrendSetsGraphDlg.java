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

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

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

import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.TrendSetConfigMgr;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Scan/DMD Trend Sets Graph Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2013   1812        mpduff   Redraw now updates with new data.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TrendSetsGraphDlg extends CaveSWTDialog {

    private final ScanTables scanTable;

    private Combo identCbo;

    private Combo trendSetCbo;

    private String ident;

    private String trendSetName;

    private TrendSetConfigMgr trendCfgMgr;

    private final ITrendSetsGraphUpdate updateCallback;

    private final IRequestTrendGraphData requestDataCallback;

    // private LinkedHashMap<Date, Double> dataMap;

    private LinkedHashMap<String, TrendGraphData> trendSetData;

    private final String[] identArray;

    private String[] attrArray;

    private Composite canvasesComp = null;

    private HashMap<String, TrendGraphCanvas> canvasMap;

    private final Integer vcp;

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param scanTable
     * @param ident
     * @param trendSetName
     * @param updateCallback
     * @param requestDataCallback
     * @param identArray
     * @param vcp
     */
    public TrendSetsGraphDlg(Shell parentShell, ScanTables scanTable,
            String ident, String trendSetName,
            ITrendSetsGraphUpdate updateCallback,
            IRequestTrendGraphData requestDataCallback, String[] identArray,
            Integer vcp) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText(scanTable.name() + " Trend Graph");

        this.scanTable = scanTable;
        this.ident = ident;
        this.trendSetName = trendSetName;
        this.updateCallback = updateCallback;
        this.requestDataCallback = requestDataCallback;
        this.identArray = identArray;
        this.vcp = vcp;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        trendSetData = new LinkedHashMap<String, TrendGraphData>();
        canvasMap = new HashMap<String, TrendGraphCanvas>();

        trendCfgMgr = SCANConfig.getInstance().getTrendConfigMgr(scanTable);
        attrArray = trendCfgMgr.getAttributes(trendSetName);

        /*
         * need to have an array of data maps for the multiple canvases
         */
        for (String attr : attrArray) {
            TrendGraphData trendGraphData = requestDataCallback
                    .requestTrendGraphData(scanTable, attr, ident);
            trendSetData.put(attr, trendGraphData);
        }

        createTopControls();
        createGraphCanvases();
    }

    private void createTopControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(3, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        identCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        identCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleIdentComboAction();
            }
        });

        populateIdentCombo();

        trendSetCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        trendSetCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTrendSetComboAction();
            }
        });

        populateTrendSetsCombo();

        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }

    private void createGraphCanvases() {
        /*
         * TODO : -- Have a composite member variable and then dispose of all
         * the children canvases -- create all of the canvases to match what is
         * in the trend set -- re-layout the composite
         */
        canvasMap.clear();

        /*
         * Dispose of the canvasComp that will also dispose of all the canvases.
         */
        if (canvasesComp != null) {
            canvasesComp.dispose();
            canvasesComp = null;
        }

        canvasesComp = new Composite(shell, SWT.NONE);
        canvasesComp.setLayout(new GridLayout(1, false));

        /*
         * Loop and create the canvases from the trendSetData map
         */
        Set<String> attrKeys = trendSetData.keySet();

        for (String attrKey : attrKeys) {
            // System.out.println("Canvas attr = " + attrKey);

            TrendGraphData tgd = trendSetData.get(attrKey);
            TrendGraphCanvas tgc = new TrendGraphCanvas(canvasesComp, tgd,
                    this.requestDataCallback.getCurrentDate(), scanTable,
                    attrKey, vcp, requestDataCallback, ident);

            canvasMap.put(attrKey, tgc);
        }

        canvasesComp.pack();
        shell.pack();
        shell.layout();
        shell.redraw();
    }

    private void populateIdentCombo() {
        for (String str : identArray) {
            identCbo.add(str);
        }

        identCbo.select(identCbo.indexOf(ident));
    }

    private void populateTrendSetsCombo() {
        String[] trendNames = trendCfgMgr.getTrendSetNames();

        for (String trendName : trendNames) {
            trendSetCbo.add(trendName);
        }

        trendSetCbo.select(trendSetCbo.indexOf(trendSetName));
    }

    private void handleIdentComboAction() {
        ident = identCbo.getItem(identCbo.getSelectionIndex());

        trendSetData.clear();

        // Loop through all of the attributes and call update and store the data
        // map for each attribute
        for (String attr : attrArray) {
            TrendGraphData tgd = requestDataCallback.requestTrendGraphData(
                    scanTable, attr, ident);
            trendSetData.put(attr, tgd);

            // Call the update call back so the table can manage this dialog.
            this.updateCallback.trendSetGraphChanged(ident, trendSetName, this);

            // Update the canvas with the new data
            canvasMap.get(attr).updateAttribute(attr, tgd,
                    requestDataCallback.getCurrentDate());
            canvasMap.get(attr).setIndent(ident);
        }
    }

    private void handleTrendSetComboAction() {
        trendSetData.clear();

        trendSetName = trendSetCbo.getItem(trendSetCbo.getSelectionIndex());
        System.out.println("trendSetName = " + trendSetName);

        // Get a new attributes list since the trend name has changed
        attrArray = trendCfgMgr.getAttributes(trendSetName);

        for (String attr : attrArray) {
            System.out.println("Change trend set - attr = " + attr);

            TrendGraphData tgd = requestDataCallback.requestTrendGraphData(
                    scanTable, attr, ident);
            trendSetData.put(attr, tgd);
        }

        // TODO : finish this...

        // Recreate the graph canvases since they have most likely changed
        // due to the new trend set.
        createGraphCanvases();

        this.updateCallback.trendSetGraphChanged(ident, trendSetName, this);
    }

    /**
     * Update the trend graph data so the latest data can be displayed.
     * 
     * @return true if item is to be disposed
     */
    public boolean updateTrendSetsGraph() {
        trendSetData.clear();
        // Loop through all of the attributes and call update and store the data
        // map for
        // each attribute
        for (String attr : attrArray) {
            TrendGraphData tgd = requestDataCallback.requestTrendGraphData(
                    scanTable, attr, ident);
            trendSetData.put(attr, tgd);

            // Call the update call back so the table can manage this dialog.
            this.updateCallback.trendSetGraphChanged(ident, trendSetName, this);

            // Update the canvas with the new data
            canvasMap.get(attr).updateAttribute(attr, tgd,
                    requestDataCallback.getCurrentDate());
        }

        if (requestDataCallback.cellValid(this.ident) == false) {
            return true;
        }

        return false;
    }

    /**
     * Redraw the graphs with updated data.
     */
    public void redrawTrendGraph() {
        updateTrendSetsGraph();
    }

    public void displayDialog() {
        shell.setActive();
    }

    public boolean dialogIsDisposed() {
        return shell.isDisposed();
    }

    /**
     * Overriding the dispose method to notify that the trend graph is closing.
     */
    @Override
    protected void disposed() {
        this.updateCallback.trendSetGraphClosing(this);
    }
}
