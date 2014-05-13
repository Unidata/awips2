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

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Scan/DMD Trend Graph Dialog.
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
public class TrendGraphDlg extends CaveSWTDialog {

    private final ScanTables scanTable;

    private Combo identCbo;

    private Combo attrCbo;

    private String ident;

    private String attrName;

    private TrendGraphCanvas trendGraphCanvas;

    private final ITrendGraphUpdate updateCallback;

    private final IRequestTrendGraphData requestDataCallback;

    private TrendGraphData trendGraphData;

    private final String[] identArray;

    private final Integer vcp;

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param scanTable
     * @param ident
     * @param attrName
     * @param updateCallback
     * @param requestDataCallback
     * @param identArray
     * @param vcp
     */
    public TrendGraphDlg(Shell parentShell, ScanTables scanTable, String ident,
            String attrName, ITrendGraphUpdate updateCallback,
            IRequestTrendGraphData requestDataCallback, String[] identArray,
            Integer vcp) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText(scanTable.name() + " Trend Graph");

        this.scanTable = scanTable;
        this.ident = ident;
        this.attrName = attrName;
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
        trendGraphData = requestDataCallback.requestTrendGraphData(scanTable,
                attrName, ident);

        createTopControls();
        createGraphCanvas();
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

        attrCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        attrCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAttributeComboAction();
            }
        });

        populateAttributesCombo();

        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    private void createGraphCanvas() {
        trendGraphCanvas = new TrendGraphCanvas(shell, trendGraphData,
                requestDataCallback.getCurrentDate(), scanTable, attrName, vcp,
                requestDataCallback, ident);
    }

    private void populateIdentCombo() {
        for (String str : identArray) {
            identCbo.add(str);
        }

        identCbo.select(identCbo.indexOf(ident));
    }

    private void populateAttributesCombo() {
        String[] trendAttrNames = SCANConfig.getInstance().getTrendAttributes(
                scanTable);

        for (String name : trendAttrNames) {
            attrCbo.add(name);
        }

        attrCbo.select(attrCbo.indexOf(attrName));
    }

    private void handleIdentComboAction() {
        ident = identCbo.getItem(identCbo.getSelectionIndex());

        this.updateCallback.trendGraphChanged(ident, attrName, this);

        trendGraphData = requestDataCallback.requestTrendGraphData(scanTable,
                attrName, ident);
        trendGraphCanvas.updateAttribute(attrName, trendGraphData,
                requestDataCallback.getCurrentDate());
        trendGraphCanvas.setIndent(ident);
    }

    private void handleAttributeComboAction() {
        attrName = attrCbo.getItem(attrCbo.getSelectionIndex());

        this.updateCallback.trendGraphChanged(ident, attrName, this);

        trendGraphData = requestDataCallback.requestTrendGraphData(scanTable,
                attrName, ident);
        trendGraphCanvas.updateAttribute(attrName, trendGraphData,
                requestDataCallback.getCurrentDate());
    }

    /**
     * Update the trend graph data so the latest data can be displayed.
     * 
     * @return true if item is to be disposed
     */
    public boolean updateTrendGraph() {
        trendGraphData = requestDataCallback.requestTrendGraphData(scanTable,
                attrName, ident);
        trendGraphCanvas.updateAttribute(attrName, trendGraphData,
                requestDataCallback.getCurrentDate());

        if (requestDataCallback.cellValid(this.ident) == false) {
            return true;
        }

        return false;
    }

    /**
     * Redraw the graphs with updated data.
     */
    public void redrawTrendGraph() {
        updateTrendGraph();
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
        this.updateCallback.trendGraphClosing(this);
    }
}
