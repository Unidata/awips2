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
package com.raytheon.viz.gfe.dialogs.sbu;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Asks the user if they want to import digital data and/or start GFE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            randerso     Initial creation
 * Mar 20, 2013   1447     dgilling     Implement changes from A1 DR 21404,
 *                                      make default selections match A1.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class QueryOptionsDlg extends CaveJFACEDialog {

    private boolean doImCon;

    private boolean importGrids;

    private boolean startGfe;

    private boolean trMode;

    private Button importGridsBtn;

    private Button startGfeBtn;

    /**
     * @param parentShell
     */
    protected QueryOptionsDlg(Shell parentShell, boolean doImCon) {
        super(parentShell);
        this.doImCon = doImCon;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Service Backup");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.
     * eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        Label label = new Label(top, SWT.CENTER);
        label.setText("Would you also like to");

        if (doImCon) {
            importGridsBtn = new Button(top, SWT.CHECK);
            importGridsBtn.setText("Import Digital Forecast");
            importGridsBtn.setSelection(true);
            importGridsBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    importGrids = importGridsBtn.getSelection();
                }
            });
            importGrids = importGridsBtn.getSelection();

            final Button trModeBtn = new Button(top, SWT.CHECK);
            trModeBtn.setText("Troubleshooting Mode (no ISC/VTEC AT sharing)");
            trModeBtn.setSelection(false);
            trModeBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    trMode = trModeBtn.getSelection();
                }
            });
            trMode = trModeBtn.getSelection();
        }

        startGfeBtn = new Button(top, SWT.CHECK);
        startGfeBtn.setText("Start GFE");
        startGfeBtn.setSelection(true);
        startGfeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startGfe = startGfeBtn.getSelection();
            }
        });
        startGfe = startGfeBtn.getSelection();

        return top;
    }

    public boolean importGrids() {
        return this.importGrids;
    }

    public boolean startGFE() {
        return this.startGfe;
    }

    public boolean trMode() {
        return this.trMode;
    }
}
