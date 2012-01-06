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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            randerso     Initial creation
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
            importGridsBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    importGrids = importGridsBtn.getSelection();
                }
            });
            importGridsBtn.getSelection();
        }

        startGfeBtn = new Button(top, SWT.CHECK);
        startGfeBtn.setText("Start GFE");
        startGfeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                startGfe = startGfeBtn.getSelection();
            }
        });

        return top;
    }

    public boolean importGrids() {
        return this.importGrids;
    }

    public boolean startGFE() {
        return this.startGfe;
    }
}
