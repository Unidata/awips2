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
 *                         Mail Stop B8^M
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.viz.monitor.ffmp.fffg;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to dispaly FFFG help.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Nov 29, 2012 1353       rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class HelpDlg extends CaveSWTDialog {
    /**
     * Create the label object.
     */
    private Label helpLbl;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */

    public HelpDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Help for ForcedFFG");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        // Initialize layout
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 270;
        helpLbl = new Label(shell, SWT.None);
        helpLbl.setLayoutData(gd);

        // Add a close button
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd2 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd2);

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("OK");
        closeBtn.setLayoutData(gd2);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        populateDlg();
    }

    private void populateDlg() {
        String helpText = "  -  To add FFG to counties: Left click on the county name \n"
                + "     in the leftmost column and click on the button \n"
                + "     'Add/Update selected County(ies)' \n"
                + "  -  To add FFG to basins: Left click on the basin name \n"
                + "     in the leftmost column and click on the button \n"
                + "     'Add/Update selected Basin(s)' \n"
                + "  -  To remove FFG from counties: Left click on the county name \n"
                + "     in the rightmost column and click on the button \n"
                + "     'Remove County(ies)' \n"
                + "  -  To remove FFG from basins: Left click on the basin name \n"
                + "     in the rightmost column and click on the button \n"
                + "     'Remove Basin(s)' \n"
                + "  -  Basin value override county value.\n"
                + "  -  County value override CWA value.\n"
                + "  -  Visit http://www.nws.noaa.gov/mdl/ffmp/ \n"
                + "     for more details.";

        this.helpLbl.setText(helpText);
        this.shell.pack();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
    }
}
