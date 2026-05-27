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
 * Displays the FFFG About information
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
public class AboutDlg extends CaveSWTDialog {

    /**
     * The label object.
     */

    private Label aboutLbl;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public AboutDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("About ForcedFFG");
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
        gd.heightHint = 100;

        aboutLbl = new Label(shell, SWT.None);
        aboutLbl.setLayoutData(gd);

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
        String aboutText = "  The ForceFFG GUI was closely followed FFMP, LSR,\n"
                + "  ForcedFFG applications and was based on input from\n"
                + "  the AWIPS User Interface Working Group.  Visit \n"
                + "  http://www.nws.noaa.gov/mdl/ffmp/ for more details\n"
                + "  regarding this ForcedFFG GUI.";

        this.aboutLbl.setText(aboutText);
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
