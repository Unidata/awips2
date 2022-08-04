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

package com.raytheon.viz.gfe.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Ask if Procedure Jobs should be terminated or exit canceled.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2011            rferrel     Initial creation
 * Dec 10, 2013  #2367     dgilling    Rewrite to use new ProcedureJobPool and
 *                                     SmartToolJobPool.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class KillJobsOnExitDialog extends CaveJFACEDialog {

    private Composite top;

    private final DataManager dataMgr;

    /**
     * Use defaults of -240, minimum and 240 max.
     */
    public KillJobsOnExitDialog(Shell parent, DataManager dataMgr) {
        super(parent);
        int style = this.getShellStyle() | SWT.MODELESS | SWT.TITLE | SWT.CLOSE;
        this.setShellStyle(style);
        this.dataMgr = dataMgr;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        top.setLayout(layout);

        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        int cnt[] = dataMgr.getProcedureJobPool().getWorkRemaining();
        GridData data = null;
        if ((cnt[0] > 0) || (cnt[1] > 0)) {
            Label lab = new Label(top, SWT.NONE);
            lab.setText(String
                    .format("Have %d procedure(s) running and %d procedures(s) pending",
                            cnt[0], cnt[1]));
            data = new GridData(GridData.FILL, SWT.DEFAULT, true, false);
            lab.setLayoutData(data);
        }

        cnt = dataMgr.getSmartToolJobPool().getWorkRemaining();
        if ((cnt[0] > 0) || (cnt[1] > 0)) {
            Label lab = new Label(top, SWT.NONE);
            lab.setText(String
                    .format("Have %d Smart tool(s) running and %d Smart tool(s) pending",
                            cnt[0], cnt[1]));
            data = new GridData(GridData.FILL, SWT.DEFAULT, true, false);
            lab.setLayoutData(data);
        }

        Label spacer = new Label(top, SWT.NONE);
        spacer.setText("");
        Label warning = new Label(top, SWT.NONE);
        warning.setText("             ++++ WARNING ++++"
                + "\nTo prevent errors please cancel this dialog and close"
                + "\nany open Smart Tool and Procedure dialogs. Some dialogs"
                + "\nmay be hidden behind other windows.");
        spacer = new Label(top, SWT.NONE);
        spacer.setText("");
        Label confirmLabel = new Label(top, SWT.NONE);
        confirmLabel.setText("Do you want kill the job(s)?");
        data = new GridData(GridData.FILL, SWT.DEFAULT, true, false);
        confirmLabel.setLayoutData(data);
        Label spacer1 = new Label(top, SWT.NONE);
        spacer1.setText("");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Kill Jobs(s)");
    }

    @Override
    protected void buttonPressed(int buttonId) {
        boolean cancel = buttonId == IDialogConstants.CANCEL_ID;

        if (cancel) {
            super.buttonPressed(IDialogConstants.CANCEL_ID);
        } else {
            super.buttonPressed(IDialogConstants.OK_ID);
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        boolean okEnabled = true; // saveableParmCount > 0;
        Button okButton = createButton(parent, IDialogConstants.OK_ID, "Yes",
                okEnabled);
        okButton.setEnabled(okEnabled);
        Button cancelButton = createButton(parent, IDialogConstants.CANCEL_ID,
                "Cancel", !okEnabled);
        cancelButton.setFocus();
    }
}
