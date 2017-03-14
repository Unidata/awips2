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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.msgs.ShowEmptyEditAreaWarningMsg;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Warning prompt box if there is no edit area selected
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 3, 2008				njensen	Initial creation
 * Nov 7, 2012  1298        rferrel     Part of non-blocking made dialog modal.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EmptyEditAreaWarningDialog extends CaveJFACEDialog {

    private String title;

    private Composite comp;

    private Button checkbox;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public EmptyEditAreaWarningDialog(Shell parentShell) {
        super(parentShell);
        this.title = "Empty Edit Area Warning";
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        new ShowEmptyEditAreaWarningMsg(!checkbox.getSelection()).send();
        super.buttonPressed(buttonId);
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
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, Dialog.OK, "Yes", false);
        createButton(parent, Dialog.CANCEL, "No", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        comp = new Composite(composite, SWT.NONE);

        comp.setLayout(new GridLayout(1, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label label = new Label(composite, SWT.NONE);
        String warning = "Empty edit area.  Edit action will be "
                + "applied over the entire grid!";
        String message = warning.toUpperCase() + "\n\n";
        message += "Do you want to proceed?";
        label.setText(message);

        checkbox = new Button(composite, SWT.CHECK);
        checkbox.setText("Do not show this message again");

        applyDialogFont(composite);

        return composite;
    }

}
