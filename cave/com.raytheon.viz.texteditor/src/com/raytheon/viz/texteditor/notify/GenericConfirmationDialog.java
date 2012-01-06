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
package com.raytheon.viz.texteditor.notify;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This is a generic non-blocking pop up dialog. It is modeless, so other
 * controls can be used while this dialog is visible.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/20/2010   7210       cjeanbap    Initial Creation
 * 2008-12-09
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
public class GenericConfirmationDialog extends CaveJFACEDialog {

    private static final int MIN_WIDTH = 500;

    private static final int MIN_HEIGHT = 125;

    private Label messageText;

    private String title;

    private Composite top;

    private String message;

    /**
     * @param parent
     */
    public GenericConfirmationDialog(Shell parent, String title, String message) {
        super(parent);

        setShellStyle(SWT.MODELESS | SWT.RESIZE | SWT.DIALOG_TRIM);

        if (title != null) {
            this.title = title;
        }
        this.message = message;
    }

    @Override
    public boolean close() {
        return super.close();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;

        top.setLayout(mainLayout);

        messageText = new Label(top, SWT.CENTER);
        messageText.setText(this.message);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        data.widthHint = 500;
        messageText.setLayoutData(data);

        return top;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        Composite composite = (Composite) super.createButtonBar(parent);
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_END
                | GridData.VERTICAL_ALIGN_CENTER);
        composite.setLayoutData(data);
        return composite;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, IDialogConstants.OK_ID, "OK", false);
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
        shell.setMinimumSize(MIN_WIDTH, MIN_HEIGHT);
        shell.setText(title);
    }

    public static void main(String[] args) {
        final GenericConfirmationDialog gcd = new GenericConfirmationDialog(
                new Shell(), "Generic Confirmation Dialog", "Test Message");
        gcd.setBlockOnOpen(true);
        gcd.open();
    }
}
