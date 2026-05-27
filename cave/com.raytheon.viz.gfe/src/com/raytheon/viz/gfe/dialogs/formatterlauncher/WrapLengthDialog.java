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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * A dialog to obtain the user's input for the word wrap length in
 * ProductEditorComp. The user input is validated to ensure that it is an
 * integer in a reasonable range.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2010            wldougher     Initial creation
 * Nov 09, 2012 1298       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class WrapLengthDialog extends CaveJFACEDialog {
    private final int MAX_COLUMNS = 255;

    private final int MIN_COLUMNS = 15;

    private final String MESSAGE = "Current Wrap Length: %d\n"
            + "Enter new Wrap Length";

    protected Label notice;

    protected Text userText;

    protected Integer wrapLength;

    protected WrapLengthDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     *      .Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        getShell().setText("Wrap Length");
        Composite comp = (Composite) super.createDialogArea(parent);
        notice = new Label(comp, SWT.CENTER);
        // TODO: get current wrap length from external source
        notice.setText(String.format(MESSAGE, wrapLength));
        userText = new Text(comp, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
        GridData gridData = new GridData(SWT.CENTER, SWT.FILL, true, false);
        gridData.widthHint = 80;
        userText.setLayoutData(gridData);
        userText.setTextLimit(3);
        return comp;
    }

    /**
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void okPressed() {
        // Validate the value in the text field
        wrapLength = null;
        try {
            wrapLength = Integer.valueOf(userText.getText());
        } catch (NumberFormatException e) {
            ; // we'll handle it below
        }
        String errTxt = null;
        if (wrapLength == null) {
            errTxt = "Not an integer.";
        } else if (wrapLength < MIN_COLUMNS) {
            errTxt = "Wrap Length must be at least " + MIN_COLUMNS + ".";
        } else if (wrapLength > MAX_COLUMNS) {
            errTxt = "Wrap Length cannot be greater than " + MAX_COLUMNS + ".";
        }
        if (errTxt != null) {
            MessageBox warningDialog = new MessageBox(getShell(), SWT.OK
                    | SWT.ICON_WARNING | SWT.APPLICATION_MODAL);
            warningDialog.setText("Illegal value");
            warningDialog.setMessage(errTxt + "\nPlease try again.");
            warningDialog.open();
            userText.selectAll();
            userText.setFocus();
            return;
        }
        // If we got here, the dialog will close.
        super.okPressed();
    }

    /**
     * Method to get the wrap length chosen by the user.
     * <p>
     * Only call this method after open() has returned OK!
     * 
     * @return the wrap length value set by the user
     * @throws NullPointerException
     *             if a valid wrap length has not been entered yet
     */
    public int getWrapLength() {
        return wrapLength.intValue();
    }

    public void setWrapLength(int wrapLength) {
        this.wrapLength = Integer.valueOf(wrapLength);
    }
}
