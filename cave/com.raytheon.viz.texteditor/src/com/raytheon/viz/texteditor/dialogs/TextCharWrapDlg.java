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

package com.raytheon.viz.texteditor.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.texteditor.msgs.ITextCharWrapCallback;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog that allows the user to set the character wrap for editing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/10/2008    722         grichard    Initial creation.
 * 25SEP2012    1196        lvenable    Refactor dialogs to prevent blocking.
 * 
 * </pre>
 * 
 * @author grichard
 */
public class TextCharWrapDlg extends CaveSWTDialog {

    /**
     * Character wrap column text field.
     */
    private Text charWrapColTF;

    /**
     * ENTER button. Transitions the display from Char Wrap entry into Text
     * Editing after validating the character wrap value entered.
     */
    private Button enterBtn;

    /**
     * The value of character wrap column count.
     */
    int charWrapCol;

    Integer defCharWrapCol;

    /**
     * Interface variable for Text Char Wrap Column callback.
     */
    private ITextCharWrapCallback callbackClient = null;

    private int rangeStart;

    private int rangeEnd;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TextCharWrapDlg(Shell parent, ITextCharWrapCallback cbClient,
            final Integer defCharWrapCol, final int rangeStart,
            final int rangeEnd) {
        super(parent, SWT.DIALOG_TRIM | CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("New Width");
        this.defCharWrapCol = defCharWrapCol;
        this.rangeStart = rangeStart;
        this.rangeEnd = rangeEnd;

        callbackClient = cbClient;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Create all of the controls.
        createFields();
        createButtons();
    }

    /**
     * Create all of the WMO ID controls.
     */
    private void createFields() {
        // Create the composite to contain the wrap controls.
        Composite wrapComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        wrapComp.setLayout(gridLayout);

        Label charWidth = new Label(wrapComp, SWT.NONE);
        charWidth.setText(String.format(
                "Enter the width in number of characters:\n"
                        + "(Allowable Range: %1$d-%2$d chars)", rangeStart,
                rangeEnd));

        // Create the WMO ID text field.
        GridData gd = new GridData(55, SWT.DEFAULT);
        charWrapColTF = new Text(wrapComp, SWT.BORDER);
        charWrapColTF.setTextLimit(2);
        charWrapColTF.setLayoutData(gd);
        if (defCharWrapCol != null) {
            charWrapColTF.setText(defCharWrapCol.toString());
            charWrapColTF.selectAll();
        }

        charWrapColTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
            }
        });

        charWrapColTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
            }
        });
    }

    /**
     * Create the bottom control buttons.
     */
    private void createButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the Enter button.
        enterBtn = new Button(buttons, SWT.PUSH);
        enterBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        enterBtn.setText("Enter");
        // enterBtn.setEnabled(false);
        enterBtn.setEnabled(true);
        enterBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (!isColWidthValid()) {
                    // Notify the user that the product may not be valid.
                    userInformation(String
                            .format("Entry must be a positive number between %1$d and %2$d.",
                                    rangeStart, rangeEnd));
                    return;
                }
                callbackClient.setCharWrapCol(charWrapCol);
                setReturnValue(true);
                shell.dispose();
            }
        });

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                shell.dispose();
            }
        });

    }

    private void userInformation(String information) {
        TextWSMessageBox.open(shell, "Sorry!", information);
    }

    private boolean isColWidthValid() {
        boolean result = true;
        if (!(charWrapColTF.getCharCount() == 2)) {
            return false;
        } else {
            try {
                charWrapCol = Integer.decode(charWrapColTF.getText());
                // System.out.printf("The col wrap is: %d%n", charWrapCol);
                if (charWrapCol < rangeStart || charWrapCol > rangeEnd) {
                    return false;
                }
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return result;
    }
}