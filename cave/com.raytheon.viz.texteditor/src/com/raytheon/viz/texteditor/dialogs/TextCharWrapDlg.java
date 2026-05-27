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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog that allows the user to set the character wrap for editing.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Jan 10, 2008  722      grichard  Initial creation.
 * Sep 25, 2012  1196     lvenable  Refactor dialogs to prevent blocking.
 * May 01, 2019  7831     randerso  Code cleanup
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
     * The value of character wrap column count.
     */
    private int charWrapCol;

    private int defCharWrapCol;

    private int rangeStart;

    private int rangeEnd;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     * @param defCharWrapCol
     * @param rangeStart
     * @param rangeEnd
     */
    public TextCharWrapDlg(Shell parent, final int defCharWrapCol,
            final int rangeStart, final int rangeEnd) {
        super(parent, SWT.DIALOG_TRIM | CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("New Wrap Width");
        this.defCharWrapCol = defCharWrapCol;
        this.rangeStart = rangeStart;
        this.rangeEnd = rangeEnd;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(2, false);
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(null);
        // Create all of the controls.
        createFields();
        createButtons();
    }

    /**
     * Create all of the WMO ID controls.
     */
    private void createFields() {
        // Create the composite to contain the wrap controls.
        Label label = new Label(shell, SWT.NONE);
        label.setText("Enter word wrap width in characters:");
        GridData gd = new GridData();
        gd.horizontalSpan = 2;
        label.setLayoutData(gd);

        label = new Label(shell, SWT.NONE);
        label.setText(String.format("(Allowable Range: %1$d-%2$d chars)",
                rangeStart, rangeEnd));

        // Create the WMO ID text field.
        int digits = (int) Math.ceil(Math.log10(rangeEnd));
        charWrapColTF = new Text(shell, SWT.BORDER);
        charWrapColTF.setTextLimit(digits);
        GC gc = new GC(charWrapColTF);
        int width = digits * gc.stringExtent("9").x;
        gc.dispose();
        gd = new GridData(width, SWT.DEFAULT);
        charWrapColTF.setLayoutData(gd);
        if (defCharWrapCol != -1) {
            charWrapColTF.setText(Integer.toString(defCharWrapCol));
            charWrapColTF.selectAll();
        }

        charWrapColTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent event) {
                event.doit = Character.isDigit(event.character);
            }
        });
    }

    /**
     * Create the bottom control buttons.
     */
    private void createButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        buttonArea.setLayoutData(gd);
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the Enter button.
        Button enterBtn = new Button(buttons, SWT.PUSH);
        enterBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        enterBtn.setText("Enter");
        enterBtn.setEnabled(true);
        enterBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!isColWidthValid()) {
                    // Notify the user that the product may not be valid.
                    userInformation(String.format(
                            "Entry must be a positive number between %1$d and %2$d.",
                            rangeStart, rangeEnd));
                    return;
                }
                setReturnValue(charWrapCol);
                shell.dispose();
            }
        });

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
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