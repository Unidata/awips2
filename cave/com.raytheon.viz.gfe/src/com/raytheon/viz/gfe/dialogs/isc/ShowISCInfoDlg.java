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

package com.raytheon.viz.gfe.dialogs.isc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog containing the results of the Show_ISC_Info smart tool
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/28/09      1995       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ShowISCInfoDlg extends CaveSWTDialog {

    /** Text used for color coding */
    private static final String OK_TXT = "OK";

    /** Text used for color coding */
    private static final String IGNORED_TXT = "IGNORED";

    /** Text used for color coding */
    private static final String FAILED_TXT = "FAILED";

    /** The resulting text from the Show_ISC_Info smart tool */
    private String text;

    /** The font used for displaying the text */
    private Font textFont;

    /** Custom color for coloring the IGNORED items */
    private Color darkOrange;

    /** Text box containing the text */
    private StyledText textBox;

    /**
     * Creates a new dialog
     * 
     * @param parentShell
     *            The shell
     * @param text
     *            The text from Show_ISC_Info smart tool
     */
    public ShowISCInfoDlg(Shell parentShell, String text) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("ISC Discrepancy Info");

        if (text == null) {
            this.text = "";
        } else {
            this.text = text;
        }

    }

    @Override
    protected void disposed() {
        textFont.dispose();
        darkOrange.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        textFont = new Font(getDisplay(), "Monospace", 8, SWT.NORMAL);
        darkOrange = new Color(getDisplay(), 255, 102, 0);

        // Initialize all of the controls and layouts
        createTextArea();
        createBottomButton();
        colorText();
    }

    /**
     * Creates the text area and populates the text
     */
    private void createTextArea() {
        Composite listComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        listComp.setLayout(gl);
        listComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 500;
        gd.heightHint = 200;

        textBox = new StyledText(listComp, SWT.BORDER | SWT.V_SCROLL);
        textBox.setEditable(false);
        textBox.setText(text);
        textBox.setFont(textFont);
        textBox.setLayoutData(gd);
    }

    /**
     * Creates the close button
     */
    private void createBottomButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.setForeground(getDisplay().getSystemColor(SWT.COLOR_RED));

        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });

    }

    /**
     * Colors the text according to rules
     */
    private void colorText() {
        int startLineIndex = 0;
        int numLines = textBox.getLineCount();
        String tmpStr = null;

        for (int i = 0; i < numLines; i++) {
            startLineIndex = textBox.getOffsetAtLine(i);
            tmpStr = textBox.getLine(i);
            if (tmpStr.endsWith(OK_TXT)) {
                StyleRange sr = new StyleRange(startLineIndex
                        + tmpStr.lastIndexOf(OK_TXT), OK_TXT.length(),
                        getDisplay().getSystemColor(SWT.COLOR_DARK_GREEN), null);
                textBox.setStyleRange(sr);
            } else if (tmpStr.endsWith(IGNORED_TXT)) {
                StyleRange sr = new StyleRange(startLineIndex
                        + tmpStr.lastIndexOf(IGNORED_TXT),
                        IGNORED_TXT.length(), darkOrange, null);
                textBox.setStyleRange(sr);
            } else if (tmpStr.endsWith(FAILED_TXT)) {
                StyleRange sr = new StyleRange(startLineIndex
                        + tmpStr.lastIndexOf(FAILED_TXT), FAILED_TXT.length(),
                        getDisplay().getSystemColor(SWT.COLOR_RED), null);
                textBox.setStyleRange(sr);
            }
        }
    }
}
