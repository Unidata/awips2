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
package com.raytheon.viz.avnconfig;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * HelpUsageDlg class displays the Help usage dialog AvnFPS. This is a template
 * dialog that is used throughout AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 15 OCT 2012  1229       rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class HelpUsageDlg extends CaveSWTDialog {

    /**
     * Help description.
     */
    private String helpDesc;

    /**
     * Help text put in the help text control.
     */
    private String helpText;

    /**
     * Help text control.
     */
    private StyledText helpStTxt;

    /**
     * Help text control font.
     */
    private Font textFont;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     * @param helpDesc
     *            Help description.
     * @param helpText
     *            Help text.
     */
    public HelpUsageDlg(Shell parent, String helpDesc, String helpText) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Help");

        this.helpDesc = helpDesc;
        this.helpText = helpText;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        textFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        textFont = new Font(getDisplay(), "Monospace", 10, SWT.NORMAL);

        createHelpLabel();
        createHelpTextControl();
        createCloseButton();
    }

    /**
     * Create the help description label.
     */
    private void createHelpLabel() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label helpDescLbl = new Label(shell, SWT.CENTER);
        helpDescLbl.setText(helpDesc);
        helpDescLbl.setLayoutData(gd);
    }

    /**
     * Create the help text control.
     */
    private void createHelpTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 420;
        gd.widthHint = 600;
        helpStTxt = new StyledText(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        helpStTxt.setWordWrap(false);
        helpStTxt.setFont(textFont);
        helpStTxt.setEditable(false);
        helpStTxt.setLayoutData(gd);

        helpStTxt.setText(this.helpText);
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(shell, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);

        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }
}
