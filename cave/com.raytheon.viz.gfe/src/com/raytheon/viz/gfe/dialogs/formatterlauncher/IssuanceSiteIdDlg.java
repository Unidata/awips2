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
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display the Issuance Site ID dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation 
 * Jan 18 2010  3395       ryu         Fix "issued by" functionality
 * Nov 08 2012  1298       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class IssuanceSiteIdDlg extends CaveSWTDialog {

    /**
     * Site ID text control.
     */
    private Text siteIdTF;

    /**
     * OK button.
     */
    private Button okBtn;

    private String issuedBy;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public IssuanceSiteIdDlg(Shell parent, String issuedBy) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.issuedBy = issuedBy;
    }

    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

    }

    @Override
    protected void preOpened() {
        super.preOpened();
        shell.setText("Issuance Site ID");
        siteIdTF.insert(issuedBy);
        if (issuedBy.length() == 0 || issuedBy.length() == 3) {
            okBtn.setEnabled(true);
        }
        siteIdTF.forceFocus();
        siteIdTF.selectAll();
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        createIssuanceControls();

        createBottomButtons();
    }

    /**
     * Create the main issuance controls.
     */
    private void createIssuanceControls() {
        Composite issuanceComp = new Composite(shell, SWT.NONE);
        issuanceComp.setLayout(new GridLayout(2, false));

        Label issuanceLbl = new Label(issuanceComp, SWT.NONE);
        issuanceLbl.setText("Enter Issuance Site ID: ");

        GridData gd = new GridData(45, SWT.DEFAULT);
        siteIdTF = new Text(issuanceComp, SWT.BORDER);
        siteIdTF.setTextLimit(3);
        siteIdTF.setLayoutData(gd);
        siteIdTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                if (siteIdTF.getText().length() > 0
                        && siteIdTF.getText().length() < 3) {
                    okBtn.setEnabled(false);
                } else {
                    okBtn.setEnabled(true);
                }
            }
        });
        siteIdTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.trim().toUpperCase();
            }
        });
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(100, SWT.DEFAULT);
        okBtn = new Button(buttons, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setEnabled(true);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(siteIdTF.getText());
                close();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }
}
