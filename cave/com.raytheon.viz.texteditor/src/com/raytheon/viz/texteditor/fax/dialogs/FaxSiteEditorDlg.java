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
package com.raytheon.viz.texteditor.fax.dialogs;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.subscription.db.AutoFaxRecord;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2010            lvenable     Initial creation
 * 26Sep2012    1196      lvenable     Update for dialog refactor to not block the dialog on open.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class FaxSiteEditorDlg extends CaveSWTDialog {

    private Text afosPilTF;

    private Text faxNumberTF;

    private Text phoneNumberTF;

    private Text recipientTF;

    private Text companyTF;

    private AutoFaxRecord faxRecord;

    private LdadFaxSitesDlg callback;

    public FaxSiteEditorDlg(Shell parent, LdadFaxSitesDlg callback) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("Fax Message");
        this.callback = callback;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Create the menus
        createMenus();

        // Create the controls on the display
        createControls();
    }

    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createRecipientMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        /*
         * Update DB
         */
        MenuItem updateDbMI = new MenuItem(fileMenu, SWT.NONE);
        updateDbMI.setText("Update DB");
        updateDbMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                String afosPil = afosPilTF.getText();
                String faxNumber = faxNumberTF.getText();
                String phoneNumber = phoneNumberTF.getText();
                String recipient = recipientTF.getText();
                String company = companyTF.getText();
                if (null != afosPil && null != faxNumber && null != phoneNumber
                        && null != recipient && null != company) {
                    AutoFaxRecord add = new AutoFaxRecord(afosPil, faxNumber,
                            phoneNumber, recipient, company);
                    callback.addAutoFaxSite(add);
                    callback.updateDBAction();
                    FaxSiteEditorDlg.this.close();
                } else {
                    MessageDialog.open(MessageDialog.INFORMATION, getParent(),
                            null, "Please fill out the record completely!",
                            SWT.NONE);
                }
            }
        });

        /*
         * Exit
         */
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void createRecipientMenu(Menu menuBar) {
        // -------------------------------------
        // Create the recipient menu
        // -------------------------------------
        MenuItem recipientMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        recipientMenuItem.setText("&Recipient");

        // Create the Recipient menu item with a Recipient "dropdown" menu
        Menu recipientMenu = new Menu(menuBar);
        recipientMenuItem.setMenu(recipientMenu);

        /*
         * Add Recipient
         */
        MenuItem addRecipientMI = new MenuItem(recipientMenu, SWT.NONE);
        addRecipientMI.setText("Add Recipient");
        addRecipientMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addRecipientAction();
            }
        });

        /*
         * Delete Recipient
         */
        MenuItem deleteRecipientMI = new MenuItem(recipientMenu, SWT.NONE);
        deleteRecipientMI.setText("Delete Recipient");
        deleteRecipientMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecipientAction();
            }
        });
    }

    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the File menu item with a Help "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        /*
         * Contents and About
         */
        MenuItem contentsMI = new MenuItem(helpMenu, SWT.NONE);
        contentsMI.setText("&Contents");
        contentsMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });

        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("&About");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });
    }

    private void createControls() {

        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
        Label ldadLbl = new Label(controlComp, SWT.NONE);
        ldadLbl.setText("LDAD Fax Recipient");
        ldadLbl.setLayoutData(gd);

        // AFOS PIL
        Label afosPilLbl = new Label(controlComp, SWT.NONE);
        afosPilLbl.setText("AFOS PIL: ");

        gd = new GridData(140, SWT.DEFAULT);
        afosPilTF = new Text(controlComp, SWT.BORDER);
        afosPilTF.setLayoutData(gd);

        // Fax Number
        Label faxNumLbl = new Label(controlComp, SWT.NONE);
        faxNumLbl.setText("Fax Number: ");

        gd = new GridData(180, SWT.DEFAULT);
        faxNumberTF = new Text(controlComp, SWT.BORDER);
        faxNumberTF.setLayoutData(gd);

        // Add a separator bar
        addSeparator(controlComp);

        Label phoneNumberLbl = new Label(controlComp, SWT.NONE);
        phoneNumberLbl.setText("Phone Number: ");

        gd = new GridData(275, SWT.DEFAULT);
        phoneNumberTF = new Text(controlComp, SWT.BORDER);
        phoneNumberTF.setLayoutData(gd);

        Label recipLbl = new Label(controlComp, SWT.NONE);
        recipLbl.setText("Recipient: ");

        gd = new GridData(275, SWT.DEFAULT);
        recipientTF = new Text(controlComp, SWT.BORDER);
        recipientTF.setLayoutData(gd);

        Label companyLbl = new Label(controlComp, SWT.NONE);
        companyLbl.setText("Company: ");

        gd = new GridData(275, SWT.DEFAULT);
        companyTF = new Text(controlComp, SWT.BORDER);
        companyTF.setLayoutData(gd);

        if (null != faxRecord) {
            afosPilTF.setText(faxRecord.getId().getAfosPil());
            faxNumberTF.setText(faxRecord.getId().getFaxNumber());
            phoneNumberTF.setText(faxRecord.getPhoneNumber());
            recipientTF.setText(faxRecord.getRecipient());
            companyTF.setText(faxRecord.getCompany());
        }
        // Add a separator bar
        addSeparator(controlComp);

        /*
         * Send and Cancel buttons
         */
        Composite buttonComp = new Composite(controlComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button addRecipientBtn = new Button(buttonComp, SWT.PUSH);
        addRecipientBtn.setText("Add Recipient");
        addRecipientBtn.setLayoutData(gd);
        addRecipientBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addRecipientAction();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button deleteRecipientBtn = new Button(buttonComp, SWT.PUSH);
        deleteRecipientBtn.setText("Delete Recipient");
        deleteRecipientBtn.setLayoutData(gd);
        deleteRecipientBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                deleteRecipientAction();
            }
        });

    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void addRecipientAction() {
        boolean addRecord = MessageDialog
                .open(MessageDialog.QUESTION_WITH_CANCEL,
                        getParent(),
                        "Add Recipient",
                        "Adding this recipient is not finished until \"Update DB\" is selected from the LDAD Fax Dialog.",
                        SWT.NONE);

        if (addRecord) {
            String afosPil = afosPilTF.getText();
            String faxNumber = faxNumberTF.getText();
            String phoneNumber = phoneNumberTF.getText();
            String recipient = recipientTF.getText();
            String company = companyTF.getText();
            if (null != afosPil && null != faxNumber && null != phoneNumber
                    && null != recipient && null != company) {
                AutoFaxRecord add = new AutoFaxRecord(afosPil, faxNumber,
                        phoneNumber, recipient, company);
                if (null != faxRecord) {
                    callback.deleteAutoFaxSite(faxRecord);
                }
                callback.addAutoFaxSite(add);
                this.close();
            } else {
                MessageDialog.open(MessageDialog.INFORMATION, getParent(),
                        null, "Please fill out the record completely!",
                        SWT.NONE);
            }
        }

    }

    private void deleteRecipientAction() {
        boolean deleteRecord = MessageDialog
                .open(MessageDialog.QUESTION_WITH_CANCEL,
                        getParent(),
                        "Delete Recipient",
                        "Deleting this recipient is not finished until \"Update DB\" is selected from the LDAD Fax Dialog.",
                        SWT.NONE);

        if (deleteRecord) {
            String afosPil = afosPilTF.getText();
            String faxNumber = faxNumberTF.getText();
            String phoneNumber = phoneNumberTF.getText();
            String recipient = recipientTF.getText();
            String company = companyTF.getText();
            if (null != afosPil && null != faxNumber && null != phoneNumber
                    && null != recipient && null != company) {
                if (null != faxRecord) {
                    callback.deleteAutoFaxSite(faxRecord);
                }
                this.close();
            } else {
                MessageDialog.open(MessageDialog.INFORMATION, getParent(),
                        null, "Please fill out the record completely!",
                        SWT.NONE);
            }
        }
    }

    /**
     * @return the faxRecord
     */
    public AutoFaxRecord getFaxRecord() {
        return faxRecord;
    }

    /**
     * @param faxRecord
     *            the faxRecord to set
     */
    public void setFaxRecord(AutoFaxRecord faxRecord) {
        this.faxRecord = faxRecord;
    }
}
