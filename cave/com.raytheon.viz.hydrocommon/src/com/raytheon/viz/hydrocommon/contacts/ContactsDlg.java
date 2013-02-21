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
package com.raytheon.viz.hydrocommon.contacts;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ContactsData;
import com.raytheon.viz.hydrocommon.datamanager.ContactsDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Contacts dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 10, 2008				lvenable	Initial creation
 * Feb 13, 2013 15794       wkwock      Make Sequence number goes up to 99
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ContactsDlg extends CaveSWTDialog {

    /**
     * control font.
     */
    private Font controlFont;

    /**
     * Contact list control.
     */
    private List contactList;

    /**
     * Array of contract data.
     */
    private ArrayList<ContactsData> contactArray;

    /**
     * Sequence number spinner control.
     */
    private Spinner seqNumSpnr;

    /**
     * Contact name text control.
     */
    private Text contactNameTF;

    /**
     * Phone number text control.
     */
    private Text phoneTF;

    /**
     * Email text control.
     */
    private Text emailTF;

    /**
     * Concerns text control.
     */
    private Text concernsTF;
    /**
     * text from the remark text box
     */
    private String currentConcernsText=null;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * New contact flag.
     */
    private boolean newContactFlag = false;

    /**
     * Flag indicating if all of the controls should be displayed.
     */
    private boolean fullControls = false;

    /**
     * Location ID.
     */
    private String locationId;

    /**
     * Original contact name.
     */
    private String originalContactName = "";

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     * @param fullControls
     *            Full controls flag.
     * @param locationId
     *            Location ID.
     */
    public ContactsDlg(Shell parent, String titleInfo, boolean fullControls,
            String lid) {
        super(parent);
        setText("Contacts" + titleInfo);

        this.fullControls = fullControls;
        this.locationId = lid;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        contactArray = new ArrayList<ContactsData>();
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createContactList();

        createInformationGroup();

        createBottomButtons();

        // Get the contacts for the location.
        getContacts();

        updateContactListControl();
    }

    /**
     * Create the contacts list control and label.
     */
    private void createContactList() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite labelListComp = new Composite(shell, SWT.NONE);
        labelListComp.setLayout(new GridLayout(1, false));
        labelListComp.setLayoutData(gd);

        // Add the labels above the contact list control.
        Label listLbl = new Label(labelListComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);

        // Add the contact list control.
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 650;
        gd.heightHint = 300;
        contactList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        contactList.setLayoutData(gd);
        contactList.setFont(controlFont);
        contactList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateInformationControls();

                if (contactList.getSelectionIndex() >= 0) {
                    deleteBtn.setEnabled(true);
                }
            }
        });
    }

    /**
     * Create the information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group infoGroup = new Group(shell, SWT.NONE);
        infoGroup.setLayout(new GridLayout(3, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        // ---------------------------------------------
        // Add the controls to the information group
        // ---------------------------------------------
        gd = new GridData(130, SWT.DEFAULT);
        Label sequenceLbl = new Label(infoGroup, SWT.NONE);
        sequenceLbl.setText("Sequence:");
        sequenceLbl.setLayoutData(gd);

        gd = new GridData(350, SWT.DEFAULT);
        Label contactLbl = new Label(infoGroup, SWT.NONE);
        contactLbl.setText("Contact:");
        contactLbl.setLayoutData(gd);

        Label phoneLbl = new Label(infoGroup, SWT.NONE);
        phoneLbl.setText("Phone:");

        gd = new GridData(40, SWT.DEFAULT);
        seqNumSpnr = new Spinner(infoGroup, SWT.BORDER);
        seqNumSpnr.setDigits(0);
        seqNumSpnr.setIncrement(1);
        seqNumSpnr.setPageIncrement(1);
        seqNumSpnr.setSelection(1);
        seqNumSpnr.setMinimum(1);
        seqNumSpnr.setMaximum(99);
        seqNumSpnr.setLayoutData(gd);

        gd = new GridData(275, SWT.DEFAULT);
        contactNameTF = new Text(infoGroup, SWT.BORDER);
        contactNameTF.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        phoneTF = new Text(infoGroup, SWT.BORDER);
        phoneTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label emailLbl = new Label(infoGroup, SWT.NONE);
        emailLbl.setText("Email:");
        emailLbl.setLayoutData(gd);

        gd = new GridData(400, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        emailTF = new Text(infoGroup, SWT.BORDER);
        emailTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label concernsLbl = new Label(infoGroup, SWT.NONE);
        concernsLbl.setText("Remarks/Concerns:");
        concernsLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 100;
        gd.horizontalSpan = 3;
        concernsTF = new Text(infoGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        concernsTF.setLayoutData(gd);
        currentConcernsText=concernsTF.getText();
        ModifyListener listener = new ModifyListener() {
        	@Override
        	public void modifyText(ModifyEvent e) {
        		if (concernsTF.getText().length()>255){
        			concernsTF.setText(currentConcernsText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentConcernsText=concernsTF.getText();
        	}
        };

        concernsTF.addModifyListener(listener);

    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setEnabled(false);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (updateInsertContactData() == true) {
                    shell.dispose();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setEnabled(false);
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateInsertContactData();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                newContactFlag = true;
                clearInformationFields();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteSelectedContact();
            }
        });

        if (fullControls == false) {
            okBtn.setVisible(false);
            applyBtn.setVisible(false);
            deleteBtn.setVisible(false);
            newBtn.setVisible(false);
        }
    }

    /**
     * Clear the informations fields.
     */
    private void clearInformationFields() {
        seqNumSpnr.setSelection(5);
        contactNameTF.setText("");
        phoneTF.setText("");
        emailTF.setText("");
        concernsTF.setText("");

        okBtn.setEnabled(true);
        applyBtn.setEnabled(true);
        deleteBtn.setEnabled(false);
    }

    /**
     * Update the contact data.
     */
    private boolean updateInsertContactData() {
        if (validateEntryData() == false) {
            return false;
        }

        if (newContactFlag == true) {
            try {
                /*
                 * Check if the contact already exists.
                 */
                if (ContactsDataManager.getInstance().recordExists(locationId,
                        contactNameTF.getText().trim()) == true) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                            | SWT.OK | SWT.CANCEL);
                    mb.setText("Insert");
                    mb
                            .setMessage("The contact you want to insert already exists.\n"
                                    + "Do you wish to update the existing data?");
                    int result = mb.open();

                    if (result == SWT.CANCEL) {
                        return false;
                    }

                    updateContact();
                } else {
                    insertNewContact();
                }

            } catch (VizException ve) {
                ve.printStackTrace();
            }
        } else {
            updateContact();
        }

        updateContactListControl();

        return true;
    }

    /**
     * Insert a new contact into the database.
     */
    private void insertNewContact() {
        try {
            ContactsData data = new ContactsData();
            data.setLid(locationId);
            data.setContact(contactNameTF.getText().trim());
            data.setPhone(phoneTF.getText().trim());
            data.setEmail(emailTF.getText().trim());
            data.setRemark(concernsTF.getText().trim());
            data.setPriority(seqNumSpnr.getSelection());

            ContactsDataManager.getInstance().insertContactData(data);
        } catch (VizException ve) {
            ve.printStackTrace();
        }
    }

    /**
     * Update the edited contact data.
     */
    private void updateContact() {
        try {
            ContactsData data = new ContactsData();
            data.setLid(locationId);
            data.setContact(contactNameTF.getText().trim());
            data.setPhone(phoneTF.getText().trim());
            data.setEmail(emailTF.getText().trim());
            data.setRemark(concernsTF.getText().trim());
            data.setPriority(seqNumSpnr.getSelection());

            ContactsDataManager.getInstance().updateContactData(data,
                    originalContactName);
        } catch (VizException ve) {
            ve.printStackTrace();
        }
    }

    /**
     * Update the data in the information controls.
     */
    private void updateInformationControls() {
        ContactsData data = contactArray.get(contactList.getSelectionIndex());

        seqNumSpnr.setSelection(data.getPriority());
        contactNameTF.setText(data.getContact());
        phoneTF.setText(data.getPhone());
        emailTF.setText(data.getEmail());
        concernsTF.setText(data.getRemark());

        originalContactName = data.getContact();

        newContactFlag = false;
    }

    /**
     * Update the contacts list control with any changed data.
     */
    private void updateContactListControl() {
        contactList.removeAll();

        getContacts();

        for (ContactsData data : contactArray) {
            contactList.add(data.toString());
        }

        /*
         * If there are on items in the list then disable the delete, ok, and
         * apply buttons then return.
         */
        if (contactList.getItemCount() == 0) {
            deleteBtn.setEnabled(false);
            okBtn.setEnabled(false);
            applyBtn.setEnabled(false);
            return;
        }

        deleteBtn.setEnabled(true);
        applyBtn.setEnabled(true);
        okBtn.setEnabled(true);
        contactList.select(0);
        updateInformationControls();
    }

    /**
     * Delete the selected contact data.
     */
    private void deleteSelectedContact() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete");
        mb.setMessage("Do you want to delete the selected contact?");
        int result = mb.open();

        if (result == SWT.CANCEL) {
            return;
        }

        ContactsData data = contactArray.get(contactList.getSelectionIndex());

        try {
            ContactsDataManager.getInstance().deleteRecord(data);
        } catch (VizException ve) {
            ve.printStackTrace();
        }

        updateContactListControl();
    }

    /**
     * Get the label text for the contact list control.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "%S        %S                                  %S";

        String labelStr = String.format(format, "Sequence", "Contact", "Phone");

        return labelStr;
    }

    /**
     * Get the list of contacts at the specified location.
     */
    private void getContacts() {
        if (contactArray != null) {
            contactArray.clear();
        }

        try {
            contactArray = ContactsDataManager.getInstance().getContactData(
                    locationId);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Validate the data the user types in. At this time, the only mandatory
     * field is "Contact".
     * 
     * @return True if there is data in the Contact text control, false
     *         otherwise.
     */
    private boolean validateEntryData() {

        if ((contactNameTF.getText()).trim().length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage("You must enter a contact name.");
            mb.open();

            return false;
        }

        return true;
    }
}
