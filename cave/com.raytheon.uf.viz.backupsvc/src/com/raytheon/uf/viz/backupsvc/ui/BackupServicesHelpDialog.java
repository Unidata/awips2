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
package com.raytheon.uf.viz.backupsvc.ui;

import org.eclipse.jface.resource.FontDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Help Dialog for the main Backup Services Dialog. This gives a brief overview
 * of the Backup Services Dialog and describes how the Admin would use it.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 03, 2021 84639      Robert.Blum Initial creation
 *
 * </pre>
 *
 * @author Robert.Blum
 * @version 1.0
 */

public class BackupServicesHelpDialog extends CaveSWTDialog {

    private static final String OVERVIEW_TEXT = "This is the Backup Services Dialog that grants Backup Services Admins the "
            + "controls necessary to handle Incoming and Outgoing localization overrides. "
            + "These overrides are turned into Backup Jobs. During the Backup Services workflow a"
            + " job can have one of the following statuses:";

    private static final String OUTGOING_OVERVIEW_TEXT = "For each localization override made at this site, a BackupSvcOutgoing job is created."
            + " These jobs will appear in the BackupSvcOutgoing table in the metadata database."
            + " Each of these entries will also appear in the Outgoing tab of this dialog. The "
            + "following actions can be taken on these jobs:";

    private static final String INCOMING_OVERVIEW_TEXT = "For each localization override receieved from a backup site, a BackupSvcIncoming job is created."
            + "These jobs will appear in the BackupSvcIncoming table in the metadata database."
            + " Each of these entries will also appear in the Incoming tab of this dialog. The "
            + "following actions can be taken on these jobs:";

    private static final String NEW_STATUS_DESC = "The job was recently added to the database and is waiting to be sent.";

    private static final String SENT_STATUS_DESC = "The admin has sent the file to the configured backup sites.";

    private static final String ACCEPTED_STATUS_DESC = "The admin at the receiving site has installed the file. This job will be purged"
            + " from the UI and database after a configured amount of time.";

    private static final String REJECTED_STATUS_DESC = "The admin at the receiving site has declined to install the file. This job will be purged from"
            + " the UI and database after a configured amount of time.";

    private static final String FAILED_STATUS_DESC = "The outgoing job has failed to be sent to the recipient site or the incoming job has failed to be Accepted/Rejected. Failed jobs can be reattempted.";

    private static final String REVIEW_BUTTON_DESC = "Brings the selected jobs up in a file viewer so the override can be examined.";

    private static final String SEND_BUTTON_DESC = "Sends the selected jobs to the configured backup sites.";

    private static final String DELETE_BUTTON_DESC = "Deletes the selected jobs from the table and database. These jobs will not be sent to the configured backup sites.";

    private static final String WAIT_BUTTON_DESC = "Changes the status on the selected jobs to Wait. These jobs can be re-evalutated at a later time.";

    private static final String ACCEPT_BUTTON_DESC = "Installs the selected jobs to the system.";

    private static final String REJECT_BUTTON_DESC = "Rejects the selected jobs. These jobs will be purged after a configured amount of time.";

    protected BackupServicesHelpDialog(Shell parent) {
        super(parent, SWT.CLOSE | SWT.TITLE);
        setText("Backup Services Help");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 700;
        mainComp.setLayoutData(gridData);
        mainComp.setLayout(new GridLayout(1, true));

        createOverviewGroup(mainComp);
        createOutgoingGroup(mainComp);
        createIncomingGroup(mainComp);

        Button closeButton = new Button(mainComp, SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        closeButton.setLayoutData(gridData);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Creates the Group and all containing widgets for the overview section of
     * the help dialog.
     *
     * @param parent
     *            The parent Composite
     */
    public void createOverviewGroup(Composite parent) {
        Group overviewGrp = new Group(parent, SWT.None);
        overviewGrp.setText("General Overview:");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        overviewGrp.setLayoutData(gridData);
        overviewGrp.setLayout(new GridLayout(2, false));

        Label overviewText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.horizontalSpan = 2;
        overviewText.setLayoutData(gridData);
        overviewText.setText(OVERVIEW_TEXT);

        Label newLabel = new Label(overviewGrp, SWT.NONE);
        FontDescriptor boldDescriptor = FontDescriptor
                .createFrom(newLabel.getFont()).setStyle(SWT.BOLD);
        Font boldFont = boldDescriptor.createFont(newLabel.getDisplay());
        newLabel.setFont(boldFont);
        newLabel.setText("New:");
        Label newText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        newText.setLayoutData(gridData);
        newText.setText(NEW_STATUS_DESC);

        Label sentLabel = new Label(overviewGrp, SWT.NONE);
        sentLabel.setFont(boldFont);
        sentLabel.setText("Sent:");
        Label sentText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        sentText.setLayoutData(gridData);
        sentText.setText(SENT_STATUS_DESC);

        Label acceptedLabel = new Label(overviewGrp, SWT.NONE);
        acceptedLabel.setFont(boldFont);
        acceptedLabel.setText("Accepted:");
        Label acceptedText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        acceptedText.setLayoutData(gridData);
        acceptedText.setText(ACCEPTED_STATUS_DESC);

        Label rejectedLabel = new Label(overviewGrp, SWT.NONE);
        rejectedLabel.setFont(boldFont);
        rejectedLabel.setText("Rejected:");
        Label rejectedText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        rejectedText.setLayoutData(gridData);
        rejectedText.setText(REJECTED_STATUS_DESC);

        Label failedLabel = new Label(overviewGrp, SWT.NONE);
        failedLabel.setFont(boldFont);
        failedLabel.setText("Failed:");
        Label failedText = new Label(overviewGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        failedText.setLayoutData(gridData);
        failedText.setText(FAILED_STATUS_DESC);
    }

    /**
     * Creates the Group and all containing widgets for the outgoing section of
     * the help dialog.
     *
     * @param parent
     *            The parent Composite
     */
    public void createOutgoingGroup(Composite parent) {
        Group outGoingGrp = new Group(parent, SWT.None);
        outGoingGrp.setText("Outgoing Backup Jobs:");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        outGoingGrp.setLayoutData(gridData);
        outGoingGrp.setLayout(new GridLayout(2, false));

        Label outgoingOverviewText = new Label(outGoingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        gridData.horizontalSpan = 2;
        outgoingOverviewText.setLayoutData(gridData);
        outgoingOverviewText.setText(OUTGOING_OVERVIEW_TEXT);

        Label reviewLabel = new Label(outGoingGrp, SWT.NONE);
        FontDescriptor boldDescriptor = FontDescriptor
                .createFrom(reviewLabel.getFont()).setStyle(SWT.BOLD);
        Font boldFont = boldDescriptor.createFont(reviewLabel.getDisplay());
        reviewLabel.setFont(boldFont);
        reviewLabel.setText("Review:");
        Label reviewText = new Label(outGoingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        reviewText.setLayoutData(gridData);
        reviewText.setText(REVIEW_BUTTON_DESC);

        Label sendLabel = new Label(outGoingGrp, SWT.NONE);
        sendLabel.setFont(boldFont);
        sendLabel.setText("Send:");
        Label sendText = new Label(outGoingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        sendText.setLayoutData(gridData);
        sendText.setText(SEND_BUTTON_DESC);

        Label deleteLabel = new Label(outGoingGrp, SWT.NONE);
        deleteLabel.setFont(boldFont);
        deleteLabel.setText("Delete:");
        Label deleteText = new Label(outGoingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        deleteText.setLayoutData(gridData);
        deleteText.setText(DELETE_BUTTON_DESC);

        Label waitLabel = new Label(outGoingGrp, SWT.NONE);
        waitLabel.setFont(boldFont);
        waitLabel.setText("Wait:");
        Label waitText = new Label(outGoingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        waitText.setLayoutData(gridData);
        waitText.setText(WAIT_BUTTON_DESC);
    }

    /**
     * Creates the Group and all containing widgets for the Incoming section of
     * the help dialog.
     *
     * @param parent
     *            The parent Composite
     */
    public void createIncomingGroup(Composite parent) {
        Group incomingGrp = new Group(parent, SWT.None);
        incomingGrp.setText("Incoming Backup Jobs:");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        incomingGrp.setLayoutData(gridData);
        incomingGrp.setLayout(new GridLayout(2, false));

        Label incomingOverviewText = new Label(incomingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        gridData.horizontalSpan = 2;
        incomingOverviewText.setLayoutData(gridData);
        incomingOverviewText.setText(INCOMING_OVERVIEW_TEXT);

        Label reviewLabel = new Label(incomingGrp, SWT.NONE);
        FontDescriptor boldDescriptor = FontDescriptor
                .createFrom(reviewLabel.getFont()).setStyle(SWT.BOLD);
        Font boldFont = boldDescriptor.createFont(reviewLabel.getDisplay());
        reviewLabel.setFont(boldFont);
        reviewLabel.setText("Review:");
        Label reviewText = new Label(incomingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        reviewText.setLayoutData(gridData);
        reviewText.setText(REVIEW_BUTTON_DESC);

        Label acceptLabel = new Label(incomingGrp, SWT.NONE);
        acceptLabel.setFont(boldFont);
        acceptLabel.setText("Accept:");
        Label acceptText = new Label(incomingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        acceptText.setLayoutData(gridData);
        acceptText.setText(ACCEPT_BUTTON_DESC);

        Label rejectLabel = new Label(incomingGrp, SWT.NONE);
        rejectLabel.setFont(boldFont);
        rejectLabel.setText("Reject:");
        Label rejectText = new Label(incomingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        rejectText.setLayoutData(gridData);
        rejectText.setText(REJECT_BUTTON_DESC);

        Label waitLabel = new Label(incomingGrp, SWT.NONE);
        waitLabel.setFont(boldFont);
        waitLabel.setText("Wait:");
        Label waitText = new Label(incomingGrp, SWT.WRAP);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        waitText.setLayoutData(gridData);
        waitText.setText(WAIT_BUTTON_DESC);
    }
}
