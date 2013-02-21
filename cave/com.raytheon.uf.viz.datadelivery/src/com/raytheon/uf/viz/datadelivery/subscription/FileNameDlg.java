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
package com.raytheon.uf.viz.datadelivery.subscription;

import org.eclipse.swt.SWT;
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

import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Create a copy of a subscription dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            mpduff      Initial creation.
 * Dec 17, 2012   1434     mpduff      Don't allow underscores in name.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FileNameDlg extends CaveSWTDialog {
    /** Filename text field */
    private Text nameTxt;

    /** The original subscription name */
    private final String origName;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param origName
     *            The original subscription name
     */
    public FileNameDlg(Shell parent, String origName) {
        super(parent, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL);
        this.setText("Copy Subscription");
        this.origName = origName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 275;
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 8;
        gl.horizontalSpacing = 8;
        gl.marginTop = 8;
        gl.marginBottom = 8;

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        // New Subscription Name text box
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label lbl = new Label(mainComp, SWT.NONE);
        lbl.setText("New Subscription Name:");
        lbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        nameTxt = new Text(mainComp, SWT.BORDER);
        nameTxt.setText(origName);
        nameTxt.selectAll();
        nameTxt.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        // OK button
        int btnWidth = 75;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (handleOk()) {
                    close();
                }
            }
        });

        // Cancel button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * OK Button action handler.
     */
    private boolean handleOk() {
        if ((nameTxt.getText() != null) && (!nameTxt.getText().isEmpty())
                && (!nameTxt.getText().contains("_"))) {
            setReturnValue(nameTxt.getText());
            return true;
        } else {
            String title;
            String message;
            if (nameTxt.getText() == null || nameTxt.getText().isEmpty()) {
                title = "Name Required";
                message = "Name required. A Subscription Name must be entered.";
            } else {
                title = "Name Invalid";
                message = "Underscore is not a valid character for a subscription name.";
            }

            DataDeliveryUtils.showMessage(getShell(), SWT.OK, title, message);
            return false;
        }
    }
}
