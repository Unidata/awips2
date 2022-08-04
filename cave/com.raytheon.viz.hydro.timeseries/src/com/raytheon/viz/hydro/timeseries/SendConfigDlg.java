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
package com.raytheon.viz.hydro.timeseries;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydrocommon.ui.CaveHydroSWTDialog;

/**
 * Dialog add remove distribution directories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2011            mpduff     Initial creation
 * Feb 06, 2013 1578       rferrel    Code cleanup for non-blocking dialog.
 * May 09, 2016 5483       bkowal     Fix GUI sizing issues.
 * Sep 21, 2018 7379       mduff       Moved for PDC Refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class SendConfigDlg extends CaveHydroSWTDialog {

    /** Manager for handling the shef xml data. */
    private ShefIssueMgr shefIssueMgr;

    /** Distribute product check box. */
    private Button distChk;

    /** Internal directory check box. */
    private Button directoryChk;

    /** Button to bring up dialog to add a directory . */
    private Button addBtn;

    /** Button to remove selectted directory. */
    private Button removeBtn;

    /** Button to remove all directories in the list. */
    private Button removeAllBtn;

    /** List to display the directories. */
    private List directoryList;

    /**
     * @param parentShell
     * @param style
     */
    protected SendConfigDlg(Shell parentShell) {
        super(parentShell);
        setText("Send Product Actions");
        shefIssueMgr = ShefIssueMgr.getInstance();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Setup layout of dialog.
     */
    private void initializeComponents() {
        createDistributionGroup();
        createDirectoryGroup();
        createBottomButtons();
    }

    private void createDistributionGroup() {
        boolean selected = AppsDefaults.getInstance()
                .getBoolean("timeseries_dist_shef", false);
        GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, true);
        Group distGroup = new Group(shell, SWT.NONE);
        distGroup.setText("Distribution Method");
        GridLayout gridLayout = new GridLayout(1, false);
        distGroup.setLayout(gridLayout);
        distGroup.setLayoutData(gd);

        Composite distComp = new Composite(distGroup, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        distComp.setLayout(gl);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        distChk = new Button(distComp, SWT.CHECK);
        distChk.setText("Distribute Product");
        distChk.setLayoutData(gd);
        distChk.setSelection(selected);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        directoryChk = new Button(distComp, SWT.CHECK);
        directoryChk.setText("Internal Directory");
        directoryChk.setLayoutData(gd);
        directoryChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                activateDirectoryWidgets();
            }
        });
    }

    private void createDirectoryGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        Group dirGroup = new Group(shell, SWT.NONE);
        dirGroup.setText("Distribution Directories");
        GridLayout gridLayout = new GridLayout(1, false);
        dirGroup.setLayout(gridLayout);
        dirGroup.setLayoutData(gd);

        Composite dirComp = new Composite(dirGroup, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        dirComp.setLayout(gl);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        directoryList = new List(dirGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        gd.heightHint = directoryList.getItemHeight() * 15;
        directoryList.setLayoutData(gd);

        Composite buttonComp = new Composite(dirGroup, SWT.NONE);

        buttonComp.setLayout(new GridLayout(3, true));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                DirectoryDialog fd = new DirectoryDialog(shell, SWT.OPEN);
                String dir = fd.open();
                if (dir != null) {
                    directoryList.add(dir);
                    String[] items = directoryList.getItems();
                    java.util.Arrays.sort(items);
                    directoryList.removeAll();
                    for (String s : items) {
                        directoryList.add(s);
                    }
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setText("Remove");
        removeBtn.setLayoutData(gd);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (directoryList.getSelectionIndex() != -1) {
                    directoryList.remove(directoryList.getSelectionIndex());
                } else {
                    showMessage(shell, SWT.ICON_WARNING | SWT.OK,
                            "Select Directory",
                            "Please select a directory to remove.");
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        removeAllBtn = new Button(buttonComp, SWT.PUSH);
        removeAllBtn.setText("Remove All");
        removeAllBtn.setLayoutData(gd);
        removeAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                directoryList.removeAll();
            }
        });
    }

    /**
     * Layout the button at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        buttonComp.setLayoutData(gd);

        final int buttonMinimumWidth = buttonComp.getDisplay().getDPI().x;

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = buttonMinimumWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                applyChanges();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = buttonMinimumWidth;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                applyChanges();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = buttonMinimumWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    @Override
    protected void opened() {
        super.opened();
        // Populate the dialog
        ShefIssueXML xml = this.shefIssueMgr.getShefIssueXml();

        if (xml != null) {
            if (xml.isDistributeProduct()) {
                this.distChk.setSelection(true);
            }

            if (xml.isDirCopy()) {
                this.directoryChk.setSelection(true);
            }

            InternalDirectoryXML dirList = xml.getInternalDirectory();
            for (String dir : dirList.getDirectories()) {
                this.directoryList.add(dir);
            }
        }

        activateDirectoryWidgets();
    }

    /**
     * Save the changes based on dialog selections.
     */
    private void applyChanges() {
        ShefIssueXML xml = this.shefIssueMgr.getShefIssueXml();
        if (xml == null) {
            xml = new ShefIssueXML();
        }
        xml.setDirCopy(this.directoryChk.getSelection());
        xml.setDistributeProduct(this.distChk.getSelection());
        String[] items = directoryList.getItems();
        ArrayList<String> arl = new ArrayList<>(Arrays.asList(items));
        xml.getInternalDirectory().setDirectories(arl);
        this.shefIssueMgr.saveXml();
    }

    @Override
    protected void disposed() {
        super.disposed();

        if (shefIssueMgr != null) {
            ShefIssueMgr.recycle();
        }
    }

    /**
     * Set enable status of directory widgets.
     */
    private void activateDirectoryWidgets() {
        boolean activate = directoryChk.getSelection();

        directoryList.setEnabled(activate);
        addBtn.setEnabled(activate);
        removeBtn.setEnabled(activate);
        removeAllBtn.setEnabled(activate);
    }

    /**
     * Show a dialog message.
     * 
     * @param shell
     *            The parent shell
     * @param style
     *            The dialog style
     * @param title
     *            The dialog title
     * @param msg
     *            The dialog message
     * @return The value representing the button clicked on the dialog
     */
    private int showMessage(Shell shell, int style, String title, String msg) {
        MessageBox messageBox = new MessageBox(shell, style);
        messageBox.setText(title);
        messageBox.setMessage(msg);
        return messageBox.open();
    }
}
