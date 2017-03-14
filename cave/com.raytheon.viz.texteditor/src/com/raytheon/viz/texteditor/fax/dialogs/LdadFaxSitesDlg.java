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

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.dataplugin.text.subscription.AutoFaxContainer;
import com.raytheon.uf.common.dataplugin.text.subscription.db.AutoFaxRecord;
import com.raytheon.uf.common.dataplugin.text.subscription.request.AutoFaxRequest;
import com.raytheon.uf.common.dataplugin.text.subscription.request.GetAutoFaxRecordsRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * 26Sep2012    1196      lvenable     Dialog refacter to not block.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class LdadFaxSitesDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LdadFaxSitesDlg.class);

    private Tree faxSiteTree;

    private Label faxNumLbl;

    private Label afosPilLbl;

    private Label companyLbl;

    private Label phoneNumLbl;

    private Label contactLbl;

    private final String faxNumPrefix = "Fax Number: ";

    private final String afosPilPrefix = "AFOS PIL: ";

    private final String companyPrefix = "Company: ";

    private final String phoneNumPrefix = "Phone Number: ";

    private final String contactPrefix = "Contact: ";

    private FaxSiteEditorDlg faxSiteEditorDlg;

    private Button updateDbBtn;

    private AutoFaxContainer addList = new AutoFaxContainer();

    private AutoFaxContainer removeList = new AutoFaxContainer();

    private AutoFaxContainer currentList = new AutoFaxContainer();

    private boolean sortByPil = true;

    public LdadFaxSitesDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("Fax Site Editor");
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
        createViewMenu(menuBar);
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
         * Add Site
         */
        MenuItem addSiteMI = new MenuItem(fileMenu, SWT.NONE);
        addSiteMI.setText("Add Site...");
        addSiteMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addSiteAction();
            }
        });

        /*
         * Edit Site
         */
        MenuItem editSiteMI = new MenuItem(fileMenu, SWT.NONE);
        editSiteMI.setText("Edit Site...");
        editSiteMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                editSiteAction();
            }
        });

        /*
         * Update DB
         */
        MenuItem updateDbMI = new MenuItem(fileMenu, SWT.NONE);
        updateDbMI.setText("Update DB");
        updateDbMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateDBAction();
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

    private void createViewMenu(Menu menuBar) {
        // -------------------------------------
        // Create the View menu
        // -------------------------------------
        MenuItem viewMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        viewMenuItem.setText("&View");

        // Create the View menu item with a View "dropdown" menu
        Menu viewMenu = new Menu(menuBar);
        viewMenuItem.setMenu(viewMenu);

        /*
         * Sort by Fax Number
         */
        MenuItem sortByFaxMI = new MenuItem(viewMenu, SWT.NONE);
        sortByFaxMI.setText("Sort by Fax Number");
        sortByFaxMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                sortByFaxAction();
            }
        });

        /*
         * Sort by AFOS PIL
         */
        MenuItem sortByAfosPilMI = new MenuItem(viewMenu, SWT.NONE);
        sortByAfosPilMI.setText("Sort by AFOS PIL");
        sortByAfosPilMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                sortByAfosPilAction();
            }
        });
    }

    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the Help menu item with a Help "dropdown" menu
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

        /*
         * Add the tree view of existing fax sites
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        gd.heightHint = 450;
        faxSiteTree = new Tree(shell, SWT.MULTI | SWT.BORDER | SWT.SINGLE);
        faxSiteTree.setLayoutData(gd);
        faxSiteTree.setLinesVisible(true);
        faxSiteTree.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                treeSelectionAction();
            }
        });

        getRecordsFromDB();

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group siteInfoGroup = new Group(shell, SWT.NONE);
        siteInfoGroup.setLayout(new GridLayout(1, false));
        siteInfoGroup.setLayoutData(gd);
        siteInfoGroup.setText(" Site Information ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        faxNumLbl = new Label(siteInfoGroup, SWT.NONE);
        faxNumLbl.setLayoutData(gd);
        setLabelText(faxNumLbl, faxNumPrefix, null);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        afosPilLbl = new Label(siteInfoGroup, SWT.NONE);
        afosPilLbl.setLayoutData(gd);
        setLabelText(afosPilLbl, afosPilPrefix, null);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        companyLbl = new Label(siteInfoGroup, SWT.NONE);
        companyLbl.setLayoutData(gd);
        setLabelText(companyLbl, companyPrefix, null);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        phoneNumLbl = new Label(siteInfoGroup, SWT.NONE);
        phoneNumLbl.setLayoutData(gd);
        setLabelText(phoneNumLbl, phoneNumPrefix, null);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        contactLbl = new Label(siteInfoGroup, SWT.NONE);
        contactLbl.setLayoutData(gd);
        setLabelText(contactLbl, contactPrefix, null);

        /*
         * Update DB button
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        updateDbBtn = new Button(shell, SWT.PUSH);
        updateDbBtn.setText("Update DB");
        updateDbBtn.setLayoutData(gd);
        updateDbBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDBAction();
            }
        });
    }

    protected void treeSelectionAction() {
        for (TreeItem treeItem : faxSiteTree.getSelection()) {
            if (null != treeItem.getParentItem()) {
                TreeItem parent = treeItem.getParentItem();
                String index = parent.getText();
                String second = treeItem.getText();
                for (AutoFaxRecord faxRecord : currentList.getAutoFaxList()) {
                    if (sortByPil) {
                        if (index.equals(faxRecord.getId().getAfosPil())
                                && second.equals(faxRecord.getId()
                                        .getFaxNumber())) {
                            updateInfoDisplay(faxRecord);
                        }
                    } else {
                        if (index.equals(faxRecord.getId().getFaxNumber())
                                && second
                                        .equals(faxRecord.getId().getAfosPil())) {
                            updateInfoDisplay(faxRecord);
                        }
                    }
                }
            }
        }
    }

    private void updateInfoDisplay(AutoFaxRecord faxRecord) {
        setLabelText(faxNumLbl, faxNumPrefix, faxRecord.getId().getFaxNumber());
        setLabelText(afosPilLbl, afosPilPrefix, faxRecord.getId().getAfosPil());
        setLabelText(companyLbl, companyPrefix, faxRecord.getCompany());
        setLabelText(phoneNumLbl, phoneNumPrefix, faxRecord.getPhoneNumber());
        setLabelText(contactLbl, contactPrefix, faxRecord.getRecipient());
    }

    private void setLabelText(Label lbl, String prefix, String text) {
        if (text != null) {
            lbl.setText(prefix + text);
        } else {
            lbl.setText(prefix);
        }
    }

    private void addSiteAction() {
        if (faxSiteEditorDlg == null || faxSiteEditorDlg.isDisposed()) {
            faxSiteEditorDlg = new FaxSiteEditorDlg(shell, this);
            faxSiteEditorDlg.open();
        } else {
            faxSiteEditorDlg.bringToTop();
        }
    }

    private void editSiteAction() {

        if (faxSiteTree.getSelection().length > 0) {
            faxSiteEditorDlg = new FaxSiteEditorDlg(shell, this);
            for (TreeItem treeItem : faxSiteTree.getSelection()) {
                if (null != treeItem.getParentItem()) {
                    TreeItem parent = treeItem.getParentItem();
                    String index = parent.getText();
                    String second = treeItem.getText();
                    for (AutoFaxRecord faxRecord : currentList.getAutoFaxList()) {
                        if (sortByPil) {
                            if (index.equals(faxRecord.getId().getAfosPil())
                                    && second.equals(faxRecord.getId()
                                            .getFaxNumber())) {
                                faxSiteEditorDlg.setFaxRecord(faxRecord);
                            }
                        } else {
                            if (index.equals(faxRecord.getId().getFaxNumber())
                                    && second.equals(faxRecord.getId()
                                            .getAfosPil())) {
                                faxSiteEditorDlg.setFaxRecord(faxRecord);
                            }
                        }
                    }
                }
            }
            faxSiteEditorDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    faxSiteEditorDlg = null;
                }
            });
            faxSiteEditorDlg.open();
        }
    }

    private void sortByFaxAction() {
        sortByPil = false;
        populateTree();
    }

    private void sortByAfosPilAction() {
        sortByPil = true;
        populateTree();
    }

    /*
     * TODO - this just fills the tree with sample data
     */
    private void populateTree() {
        // Construct the map used to build the tree.
        TreeMap<String, List<String>> treeMap = new TreeMap<String, List<String>>();
        if (sortByPil) {
            for (AutoFaxRecord faxRecord : currentList.getAutoFaxList()) {
                if (treeMap.get(faxRecord.getId().getAfosPil()) == null) {
                    treeMap.put(faxRecord.getId().getAfosPil(),
                            new ArrayList<String>());
                }
                treeMap.get(faxRecord.getId().getAfosPil()).add(
                        faxRecord.getId().getFaxNumber());
            }
        } else {
            for (AutoFaxRecord faxRecord : currentList.getAutoFaxList()) {
                if (treeMap.get(faxRecord.getId().getFaxNumber()) == null) {
                    treeMap.put(faxRecord.getId().getFaxNumber(),
                            new ArrayList<String>());
                }
                treeMap.get(faxRecord.getId().getFaxNumber()).add(
                        faxRecord.getId().getAfosPil());
            }
        } // We now have a sorted set of the desired key to a list of the
          // secondary values corresponding to that key.

        // Turn off drawing to avoid flicker
        faxSiteTree.removeAll();
        faxSiteTree.setRedraw(false);

        for (String index : treeMap.keySet()) {
            TreeItem item = new TreeItem(faxSiteTree, SWT.NONE);
            item.setText(index);
            item.setExpanded(true);

            for (String secondary : treeMap.get(index)) {
                TreeItem child = new TreeItem(item, SWT.NONE);
                child.setText(secondary);
            }
        }
        // Turn drawing back on!
        faxSiteTree.setRedraw(true);
    }

    protected void addAutoFaxSite(AutoFaxRecord add) {
        addList.add(add);
        currentList.add(add);
        populateTree();
    }

    protected void deleteAutoFaxSite(AutoFaxRecord del) {
        removeList.add(del);
        currentList.getAutoFaxList().remove(del);
        populateTree();
    }

    protected void updateDBAction() {
        for (AutoFaxRecord faxRecord : removeList.getAutoFaxList()) {
            AutoFaxRequest faxReq = new AutoFaxRequest(faxRecord.getId()
                    .getAfosPil(), faxRecord.getId().getFaxNumber(),
                    faxRecord.getPhoneNumber(), faxRecord.getRecipient(),
                    faxRecord.getCompany(), true);
            try {
                ThriftClient.sendRequest(faxReq);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error removing autofax record.", e);
            }
        }
        for (AutoFaxRecord faxRecord : addList.getAutoFaxList()) {
            AutoFaxRequest faxReq = new AutoFaxRequest(faxRecord.getId()
                    .getAfosPil(), faxRecord.getId().getFaxNumber(),
                    faxRecord.getPhoneNumber(), faxRecord.getRecipient(),
                    faxRecord.getCompany(), false);
            try {
                ThriftClient.sendRequest(faxReq);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error adding autofax record.", e);
            }
        }
        removeList.getAutoFaxList().clear();
        addList.getAutoFaxList().clear();
        getRecordsFromDB();
    }

    private void getRecordsFromDB() {
        GetAutoFaxRecordsRequest getRecords = new GetAutoFaxRecordsRequest();
        AutoFaxContainer temp = null;
        try {
            temp = (AutoFaxContainer) ThriftClient.sendRequest(getRecords);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error obtaining auto fax records from DB.", e);
        }
        if (null != temp) {
            currentList = temp;
            populateTree();
        }

    }

    protected void removeSiteFromTree(String afosPil, String faxNumber) {
        if (null != currentList) {
            for (AutoFaxRecord faxRecord : currentList.getAutoFaxList()) {
                if (faxRecord.getId().getAfosPil().equals(afosPil)
                        && faxRecord.getId().getFaxNumber().equals(faxNumber)) {
                    currentList.getAutoFaxList().remove(faxRecord);
                    break;
                }
            }
            populateTree();
        }
    }
}
