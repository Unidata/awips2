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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * A dialog which displays a list of procedures for opening, saving, or
 * deleting.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * 07/31/2012   DR 15036   D. Friedman Ensure current user's procedures
 *                                     are visible.
 * 10/16/2012   1229       rferrel     Made dialog non-blocking.
 * 11 Dec 2013  #2583      lvenable    Added show mine, show all users, and show
 *                                     all radio buttons, fixed a widget disposed
 *                                     error, cleaned up code to prevent buttons
 *                                     from magically appearing, removed dead code,
 *                                     and other code clean up.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class ProcedureListDlg extends CaveSWTDialog {

    /** Flag indicating the data will only be at the root level. */
    protected boolean oneLevel = true;

    /** Text control that contains a selected procedure from the tree viewer. */
    private Text procedureTF;

    /** Tree that holds the procedure data. */
    private TreeViewer treeViewer;

    /** OK button. */
    private Button okBtn;

    /** Cancel button. */
    private Button cancelBtn;

    /** Frozen check box. */
    private Button frozenChk;

    /** Frozen flag. */
    private boolean frozen = false;

    /** Selected filename. */
    private String fileName;

    private ProcedureTree fileTree = new ProcedureTree(null, null);

    /** Flag indicating if all users should be displayed. */
    private boolean showAllUsersFlag = false;

    /** Expand button. */
    private Button expandButton;

    /** Collapse button. */
    private Button collapseButton;

    /** Show current user procedures. */
    private Button showMineRdo = null;

    /** Show all user procedures. */
    private Button showAllUsersRdo = null;

    /** Show all procedures. */
    private Button showAllProceduresRdo = null;

    /** List of buttons that will be enabled or disabled. */
    private List<Button> enableBtnArray = new ArrayList<Button>();

    /**
     * Dialog mode. This determines what functionality the dialog will be able
     * to do.
     */
    public static enum Mode {
        SAVE, OPEN, DELETE
    };

    /**
     * Enumeration to determine what procedures should be shown.
     */
    public static enum ShowType {
        MINE, ALL_USERS, ALL
    };

    protected ShowType selectedShowType = ShowType.MINE;

    /**
     * Dialog mode.
     */
    private final Mode mode;

    /**
     * Constructor.
     * 
     * @param title
     *            Dialog title.
     * @param parent
     *            Parent shell.
     * @param mode
     *            Mode of the dialog.
     */
    public ProcedureListDlg(String title, Shell parent, Mode mode) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK); // Win32
        setText(title);

        this.mode = mode;
    }

    @Override
    protected void opened() {
        // Set the minimum size on the dialog so that it cannot be resized to
        // hide the controls.
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    @Override
    protected void initializeComponents(Shell shell) {

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gd);

        createTextComp(mainComp);
        createButtonComp(mainComp);
    }

    /**
     * Create a composite with the Expand and Collapse buttons.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createExpandComp(Composite parent) {

        Composite expandComp = new Composite(parent, SWT.NONE);
        expandComp.setLayout(new RowLayout(SWT.HORIZONTAL));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 25;
        expandComp.setLayoutData(gd);

        RowData rd = new RowData();
        expandButton = new Button(expandComp, SWT.PUSH);
        expandButton.setText("Expand All");
        expandButton.setLayoutData(rd);
        enableBtnArray.add(expandButton);
        expandButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                treeViewer.expandAll();
            }
        });

        rd = new RowData();
        collapseButton = new Button(expandComp, SWT.PUSH);
        collapseButton.setText("Collapse All");
        collapseButton.setLayoutData(rd);
        enableBtnArray.add(collapseButton);
        collapseButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                treeViewer.collapseAll();
            }
        });
    }

    /**
     * Create the text and tree viewer controls.
     * 
     * @param mainComp
     *            Parent composite.
     */
    protected void createTextComp(Composite mainComp) {
        final Composite textComp = new Composite(mainComp, SWT.NONE);
        textComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textComp.setLayoutData(gd);

        /*
         * Add a text control to the dialog if it isn't in delete mode.
         */
        if (this.mode != Mode.DELETE) {
            procedureTF = new Text(textComp, SWT.BORDER);
            gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
            // gd.widthHint = 150;
            procedureTF.setLayoutData(gd);

            /*
             * If this is the open dialog, make the text control uneditable. The
             * reason for this is that currently this dialog doesn't do anything
             * with the text that the user could enter into the box. In AWIPS I,
             * entering in a name that doesn't exist will bring up the
             * procedures dialog with the entered name but no products. In AWIPS
             * II, the dialog just closes. Until this gets addressed the text
             * control should be uneditable.
             */
            if (this.mode == Mode.OPEN) {
                procedureTF.setEditable(false);
            }
        }

        if (this.mode == Mode.OPEN) {

            Composite showComp = new Composite(textComp, SWT.NONE);
            showComp.setLayout(new GridLayout(3, false));
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            showComp.setLayoutData(gd);

            showMineRdo = new Button(showComp, SWT.RADIO);
            showMineRdo.setText("Show Mine");
            showMineRdo.setSelection(true);
            enableBtnArray.add(showMineRdo);
            showMineRdo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (showMineRdo.getSelection()
                            && selectedShowType != ShowType.MINE) {
                        showProcedureAction(ShowType.MINE);
                    }
                }
            });

            gd = new GridData();
            gd.horizontalIndent = 10;
            showAllUsersRdo = new Button(showComp, SWT.RADIO);
            showAllUsersRdo.setText("Show All Users");
            showAllUsersRdo.setLayoutData(gd);
            enableBtnArray.add(showAllUsersRdo);
            showAllUsersRdo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (showAllUsersRdo.getSelection()
                            && selectedShowType != ShowType.ALL_USERS) {
                        showProcedureAction(ShowType.ALL_USERS);
                    }
                }
            });

            gd = new GridData();
            gd.horizontalIndent = 10;
            showAllProceduresRdo = new Button(showComp, SWT.RADIO);
            showAllProceduresRdo.setText("Show All");
            showAllProceduresRdo.setLayoutData(gd);
            enableBtnArray.add(showAllProceduresRdo);
            showAllProceduresRdo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (showAllProceduresRdo.getSelection()
                            && selectedShowType != ShowType.ALL) {
                        showProcedureAction(ShowType.ALL);
                    }
                }
            });
        }

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 325;
        gd.heightHint = 170;

        treeViewer = new TreeViewer(textComp, SWT.SINGLE | SWT.BORDER);
        treeViewer.getTree().setLayoutData(gd);
        treeViewer.getTree().addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                if (!treeViewer.getTree().isDisposed()
                        && treeViewer.getTree().getSelection().length > 0) {
                    if (procedureTF != null && !procedureTF.isDisposed()) {
                        procedureTF
                                .setText(treeViewer.getTree().getSelection()[0]
                                        .getText());
                    }
                }
            }
        });

        treeViewer.getTree().addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                // Check to see if the selected item has child nodes. If it
                // doesn't then call selectAction.
                if (!checkSelectionToggle(getSelectedTreeItem())) {
                    selectAction();
                }
            }
        });

        updateTreeViewerData();

        // If not in delete mode then add the expand/collapse controls.
        if (this.mode != Mode.DELETE) {
            createExpandComp(textComp);
        }
    }

    private void showProcedureAction(ShowType showType) {
        this.selectedShowType = showType;

        treeViewer.getTree().removeAll();
        treeViewer.getTree().clearAll(true);
        procedureTF.setText("");
        updateTreeViewerData();
    }

    /**
     * Update the tree viewer with data.
     */
    private void updateTreeViewerData() {

        // Set the cursor to a wait cursor. Also set the main composite to not
        // be editable while the data is being retrieved.
        getShell().setCursor(
                Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));
        enableControls(false);

        Job job = new Job("Populating procedures...") {
            protected org.eclipse.core.runtime.IStatus run(
                    org.eclipse.core.runtime.IProgressMonitor monitor) {
                fileTree = populateDataList();
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {

                        // If the treeViewer is null or has been disposed then
                        // return as the dialog has been shutdown.
                        if (treeViewer == null
                                || treeViewer.getTree().isDisposed()) {
                            return;
                        }

                        treeViewer
                                .setContentProvider(new ProcedureTreeContentProvider(
                                        fileTree));
                        treeViewer
                                .setLabelProvider(new ProcedureTreeLabelProvider());
                        treeViewer.setSorter(new ProcedureTreeSorter());

                        // it didn't seem to start with null, the string doesn't
                        // actually mean anything in this case
                        treeViewer.setInput("kickstart");

                        openUserInTreeViewer();

                        // Set the cursor to a normal cursor. Also, reenable the
                        // main composite to be editable.
                        enableControls(true);
                        getShell().setCursor(null);
                    }
                });
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Enable or disable controls based on the flag passed in.
     * 
     * @param enableFlag
     *            True to enable controls, false to disable.
     */
    private void enableControls(boolean enableFlag) {

        for (Button btn : enableBtnArray) {
            if (btn != null && !btn.isDisposed()) {
                btn.setEnabled(enableFlag);
            }
        }
    }

    private void openUserInTreeViewer() {
        if (!oneLevel) {
            IPathManager mgr = PathManagerFactory.getPathManager();
            LocalizationContext ctx = mgr.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            String user = "USER - " + ctx.getContextName();

            // find in the tree
            if (treeViewer.getContentProvider() instanceof ProcedureTreeContentProvider) {
                ProcedureTreeContentProvider content = (ProcedureTreeContentProvider) treeViewer
                        .getContentProvider();
                final Object find = content.findItem(user);
                if (find != null) {
                    treeViewer.setExpandedElements(new Object[] { find });
                    treeViewer.getTree().getDisplay().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            TreeItem[] items = treeViewer.getTree().getItems();
                            if (items != null && items.length > 0)
                                treeViewer.getTree().showItem(
                                        items[items.length - 1]);
                            treeViewer.reveal(find);
                        }
                    });
                }
            }
        }
    }

    /**
     * populate the data list
     */
    protected ProcedureTree populateDataList() {
        ProcedureTree root = new ProcedureTree("root", null);
        IPathManager mgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = mgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        LocalizationFile[] files = mgr.listFiles(ctx,
                ProcedureDlg.PROCEDURES_DIR, null, true, true);
        String[] strings = new String[files.length];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = LocalizationUtil.extractName(files[i].getName());
            root.addChild(strings[i], files[i]);
        }
        this.oneLevel = true;
        return root;
    }

    /**
     * Create the OK and Cancel buttons.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createButtonComp(Composite mainComp) {
        // Add buttom comp
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        buttonComp.setLayout(new RowLayout(SWT.VERTICAL));
        GridData gd = new GridData(SWT.CENTER, SWT.FILL, false, false);
        gd.widthHint = 100;
        buttonComp.setLayoutData(gd);

        RowData rd = new RowData(80, SWT.DEFAULT);
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("Ok");
        okBtn.setLayoutData(rd);
        enableBtnArray.add(okBtn);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAction();
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(rd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        if (mode == Mode.SAVE) {
            rd = new RowData(100, SWT.DEFAULT);
            frozenChk = new Button(buttonComp, SWT.CHECK);
            frozenChk.setText("Freeze time");
            frozenChk.setLayoutData(rd);
            frozenChk.setSelection(frozen);
            frozenChk.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    frozen = !frozen;
                }
            });
        }
    }

    /**
     * @return the fileName
     */
    public String getSelectedFileName() {
        return fileName;
    }

    /**
     * Method call to determine if frozen is selected.
     * 
     * @return True if frozen, false otherwise.
     */
    public boolean isFrozen() {
        return frozen;
    }

    /**
     * returns true if the item was toggled, toggles (expands/collapses ) if the
     * item has children
     * 
     * basically this method check to see if the node in the tree has children.
     * if the node has children it will expand the tree to show the children.
     * 
     * @param procedureTree
     *            The procedure tree.
     * @return True if there are children of the selection.
     */
    private boolean checkSelectionToggle(ProcedureTree procedureTree) {

        if (procedureTree.hasChildren()) {
            // toggle and return true
            boolean expanded = treeViewer.getExpandedState(procedureTree);
            treeViewer.setExpandedState(procedureTree, !expanded);
            return true;
        }

        return false;
    }

    /**
     * Get the procedure tree object from the selected item in the tree viewer.
     * 
     * @return The procedure tree.
     */
    private ProcedureTree getSelectedTreeItem() {
        Object tmp = null;
        if (!treeViewer.getSelection().isEmpty()) {
            tmp = treeViewer.getTree().getSelection()[0].getData();
        }

        if (tmp instanceof ProcedureTree) {
            return (ProcedureTree) tmp;
        }

        return null;
    }

    /**
     * Action method that determines what mode the dialog is in. Action from the
     * OK button or double-clicking the item in the tree viewer.
     */
    private void selectAction() {

        if (mode == Mode.SAVE) {
            if (procedureTF.getText() == null)
                return;
            // Make sure also not in the list
            // append .xml to files without it
            if (!procedureTF.getText().endsWith(".xml")) {
                procedureTF.setText(procedureTF.getText().concat(".xml"));
            }
            if (dataListContains(procedureTF.getText())) {
                if (ProcedureDlg.getDialog(procedureTF.getText()) != null) {
                    // User cannot save if dialog is open.
                    MessageDialog
                            .openError(
                                    shell,
                                    "Cannot Save Procedure",
                                    "The procedure "
                                            + procedureTF.getText()
                                            + " is currently open. It cannot be overwritten until it is closed or saved under another name.");
                } else {
                    // Pop up a warning
                    boolean result = MessageDialog.openQuestion(shell,
                            "Confirm Overwrite",
                            "The procedure " + procedureTF.getText()
                                    + " already exists.  Overwrite anyways?");
                    if (result == true) {
                        fileName = procedureTF.getText();
                        close();
                    }
                }
            } else {
                fileName = procedureTF.getText();
                close();
            }
        } else if (mode == Mode.OPEN) {
            fileName = procedureTF.getText();
            ProcedureTree tmp = getSelectedTreeItem();
            if (tmp != null) {
                // it must be a procedure tree, that is what the content
                // provider uses internally
                LocalizationFile selectedFile = ((ProcedureTree) tmp).getFile();

                if (selectedFile == null) {
                    displayOpenErrorDialog();
                    return;
                }
                setReturnValue(selectedFile);
            } else {
                displayOpenErrorDialog();
                return;
            }

            close();
        } else if (mode == Mode.DELETE) {

            TreeItem[] selection = treeViewer.getTree().getSelection();
            if (selection.length > 0) {
                boolean result = MessageDialog.openQuestion(shell,
                        "Confirm Deletion",
                        "Are you sure you want to delete the procedure \""
                                + selection[0].getText() + "\"");
                if (result == true) {
                    fileName = selection[0].getText();
                    ProcedureTree tmp = getSelectedTreeItem();
                    if (tmp != null) {
                        // it must be a procedure tree, that is what the content
                        // provider uses internally
                        LocalizationFile selectedFile = ((ProcedureTree) tmp)
                                .getFile();
                        setReturnValue(selectedFile);
                    }
                    close();
                }
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
                mb.setText("Selection Error");
                mb.setMessage("You must select an item to delete it.");
                mb.open();
            }
        }
    }

    /**
     * Display a dialog letting the user know they have to select a file to open
     * it.
     */
    private void displayOpenErrorDialog() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
        mb.setText("Selection Error");
        mb.setMessage("You must select a file to open it.");
        mb.open();
    }

    /**
     * Determine if the data list contains the specified filename.
     * 
     * @param fileName
     *            Filename.
     * @return True if the data list contains the filename, false otherwise.
     */
    private boolean dataListContains(String fileName) {
        TreeItem[] items = treeViewer.getTree().getItems();
        for (TreeItem item : items) {
            if (item.getText().equals(fileName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Method call to determine if all users should be shown.
     * 
     * @return True if all users should be shown, false otherwise.
     */
    protected boolean showAllUsers() {
        return showAllUsersFlag;
    }
}
