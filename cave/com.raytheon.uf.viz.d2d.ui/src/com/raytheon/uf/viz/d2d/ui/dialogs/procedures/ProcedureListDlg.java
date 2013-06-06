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

import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IWorkbenchActionConstants;

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
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class ProcedureListDlg extends CaveSWTDialog {

    protected boolean oneLevel = true;

    private Font font;

    private Text procedureTF;

    private TreeViewer treeViewer;

    private Button okBtn;

    private Button cancelBtn;

    private Button frozenBtn;

    private boolean frozen = false;

    String fileName;

    private ProcedureTree fileTree = new ProcedureTree(null, null);

    public static enum Mode {
        SAVE, OPEN, DELETE
    };

    private final Mode mode;

    public ProcedureListDlg(String title, Shell parent, Mode mode) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK); // Win32
        setText(title);

        this.mode = mode;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        font = new Font(shell.getDisplay(), "Courier", 11, SWT.BOLD);

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        mainComp.setLayoutData(gd);

        createTextComp(mainComp);
        createButtonComp(mainComp);
    }

    protected void createExpandComp(Composite parent) {
        Composite expandComp = new Composite(parent, SWT.NONE);
        expandComp.setLayout(new RowLayout(SWT.HORIZONTAL));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, false);
        gd.widthHint = 25;
        expandComp.setLayoutData(gd);

        if (this.mode == Mode.DELETE) {
            gd.exclude = true;
        }

        RowData rd = new RowData();
        Button expandButton = new Button(expandComp, SWT.PUSH);
        expandButton.setText("Expand All");
        expandButton.setLayoutData(rd);
        expandButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                expandAll();
            }
        });

        rd = new RowData();
        Button collapseButton = new Button(expandComp, SWT.PUSH);
        collapseButton.setText("Collapse All");
        collapseButton.setLayoutData(rd);
        collapseButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                collapseAll();
            }
        });

    }

    private void expandAll() {
        treeViewer.expandAll();
    }

    private void collapseAll() {
        treeViewer.collapseAll();
    }

    /**
     * @param mainComp
     */
    protected void createTextComp(Composite mainComp) {
        final Cursor cursor = getShell().getCursor();
        getShell().setCursor(
                Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));
        final Composite textComp = new Composite(mainComp, SWT.NONE);
        textComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 150;
        textComp.setLayoutData(gd);

        procedureTF = new Text(textComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.widthHint = 150;
        if (this.mode == Mode.DELETE) {
            gd.exclude = true;
        }
        procedureTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 175;
        treeViewer = new TreeViewer(textComp, SWT.SINGLE | SWT.BORDER
                | SWT.FILL);
        treeViewer.getTree().setLayoutData(gd);
        treeViewer.getTree().addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent arg0) {

            }

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                if (!treeViewer.getTree().isDisposed()
                        && treeViewer.getTree().getSelection().length > 0) {
                    procedureTF.setText(treeViewer.getTree().getSelection()[0]
                            .getText());
                }
            }

        });
        treeViewer.getTree().addMouseListener(new MouseListener() {
            @Override
            public void mouseUp(MouseEvent e) {
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                selectAction(true);
            }
        });

        Job job = new Job("Populating procedures...") {
            protected org.eclipse.core.runtime.IStatus run(
                    org.eclipse.core.runtime.IProgressMonitor monitor) {
                fileTree = populateDataList(treeViewer);
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        treeViewer
                                .setContentProvider(new ProcedureTreeContentProvider(
                                        fileTree));
                        treeViewer
                                .setLabelProvider(new ProcedureTreeLabelProvider());
                        treeViewer.setSorter(new ProcedureTreeSorter());

                        // uncomment following function call to enable the right
                        // click
                        // context
                        // menu
                        // createContextMenu();

                        // it didn't seem to start with null, the string doesn't
                        // actually mean
                        // anything in this case
                        treeViewer.setInput("kickstart");

                        openUserInTreeViewer();

                        createExpandComp(textComp);
                        getShell().setCursor(cursor);
                    }
                });
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    private void createContextMenu() {
        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            public void menuAboutToShow(IMenuManager mgr) {
                fillContextMenu(mgr);
            }
        });

        Menu menu = menuMgr.createContextMenu(treeViewer.getControl());
        treeViewer.getControl().setMenu(menu);
    }

    protected void fillContextMenu(IMenuManager mgr) {
        if (treeViewer.getSelection().isEmpty()) {
            return;
        }
        final Object tmp = treeViewer.getTree().getSelection()[0].getData();
        if (tmp instanceof ProcedureTree) {
            ProcedureTree sel = (ProcedureTree) tmp;

            // if there is a valid file add load option
            if (sel.getFile() != null) {
                String text = "";
                if (this.mode == ProcedureListDlg.Mode.DELETE) {
                    text = "Delete this";
                } else if (this.mode == ProcedureListDlg.Mode.OPEN) {
                    text = "Open this";
                } else if (this.mode == ProcedureListDlg.Mode.SAVE) {
                    text = "Save as this";
                } else {
                    text = "Action";
                }
                mgr.add(new Action(text) {
                    public void run() {
                        selectAction(false);
                    }
                });

                // add separator
                mgr.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
            } else {
                // add expand this option
                String text = "";
                final boolean state = treeViewer.getExpandedState(tmp);
                if (state) {
                    text = "Collapse";
                } else {
                    text = "Expand";
                }

                mgr.add(new Action(text) {
                    public void run() {
                        treeViewer.setExpandedState(tmp, !state);
                    }
                });

                // add separator
                mgr.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
            }

            // add expand/collapse options
            mgr.add(new Action("Expand All") {
                public void run() {
                    expandAll();
                }
            });
            mgr.add(new Action("Collapse All") {
                public void run() {
                    collapseAll();
                }
            });
        }
    }

    protected void openUserInTreeViewer() {
        if (this.oneLevel) {
            // do nothing for single level
        } else {
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
    protected ProcedureTree populateDataList(TreeViewer treeViewer) {
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
     * @param mainComp
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
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAction(false);
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
            frozenBtn = new Button(buttonComp, SWT.CHECK);
            frozenBtn.setText("Freeze time");
            frozenBtn.setLayoutData(rd);
            frozenBtn.setSelection(frozen);
            frozenBtn.addSelectionListener(new SelectionAdapter() {
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

    public boolean isFrozen() {
        return frozen;
    }

    /**
     * returns true if the item was toggled, toggles if the item has children
     * 
     * @param obj
     * @return
     */
    private boolean checkSelectionToggle(Object obj) {
        if (obj instanceof ProcedureTree) {
            ProcedureTree item = (ProcedureTree) obj;
            if (item.hasChildren()) {
                // toggle and return true
                boolean expanded = treeViewer.getExpandedState(obj);
                treeViewer.setExpandedState(obj, !expanded);
                return true;
            }
        }
        return false;
    }

    public void selectAction(boolean fromDblClick) {
        Object tmp = null;
        if (!treeViewer.getSelection().isEmpty()) {
            tmp = treeViewer.getTree().getSelection()[0].getData();
        }
        if (fromDblClick && checkSelectionToggle(tmp)) {
            return;
        }

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
            if (tmp instanceof ProcedureTree) {
                // it must be a procedure tree, that is what the content
                // provider uses internally
                LocalizationFile selectedFile = ((ProcedureTree) tmp).getFile();
                setReturnValue(selectedFile);
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
                    if (tmp instanceof ProcedureTree) {
                        // it must be a procedure tree, that is what the content
                        // provider uses internally
                        LocalizationFile selectedFile = ((ProcedureTree) tmp)
                                .getFile();
                        setReturnValue(selectedFile);
                    }
                    close();
                }
            }
        }
    }

    private boolean dataListContains(String fileName) {
        TreeItem[] items = treeViewer.getTree().getItems();
        for (TreeItem item : items) {
            if (item.getText().equals(fileName)) {
                return true;
            }
        }
        return false;
    }
}
