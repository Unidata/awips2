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
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.HistoryList.IHistoryListener;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Dialog to get user options on history to display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Oct 16, 2012 1229       rferrel     Changes for non-blocking AlterBundleDlg.
 * Oct 16, 2012 1229       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class HistoryListDlg extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HistoryListDlg.class);

    private List dataList;

    private final int BOTTOM_BTN_WIDTH = 90;

    private Button originalRdo;

    private Button currentRdo;

    private Button loadBtn;

    private Button alterBundleBtn;

    private Button copyOutBtn;

    private Button closeBtn;

    private AlterBundleDlg alterDlg;

    public HistoryListDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("History List");

        try {
            if (HistoryList.getInstance().getLabels().length == 0) {
                HistoryList.getInstance().addBundle();
            }
        } catch (VizException e) {
            // ignore
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        initializeComponents();

        addHistoryAndShellListeners();
    }

    /**
     * Add listeners for the history list and the shell.
     */
    private void addHistoryAndShellListeners() {
        /*
         * Listener for updating the history list.
         */
        final IHistoryListener historyListener = new IHistoryListener() {

            @Override
            public void historyListUpdated() {
                VizApp.runAsync(new Runnable() {

                    /*
                     * (non-Javadoc)
                     * 
                     * @see java.lang.Runnable#run()
                     */
                    @Override
                    public void run() {
                        String[] labels = HistoryList.getInstance().getLabels();
                        if (!dataList.isDisposed()) {
                            dataList.setItems(labels);
                        }
                    }
                });
            }
        };

        HistoryList.getInstance().addHistoryListener(historyListener);

        /*
         * Add Copy Out change listener
         */
        final ProcedureComm.ICopyOutStateChangeListener changeListener = new ProcedureComm.ICopyOutStateChangeListener() {

            @Override
            public void copyOutStateChange() {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (!copyOutBtn.isDisposed()) {
                            copyOutBtn.setEnabled(ProcedureComm.getInstance()
                                    .getCopyListenerCount() > 0);
                        }
                    }
                });
            }
        };

        ProcedureComm.getInstance().addCopyOutStateListener(changeListener);

        /*
         * Add a listener for when the shell closes.
         */
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent arg0) {

                HistoryList.getInstance()
                        .removeHistoryListener(historyListener);
                ProcedureComm.getInstance().removeCopyOutStateListener(
                        changeListener);
            }
        });
    }

    private void initializeComponents() {
        createListBox();
        createBottomControls();
    }

    private void createListBox() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 175;
        gd.heightHint = 125;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        dataList.setLayoutData(gd);
        String[] labels = HistoryList.getInstance().getLabels();
        dataList.setItems(labels);
    }

    private void createBottomControls() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        originalRdo = new Button(buttonComp, SWT.RADIO);
        originalRdo.setText("Original");
        originalRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        currentRdo = new Button(buttonComp, SWT.RADIO);
        currentRdo.setText("Current");
        currentRdo.setSelection(true);
        currentRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        loadBtn = new Button(buttonComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadAction();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        copyOutBtn = new Button(buttonComp, SWT.PUSH);
        copyOutBtn.setText("Copy Out");
        copyOutBtn.setLayoutData(gd);
        copyOutBtn.setEnabled(ProcedureComm.getInstance()
                .getCopyListenerCount() > 0);
        final HistoryListDlg thisDlg = this;
        copyOutBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = dataList.getSelectionIndex();
                if (idx > -1) {
                    try {
                        String label = HistoryList.getInstance().getLabels()[dataList
                                .getSelectionIndex()];
                        BundlePair bp = new BundlePair();
                        bp.xml = HistoryList.getInstance().getBundleAsString(
                                idx, false);
                        bp.name = label;
                        ProcedureComm.getInstance().copyOut(bp, thisDlg);
                    } catch (VizException e) {
                        ErrorDialog.openError(shell, "Error",
                                "Error loading bundle", new Status(
                                        Status.ERROR, UiPlugin.PLUGIN_ID,
                                        "Error loading Bundle", e));
                    }
                }

            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        alterBundleBtn = new Button(buttonComp, SWT.PUSH);
        alterBundleBtn.setText("Alter Bundle...");
        alterBundleBtn.setLayoutData(gd);
        alterBundleBtn.setEnabled(true);
        alterBundleBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                alterBundle();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    private void alterBundle() {
        if (this.dataList.getSelectionIndex() < 0) {
            return;
        }

        try {
            if (mustCreate(alterDlg)) {
                Bundle b = HistoryList.getInstance().getBundle(
                        dataList.getSelectionIndex(), false);
                alterDlg = new AlterBundleDlg(b, getShell());
                alterDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Bundle) {
                            // Load was issued in alterBundleDlg
                            Bundle b = (Bundle) returnValue;
                            loadAlterBundle(b);
                        }
                    }
                });
                alterDlg.open();
            } else {
                alterDlg.bringToTop();
            }
        } catch (VizException e) {
            final String err = "Error altering bundle";
            statusHandler.handle(Priority.PROBLEM, err, e);
        }
    }

    private void loadAlterBundle(Bundle b) {
        String editorName = null;

        if (b.getDisplays().length > 0) {
            editorName = DescriptorMap.getEditorId(b.getDisplays()[0]
                    .getDescriptor().getClass().getName());
        }

        AbstractEditor editor = UiUtil.createOrOpenEditor(editorName,
                b.getDisplays());

        for (IDisplayPane pane : editor.getDisplayPanes()) {
            pane.getRenderableDisplay().getDescriptor().getResourceList()
                    .clear();
        }

        ProcedureLoadJob.getInstance().enqueue(b, editor);
    }

    /**
     * Action performed when the load button is clicked. The selected item is
     * loaded into CAVE.
     */
    private void loadAction() {
        String editorName = null;
        try {
            Bundle b = HistoryList.getInstance().getBundle(
                    dataList.getSelectionIndex(), true);

            if (b != null && b.getDisplays() != null
                    && b.getDisplays().length > 0) {
                editorName = DescriptorMap.getEditorId(b.getDisplays()[0]
                        .getDescriptor().getClass().getName());
            }

            AbstractEditor editor = UiUtil.createOrOpenEditor(editorName,
                    b.getDisplays());

            if (dataList.getSelectionIndex() < 0) {
                return;
            }

            ProcedureLoadJob.getInstance().enqueue(b, editor);
        } catch (VizException e) {
            statusHandler.handle(Priority.SIGNIFICANT, "Error loading bundle",
                    e);
        }
    }
}
