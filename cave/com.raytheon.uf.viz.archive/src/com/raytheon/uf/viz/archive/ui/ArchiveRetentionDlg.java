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
package com.raytheon.uf.viz.archive.ui;

import java.util.Calendar;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.viz.archive.data.IArchiveTotals;
import com.raytheon.uf.viz.archive.ui.ArchiveTableComp.TableType;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Archive retention dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2013 #1964      lvenable     Initial creation
 * May 31, 2013 #1965      bgonzale     Initial work for updating retention configurations.
 * Jun 10, 2013 #1966      rferrel      Implemented hooks to get display and save to work.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ArchiveRetentionDlg extends AbstractArchiveDlg implements
        IArchiveTotals, IModifyListener {

    /** Current Archive/Category selection's minimum retention hours. */
    private RetentionHours minRetention;

    /** Current Archive/Category selection's extended retention hours. */
    private RetentionHours extRetention;

    /** Displays the total number of selected items for all tables. */
    private Label totalSelectedItems;

    /** Displays the total size of selected items. */
    private Label totalSizeLbl;

    /** Performs save action button. */
    private Button saveBtn;

    /** Flag set when user wants to close with unsaved modifications. */
    boolean closeFlag = false;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public ArchiveRetentionDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.MODE_INDEPENDENT | CAVE.INDEPENDENT_SHELL);
        this.setSelect = true;
        this.tableType = TableType.Retention;
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
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
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
        setText("Archive Retention");
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);
        ArchiveConfigManager.getInstance().reset();

        init();
    }

    /**
     * Initialize method to create all of the composite & controls.
     */
    private void init() {
        createRetentionControls();
        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
    }

    /**
     * Create the retention controls.
     */
    private void createRetentionControls() {
        Composite retentionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        retentionComp.setLayout(gl);
        retentionComp.setLayoutData(gd);

        /*
         * Top row of controls.
         */
        createComboControls(retentionComp);
        createTable();

        // composite for retention time selection
        Composite selectionComp = new Composite(retentionComp, SWT.NONE);
        selectionComp.setLayout(new GridLayout(3, true));
        selectionComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        gd = new GridData();
        gd.horizontalIndent = 20;
        Label minRetentionLbl = new Label(selectionComp, SWT.NONE);
        minRetentionLbl.setText("Minimum Retention: ");
        minRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Spinner minRetentionSpnr = new Spinner(selectionComp, SWT.BORDER);
        minRetentionSpnr.setIncrement(1);
        minRetentionSpnr.setPageIncrement(5);
        minRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        minRetentionSpnr.setMinimum(1);
        minRetentionSpnr.setLayoutData(gd);

        Combo minRetentionCbo = new Combo(selectionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        minRetention = new RetentionHours(1, minRetentionSpnr, minRetentionCbo) {

            @Override
            protected boolean handleTimeSelection() {
                boolean state = super.handleTimeSelection();
                getSelectedArchive().setRetentionHours(getHours());
                return state;
            }
        };
        minRetention.addModifyListener(this);

        /*
         * Bottom row of controls.
         */
        gd = new GridData();
        gd.horizontalIndent = 20;
        Label extRetentionLbl = new Label(selectionComp, SWT.NONE);
        extRetentionLbl.setText("Extended Retention: ");
        extRetentionLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Spinner extRetentionSpnr = new Spinner(selectionComp, SWT.BORDER);
        extRetentionSpnr.setIncrement(1);
        extRetentionSpnr.setPageIncrement(5);
        extRetentionSpnr.setMaximum(Integer.MAX_VALUE);
        extRetentionSpnr.setMinimum(0);
        extRetentionSpnr.setLayoutData(gd);

        Combo extRetentionCbo = new Combo(selectionComp, SWT.VERTICAL
                | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        extRetention = new RetentionHours(1, extRetentionSpnr, extRetentionCbo) {

            @Override
            protected boolean handleTimeSelection() {
                boolean state = super.handleTimeSelection();
                getSelectedCategory().setRetentionHours(getHours());
                return state;
            }
        };
        extRetention.addModifyListener(this);
    }

    /**
     * Create the bottom action buttons.
     */
    private void createBottomActionButtons() {

        Composite actionControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionControlComp.setLayout(gl);
        actionControlComp.setLayoutData(gd);

        // TODO - For the future ?
        // Button calcSizeBtn = new Button(actionControlComp, SWT.PUSH);
        // calcSizeBtn.setText(" Calculate Sizes ");
        // calcSizeBtn.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // // TODO : add calculate size functionality
        // // With Roger's automated size calculation code, this doesn't
        // // seem relevant unless it is for calculating compressed size
        // }
        // });

        saveBtn = new Button(actionControlComp, SWT.PUSH);
        saveBtn.setText(" Save ");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                ArchiveConfigManager manager = ArchiveConfigManager
                        .getInstance();
                manager.save();
                saveBtn.setEnabled(false);
                clearModified();
            }
        });
        saveBtn.setEnabled(false);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(actionControlComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (verifyClose()) {
                    close();
                } else {
                    e.doit = false;
                }
            }
        });
    }

    /**
     * When unsaved modifications this asks the user to verify the close.
     * 
     * @return true when okay to close.
     */
    private boolean verifyClose() {
        boolean state = true;
        if (isModified()) {
            MessageBox box = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK
                    | SWT.CANCEL);
            box.setText("Close Retention");
            box.setMessage("Unsave changes.\nSelect OK to continue.");
            state = box.open() == SWT.OK;
        }
        closeFlag = state;
        return state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#setTotalSizeText(java
     * .lang.String)
     */
    @Override
    protected void setTotalSizeText(String sizeStringText) {
        if (totalSizeLbl != null && !totalSizeLbl.isDisposed()) {
            totalSizeLbl.setText(sizeStringText);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#setTotalSelectedItems
     * (int)
     */
    @Override
    protected void setTotalSelectedItems(int totalSize) {
        if (totalSelectedItems != null && !totalSelectedItems.isDisposed()) {
            totalSelectedItems.setText("" + totalSize);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#getStart()
     */
    @Override
    protected Calendar getStart() {
        // display all elements so no start bound
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#getEnd()
     */
    @Override
    protected Calendar getEnd() {
        // display all elements so no end bound
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#updateTotals(java.util
     * .List)
     */
    @Override
    public void updateTotals(List<DisplayData> displayDatas) {
        super.updateTotals(displayDatas);
        if (displayDatas != null) {
            for (DisplayData displayData : displayDatas) {
                displayData.updateCategory();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#archiveComboSelection()
     */
    @Override
    protected void archiveComboSelection() {
        super.archiveComboSelection();
        minRetention.setHours(getSelectedArchive().getRetentionHours());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#categoryComboSelection
     * ()
     */
    @Override
    protected void categoryComboSelection() {
        super.categoryComboSelection();
        extRetention.setHours(getSelectedCategory().getRetentionHours());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.IModifyListener#modified()
     */
    @Override
    public void modified() {
        saveBtn.setEnabled(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#clearModified()
     */
    @Override
    public void clearModified() {
        super.clearModified();
        minRetention.clearModified();
        extRetention.clearModified();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        addModifiedListener(this);
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                if (closeFlag || !isModified()) {
                    return;
                }

                e.doit = false;
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (verifyClose()) {
                            close();
                        }
                    }
                });
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.archive.ui.AbstractArchiveDlg#disposed()
     */
    @Override
    protected void disposed() {
        minRetention.removeModifyListener(this);
        extRetention.removeModifyListener(this);
        removeModifiedListener(this);
        super.disposed();
    }

    /**
     * Indicate unsaved user changes.
     * 
     * @return modified
     */
    private boolean isModified() {
        return (saveBtn != null) && !saveBtn.isDisposed()
                && saveBtn.isEnabled();
    }
}
