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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.archive.data.ArchiveInfo;
import com.raytheon.uf.viz.archive.data.CategoryInfo;
import com.raytheon.uf.viz.archive.data.IArchiveTotals;
import com.raytheon.uf.viz.archive.data.ILoadDisplayDataListener;
import com.raytheon.uf.viz.archive.data.IRetentionHour;
import com.raytheon.uf.viz.archive.data.IUpdateListener;
import com.raytheon.uf.viz.archive.data.SizeJob;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Abstract base class for Archive dialogs. Contains and manages information
 * needed for the archive and category selectors and populates the selection
 * table of display labels elements.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 30, 2013  1965     bgonzale  Initial creation
 * Jun 10, 2013  1966     rferrel   Change to allow Case Creation to extend.
 * Jul 24, 2013  2220     rferrel   Changes to queue size request for all data.
 * Aug 01, 2013  2221     rferrel   Changes for select configuration.
 * Aug 06, 2013  2222     rferrel   Changes to display all selected data.
 * Nov 14, 2013  2549     rferrel   Get category data moved off the UI thread.
 * Dec 11, 2013  2624     rferrel   No longer clear table prior to populating.
 * Apr 15, 2014  3034     lvenable  Added dispose checks in runAsync calls.
 * Apr 10, 2014  3023     rferrel   Added setTotalSelectedSize method.
 * Apr 23, 2014  3045     rferrel   Changes to prevent race condition while
 *                                  getting labels.
 * Aug 26, 2014  3553     rferrel   Force redisplay of table after getting all
 *                                  display labels.
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Jul 25, 2018  6748     randerso  Fixed to work with changes in
 *                                  CaveSWTDialog.shouldClose(). Code cleanup.
 * Dec 19, 2018  7677     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author bgonzale
 */

public abstract class AbstractArchiveDlg extends CaveSWTDialog
        implements IArchiveTotals, IUpdateListener, IModifyListener,
        IRetentionHour, ILoadDisplayDataListener {
    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    /** Table composite that holds the table controls. */
    private ArchiveTableComp tableComp;

    /** Archive configuration combo box. */
    private Combo archCfgCbo;

    /** Category combo box. */
    private Combo categoryCbo;

    /** Data manager. */
    protected ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

    /**
     * Boolean to indicate when DisplayData is created should its selection be
     * set based on the information in the configuration files.
     */
    protected boolean setSelect = false;

    /**
     * Must be set by sub-class prior to creating table.
     */
    protected ArchiveConstants.Type type;

    /**
     * Job that computes sizes of table row entries off the UI thread.
     */
    protected final SizeJob sizeJob = new SizeJob(this);

    /** Keeps track of when it is safe to clear the busy cursor. */
    protected final AtomicInteger busyCnt = new AtomicInteger(0);

    /** Performs save action button. */
    protected Button saveBtn;

    /** Optional button to toggle displaying all selected or category */
    protected Button showSelectedBtn;

    /** Flag set when user wants to close with unsaved modifications. */
    protected boolean closeFlag = false;

    /** Current select (case/retention) loaded into the dialog. */
    protected String selectName = ArchiveConstants.defaultSelectName;

    /** Which table is being displayed. */
    private boolean showingSelected = true;

    private String previousSelectedArchive = null;

    private String previousSelectedCategory = null;

    /** Job running to populate the currently selected archive/category. */
    private Job populateTableJob = null;

    /** Flag to indicate all labels for all tables are loaded. */
    protected volatile boolean haveAllLabels = false;

    /**
     * @param parentShell
     */
    public AbstractArchiveDlg(Shell parentShell) {
        super(parentShell);
    }

    /**
     * @param parentShell
     * @param swtStyle
     */
    public AbstractArchiveDlg(Shell parentShell, int swtStyle) {
        super(parentShell, swtStyle);
    }

    /**
     * @param parentShell
     * @param style
     * @param caveStyle
     */
    public AbstractArchiveDlg(Shell parentShell, int style, int caveStyle) {
        super(parentShell, style, caveStyle);
    }

    /**
     * @return the name of the currently selected archive in the dialog; null if
     *         none found
     */
    public String getSelectedArchiveName() {
        int archiveSelection = archCfgCbo.getSelectionIndex();
        String archiveName = archiveSelection == -1 ? null
                : archCfgCbo.getItem(archiveSelection);
        return archiveName;
    }

    /**
     * @return the name of the currently selected category in the dialog; null
     *         if none found
     */
    public String getSelectedCategoryName() {
        int categorySelection = categoryCbo.getSelectionIndex();
        String categoryName = categorySelection == -1 ? null
                : categoryCbo.getItem(categorySelection);
        return categoryName;
    }

    /**
     * @return the currently selected archive in the dialog; null if none found
     */
    public ArchiveConfig getSelectedArchive() {
        int archiveSelection = archCfgCbo.getSelectionIndex();
        String archiveName = archiveSelection == -1 ? null
                : archCfgCbo.getItem(archiveSelection);
        return ArchiveConfigManager.getInstance().getArchive(archiveName);
    }

    /**
     * @return the currently selected category in the dialog; null if none found
     */
    public CategoryConfig getSelectedCategory() {
        String archiveName = getSelectedArchiveName();
        ArchiveConfig archive = ArchiveConfigManager.getInstance()
                .getArchive(archiveName);
        if (archive != null) {
            String categoryName = getSelectedCategoryName();
            return archive.getCategory(categoryName);
        }
        return null;
    }

    /**
     * This method is called by the AbstractArchiveDlg to set the size text.
     *
     * @param prettyByteSize
     */
    protected abstract void setTotalSizeText(String sizeStringText);

    /**
     * This method is called by the AbstractArchiveDlg to set total Size.
     *
     * @param totalSize
     */
    protected abstract void setTotalSelectedItems(int totalSize);

    /**
     * This method is called by the AbstractArchiveDlg to get the start of the
     * time frame that bounds the data for the dialog.
     *
     * @return GMT Calendar
     */
    protected abstract Calendar getStart();

    /**
     * This method is called by the AbstractArchiveDlg to get the end of the
     * time frame that bounds the data for the dialog.
     *
     * @return GMT Calendar
     */
    protected abstract Calendar getEnd();

    /**
     * Create the Archive and Category combo controls.
     *
     * @param comp
     *            Composite to put the controls in.
     */
    protected Composite createComboControls(Composite comp) {
        Composite comboComp = new Composite(comp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        comboComp.setLayout(gl);
        comboComp.setLayoutData(gd);

        Label archCfgLbl = new Label(comboComp, SWT.NONE);
        archCfgLbl.setText("Archive Config:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        archCfgCbo = new Combo(comboComp,
                SWT.VERTICAL | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        archCfgCbo.setLayoutData(gd);
        archCfgCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                archiveComboSelection();
            }
        });

        Label catLbl = new Label(comboComp, SWT.NONE);
        catLbl.setText("Category:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        categoryCbo = new Combo(comboComp,
                SWT.VERTICAL | SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        categoryCbo.setLayoutData(gd);
        categoryCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                categoryComboSelection();
            }
        });

        return comboComp;
    }

    protected abstract void init(Shell shell);

    @Override
    protected final void initializeComponents(Shell shell) {
        ArchiveConfigManager.getInstance().reset();
        init(shell);
        initializeData();
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        shell.setMinimumSize(shell.getSize());
    }

    protected void initializeData() {
        setCursorBusy(true);
        populateComboBoxes();
        String titleName = initDisplayData();
        setSelectName(titleName);
        populateTableComp();
        setCursorBusy(false);

        addModifiedListener(this);
    }

    @Override
    public boolean shouldClose() {
        if (closeFlag || !isModified()) {
            return true;
        }

        return verifyClose();
    }

    /**
     * Change the select name and update the dialog's title. Assumes the
     * sub-classes places a hyphen at the end of the string when setting the
     * dialog's title.
     *
     * @param selectName
     */
    protected void setSelectName(String selectName) {
        this.selectName = selectName;
        StringBuilder sb = new StringBuilder(getText());
        sb.setLength(sb.indexOf("-") + 1);
        sb.append(" ").append(selectName);
        setText(sb.toString());
    }

    /**
     * Create the table control.
     */
    protected void createTable() {
        tableComp = new ArchiveTableComp(shell, type, this, sizeJob);
        // Indicate loading the table labels.
        tableComp
                .setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        sizeJob.addUpdateListener(this);
    }

    @Override
    protected void disposed() {
        sizeJob.removeUpdateListener(this);
        sizeJob.cancel();
        if (populateTableJob != null) {
            populateTableJob.cancel();
        }
    }

    /**
     * Update table based on current item selections in archive and category.
     */
    protected void updateTableComp() {
        populateTableComp();
    }

    /**
     * Initial population up of the combo boxes.
     */
    private void populateComboBoxes() {
        boolean doSelect = false;
        for (String archiveName : manager.getArchiveDataNamesList()) {
            archCfgCbo.add(archiveName);
            doSelect = true;
        }

        if (doSelect) {
            archCfgCbo.select(0);
            initCategoryCbo();
        }
    }

    /**
     * Method invoked when archive combo selection is changed.
     */
    protected void archiveComboSelection() {
        String selectedArchvieName = getSelectedArchiveName();
        if (!selectedArchvieName.equals(previousSelectedArchive)) {
            previousSelectedArchive = selectedArchvieName;
            populateCategoryCbo();
        }
    }

    /**
     * Populate the category combo based on the archive name and populate the
     * table.
     */
    private void populateCategoryCbo() {
        initCategoryCbo();
        categoryComboSelection();
    }

    private void initCategoryCbo() {
        String archiveName = getSelectedArchiveName();
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        categoryCbo.removeAll();
        for (String categoryName : manager.getCategoryNames(archiveName)) {
            categoryCbo.add(categoryName);
        }
        categoryCbo.select(0);
    }

    /**
     * Method invoked when the category combo selection is changed.
     */
    protected void categoryComboSelection() {
        String archiveName = getSelectedArchiveName();
        String categoryName = getSelectedCategoryName();
        if (!archiveName.equals(previousSelectedArchive)
                || !categoryName.equals(previousSelectedCategory)) {
            previousSelectedArchive = archiveName;
            previousSelectedCategory = categoryName;
            populateTableComp();
        }
    }

    /**
     * Set up all display data and queue getting sizes for any that are
     * selected.
     */
    protected String initDisplayData() {
        String displayArchive = getSelectedArchiveName();
        String displayCategory = getSelectedCategoryName();
        String selectedName = sizeJob.initData(type, null, displayArchive,
                displayCategory, this);
        sizeJob.resetTime(getStart(), getEnd());
        return selectedName;
    }

    /**
     * Delete a select configuration file.
     *
     * @param selectName
     */
    protected void deleteSelect(String selectName) {
        String fileName = ArchiveConstants.selectFileName(type, selectName);
        try {
            manager.deleteSelection(fileName);
        } catch (LocalizationException e) {
            statusHandler.debug("Unable to delete file: " + fileName, e);
            MessageDialog.openError(shell, "Case Error",
                    "Unable to delete file: " + fileName);
        }
    }

    /**
     * Load selected configuration file.
     *
     * @param selectName
     */
    protected boolean loadSelect(String selectName) {
        String message = sizeJob.loadSelect(selectName, type);
        if (message != null) {
            MessageDialog.openError(shell, "Unable to load Case", message);
            return false;
        }
        return true;
    }

    /**
     * Populate the table based on the currently selected archive, category and
     * adjust sizes on the display table.
     */
    protected void populateTableComp() {
        populateTableComp(false);
    }

    /**
     * @param forceUpdate
     */
    protected void populateTableComp(final boolean forceUpdate) {
        final String archiveName = getSelectedArchiveName();
        final String categoryName = getSelectedCategoryName();

        setCursorBusy(true);

        setShowingSelected(false);

        if (populateTableJob != null) {
            populateTableJob.cancel();
            setCursorBusy(false);
        }

        populateTableJob = new Job("populate category table") {
            private AtomicBoolean shutdown = new AtomicBoolean(false);

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                getCategoryTableData(archiveName, categoryName, shutdown,
                        forceUpdate);

                // Just populated the current table update cursor.
                if (!shutdown.get()) {
                    populateTableJob = null;
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            setCursorBusy(false);
                        }
                    });
                }
                return Status.OK_STATUS;
            }

            @Override
            protected void canceling() {
                shutdown.set(true);
            }

        };
        populateTableJob.schedule();
    }

    /**
     * This gets the desired categories data. Assumed called from non-UI thread
     * since it is possible getting the data may take time which would hang up
     * the UI thread.
     *
     * @param archiveName
     * @param categoryName
     * @param shutdown
     */
    private void getCategoryTableData(final String archiveName,
            final String categoryName, final AtomicBoolean shutdown,
            boolean forceUpdate) {

        if (!sizeJob.isCurrentDisplay(archiveName, categoryName)) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (!isDisposed()) {
                        tableComp.clearTable();
                    }
                }
            });
        }

        final List<DisplayData> displayDatas = sizeJob.changeDisplay(
                archiveName, categoryName, shutdown, forceUpdate);

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                // If the dialog has been disposed then return.
                if (isDisposed()) {
                    return;
                }

                if (displayDatas != null) {
                    tableComp.populateTable(archiveName, categoryName,
                            displayDatas);
                } else {
                    tableComp.refresh();
                }
            }
        });
    }

    /**
     * Set the shells cursor to the desire state.
     *
     * @param state
     */
    protected void setCursorBusy(boolean state) {
        if (!shell.isDisposed()) {
            if (state) {
                busyCnt.addAndGet(1);
                shell.setCursor(
                        shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            } else if (busyCnt.addAndGet(-1) == 0) {
                shell.setCursor(null);
            }
        }
    }

    @Override
    public void updateTotals(List<DisplayData> displayDatas) {
        long totalSize = 0;
        int totalSelected = 0;

        for (String archiveName : sizeJob.getArchiveNames()) {
            ArchiveInfo archiveInfo = sizeJob.get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    if (displayData.isSelected()) {
                        ++totalSelected;
                        if (totalSize != DisplayData.UNKNOWN_SIZE) {
                            long size = displayData.getSize();
                            if (size >= 0) {
                                totalSize += size;
                            } else {
                                totalSize = DisplayData.UNKNOWN_SIZE;
                            }
                        }
                    }
                }
            }
        }

        setTotalSelectedSize(totalSize);
        setTotalSelectedItems(totalSelected);
    }

    /**
     *
     * @param selectedTotalSize
     */
    protected void setTotalSelectedSize(long selectedTotalSize) {
        String sizeMsg = null;
        if (selectedTotalSize == DisplayData.UNKNOWN_SIZE) {
            sizeMsg = DisplayData.UNKNOWN_SIZE_LABEL;
        } else {
            sizeMsg = SizeUtil.prettyByteSize(selectedTotalSize);
        }
        setTotalSizeText(sizeMsg);
    }

    /**
     * Creates the showSelectedBtn for sub-classes.
     *
     * @param actionControlComp
     */
    protected void createShowingSelectedBtn(Composite actionControlComp) {
        showSelectedBtn = new Button(actionControlComp, SWT.PUSH);
        setShowingSelected(false);
        showSelectedBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handelShowSelectAll();
            }
        });
    }

    /**
     * Populate the table with the desired display.
     */
    protected void handelShowSelectAll() {
        if (showingSelected) {
            populateTableComp();
        } else {
            populateSelectAllTable();
        }
    }

    /**
     * Sets the state for the showing selected flag and updates the label for
     * the show selected button.
     *
     * @param state
     */
    private void setShowingSelected(boolean state) {
        if (showSelectedBtn != null && !showSelectedBtn.isDisposed()) {
            if (showingSelected != state) {
                showingSelected = state;
                if (showingSelected) {
                    showSelectedBtn.setText(" Category ");
                    showSelectedBtn
                            .setToolTipText("Change display to show category.");
                } else {
                    showSelectedBtn.setText(" Selected ");
                    showSelectedBtn.setToolTipText(
                            "Change display to show all case selections");
                }
            }
        } else {
            showingSelected = false;
        }
    }

    /**
     * Up date the table to display all selected data.
     */
    private void populateSelectAllTable() {
        setCursorBusy(true);

        try {
            setShowingSelected(true);
            List<DisplayData> selectedData = sizeJob.changeDisplayAll();

            if (selectedData != null) {
                tableComp.populateSelectAll(selectedData);
            }
        } finally {
            setCursorBusy(false);
        }
    }

    @Override
    public void update(final List<DisplayData> displayDatas) {
        final List<DisplayData> myDisplayDatas = new ArrayList<>(
                displayDatas.size());
        for (DisplayData data : displayDatas) {
            String label = data.getDisplayLabel();
            for (DisplayData displayData : sizeJob.get(data.getArchiveName())
                    .get(data.getCategoryName()).getDisplayDataList()) {
                if (label.equals(displayData.getDisplayLabel())) {
                    displayData.setSize(data.getSize());
                    myDisplayDatas.add(displayData);
                    break;
                }
            }
        }

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                // If the dialog has been disposed then return.
                if (isDisposed()) {
                    return;
                }

                tableComp.updateSize(myDisplayDatas);
                updateTotals(null);
            }
        });
    }

    /**
     * Reset modification flag.
     */
    protected void clearModified() {
        tableComp.clearModified();
    }

    /**
     * Add listener to inform when modification flag is set.
     *
     * @param iModifyListener
     */
    protected void addModifiedListener(IModifyListener iModifyListener) {
        tableComp.addModifiedListener(iModifyListener);
    }

    /**
     * Get the data information on all selected items; not just the currently
     * displayed table.
     *
     * @return selectedDatas
     */
    protected List<DisplayData> getSelectedData() {
        return sizeJob.getSelectAll();
    }

    /**
     * Remove modification listener.
     *
     * @param iModifyListener
     */
    protected void removeModifiedListener(IModifyListener iModifyListener) {
        tableComp.removeModifiedListener(iModifyListener);
    }

    /**
     * Reset all data to unknown size, and queue request for sizes.
     */
    protected void resetSizes() {
        sizeJob.recomputeSize();
        populateTableComp();
    }

    /**
     * Allows a sub-class to set the start and end times based on the off set.
     *
     * @param startTimeOffset
     */
    @Override
    public void setRetentionTimes(long startTimeOffset) {
        // do nothing override by sub-classes
    }

    /**
     * Indicate unsaved user changes.
     *
     * @return modified
     */
    protected boolean isModified() {
        return (saveBtn != null) && !saveBtn.isDisposed()
                && saveBtn.isEnabled();
    }

    /**
     * Save current selections of the select configuration file.
     *
     * @param selectName
     * @return true when save is successful
     */
    protected boolean saveSelection(String selectName) {
        Calendar start = getStart();
        Calendar end = getEnd();
        long startRetentionMS = 0L;
        if (start != null && end != null) {
            startRetentionMS = end.getTimeInMillis() - start.getTimeInMillis();
        }
        String errorMsg = sizeJob.saveSelect(selectName, type,
                startRetentionMS);
        if (errorMsg != null) {
            MessageDialog.openError(shell, type + " Selection Error", errorMsg);
        }

        return errorMsg == null;
    }

    /**
     * Perform updates once all the display data is loaded.
     */
    @Override
    public void loadedAllDisplayData() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                haveAllLabels = true;
                if (showingSelected) {
                    populateSelectAllTable();
                } else {
                    populateTableComp(true);
                }
                tableComp.setCursor(null);
            }
        });
    }

    /**
     * When unsaved modifications this asks the user to verify the close.
     *
     * @return true when okay to close.
     */
    protected boolean verifyClose() {
        boolean state = true;
        if (isModified()) {
            MessageBox box = new MessageBox(shell,
                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
            box.setText("Close " + type);
            box.setMessage("Unsaved changes.\nSelect OK to discard changes.");
            state = box.open() == SWT.OK;
        }
        closeFlag = state;
        return state;
    }
}
