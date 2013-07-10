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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.archive.data.ArchiveInfo;
import com.raytheon.uf.viz.archive.data.CategoryInfo;
import com.raytheon.uf.viz.archive.data.IArchiveTotals;
import com.raytheon.uf.viz.archive.data.IUpdateListener;
import com.raytheon.uf.viz.archive.data.SizeJob;
import com.raytheon.uf.viz.archive.data.SizeJobRequest;
import com.raytheon.uf.viz.archive.ui.ArchiveTableComp.TableType;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2013 1965       bgonzale     Initial creation
 * Jun 10, 2013 1966       rferrel      Change to allow Case Creation to extend.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public abstract class AbstractArchiveDlg extends CaveSWTDialog implements
        IArchiveTotals, IUpdateListener {

    /** Table composite that holds the table controls. */
    private ArchiveTableComp tableComp;

    /** Archive configuration combo box. */
    private Combo archCfgCbo;

    /** Category combo box. */
    private Combo categoryCbo;

    /** Information for populating the various table displays. */
    protected final Map<String, ArchiveInfo> archiveInfoMap = new HashMap<String, ArchiveInfo>();

    /**
     * Boolean to indicate when DisplayData is created should its selection be
     * set based on the information in the configuration files.
     */
    protected boolean setSelect = false;

    /**
     * Must be set by sub-class prior to creating table.
     */
    protected TableType tableType;

    /**
     * Job that computes sizes of table row entries off the UI thread.
     */
    protected final SizeJob sizeJob = new SizeJob();

    /** Keeps track of when it is safe to clear the busy cursor. */
    protected final AtomicInteger busyCnt = new AtomicInteger(0);

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

    public List<DisplayData> getAllSelected() {
        List<DisplayData> allSelected = new ArrayList<DisplayData>();
        for (String archiveName : archiveInfoMap.keySet()) {
            ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    if (displayData.isSelected()) {
                        allSelected.add(displayData);
                    }
                }
            }
        }
        return allSelected;
    }

    /**
     * @return the name of the currently selected archive in the dialog; null if
     *         none found
     */
    public String getSelectedArchiveName() {
        int archiveSelection = archCfgCbo.getSelectionIndex();
        String archiveName = archiveSelection == -1 ? null : archCfgCbo
                .getItem(archiveSelection);
        return archiveName;
    }

    /**
     * @return the name of the currently selected category in the dialog; null
     *         if none found
     */
    public String getSelectedCategoryName() {
        int categorySelection = categoryCbo.getSelectionIndex();
        String categoryName = categorySelection == -1 ? null : categoryCbo
                .getItem(categorySelection);
        return categoryName;
    }

    /**
     * @return the currently selected archive in the dialog; null if none found
     */
    public ArchiveConfig getSelectedArchive() {
        int archiveSelection = archCfgCbo.getSelectionIndex();
        String archiveName = archiveSelection == -1 ? null : archCfgCbo
                .getItem(archiveSelection);
        return ArchiveConfigManager.getInstance().getArchive(archiveName);
    }

    /**
     * @return the currently selected category in the dialog; null if none found
     */
    public CategoryConfig getSelectedCategory() {
        String archiveName = getSelectedArchiveName();
        ArchiveConfig archive = ArchiveConfigManager.getInstance().getArchive(
                archiveName);
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
        archCfgLbl.setText("Archive Config: ");

        gd = new GridData(200, SWT.DEFAULT);
        archCfgCbo = new Combo(comboComp, SWT.VERTICAL | SWT.DROP_DOWN
                | SWT.BORDER | SWT.READ_ONLY);
        archCfgCbo.setLayoutData(gd);
        archCfgCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                archiveComboSelection();
            }
        });

        Label catLbl = new Label(comboComp, SWT.NONE);
        catLbl.setText("Category: ");

        gd = new GridData(200, SWT.DEFAULT);
        categoryCbo = new Combo(comboComp, SWT.VERTICAL | SWT.DROP_DOWN
                | SWT.BORDER | SWT.READ_ONLY);
        categoryCbo.setLayoutData(gd);
        categoryCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                categoryComboSelection();
            }
        });

        return comboComp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        setCursorBusy(true);

        // Setup to display blank dialog with busy cursor while getting data.
        Job job = new Job("setup") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                ArchiveConfigManager.getInstance().reset();
                if (!shell.isDisposed()) {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            populateComboBoxes();
                            setCursorBusy(false);
                        }
                    });
                }
                initDisplayData();
                if (!shell.isDisposed()) {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            updateTableComp();
                        }
                    });
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Create the table control.
     */
    protected void createTable() {
        tableComp = new ArchiveTableComp(shell, tableType, this);
        sizeJob.addUpdateListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        sizeJob.removeUpdateListener(this);
        sizeJob.cancel();
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
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        boolean doSelect = false;
        for (String archiveName : manager.getArchiveDataNamesList()) {
            archCfgCbo.add(archiveName);
            doSelect = true;
        }

        if (doSelect) {
            archCfgCbo.select(0);
            archiveComboSelection();
        }
    }

    /**
     * Method invoked when archive combo selection is changed.
     */
    protected void archiveComboSelection() {
        populateCategoryCbo();
    }

    /**
     * Populate the category combo based on the archive name and populate the
     * table.
     * 
     * @param archiveName
     */
    private void populateCategoryCbo() {
        String archiveName = getSelectedArchiveName();
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        categoryCbo.removeAll();
        for (String categoryName : manager.getCategoryNames(archiveName)) {
            categoryCbo.add(categoryName);
        }
        categoryCbo.select(0);
        categoryComboSelection();
    }

    /**
     * Method invoked when the category combo selection is changed.
     */
    protected void categoryComboSelection() {
        populateTableComp();
    }

    /**
     * Set up all display data and queue getting sizes for any that are
     * selected.
     */
    private void initDisplayData() {
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        Calendar startCal = getStart();
        Calendar endCal = getEnd();
        String[] archiveNames = manager.getArchiveDataNamesList();
        for (String archiveName : archiveNames) {
            ArchiveInfo archiveInfo = new ArchiveInfo();
            archiveInfoMap.put(archiveName, archiveInfo);
            String[] categoryNames = manager.getCategoryNames(manager
                    .getArchive(archiveName));
            for (String categoryName : categoryNames) {
                List<DisplayData> displayDatas = manager.getDisplayData(
                        archiveName, categoryName, setSelect);
                CategoryInfo categoryInfo = new CategoryInfo(archiveName,
                        categoryName, displayDatas);
                archiveInfo.add(categoryInfo);
                for (DisplayData displayData : displayDatas) {
                    if (displayData.isSelected()) {
                        sizeJob.queue(new SizeJobRequest(displayData, startCal,
                                endCal));
                    }
                }
            }
        }
    }

    /**
     * Populate the table based on the currently selected archive, category and
     * adjust sizes on the display table.
     */
    protected void populateTableComp() {
        String archiveName = getSelectedArchiveName();
        String categoryName = getSelectedCategoryName();
        Calendar startCal = getStart();
        Calendar endCal = getEnd();

        setCursorBusy(true);

        try {
            sizeJob.clearQueue();

            ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);

            // Not yet populated by background job.
            if (archiveInfo == null) {
                return;
            }

            // Not yet populated by background job.
            CategoryInfo categoryInfo = archiveInfo.get(categoryName);
            if (categoryInfo == null) {
                return;
            }

            for (DisplayData displayData : categoryInfo.getDisplayDataList()) {
                sizeJob.queue(new SizeJobRequest(displayData, startCal, endCal));
            }
            sizeJob.requeueSelected(startCal, endCal);

            tableComp.populateTable(categoryInfo.getDisplayDataList());
        } finally {
            setCursorBusy(false);
        }
    }

    /**
     * Set the shells cursor to the desire state.
     * 
     * @param state
     */
    protected void setCursorBusy(boolean state) {
        if (state) {
            busyCnt.addAndGet(1);
            shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        } else if (busyCnt.addAndGet(-1) == 0) {
            shell.setCursor(null);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.data.IArchiveTotals#updateTotals(java.util
     * .List)
     */
    @Override
    public void updateTotals(List<DisplayData> displayDatas) {
        long totalSize = 0;
        int totalSelected = 0;

        for (String archiveName : archiveInfoMap.keySet()) {
            ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);
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

        String sizeMsg = null;
        if (totalSize == DisplayData.UNKNOWN_SIZE) {
            sizeMsg = DisplayData.UNKNOWN_SIZE_LABEL;
        } else {
            sizeMsg = SizeUtil.prettyByteSize(totalSize);
        }

        setTotalSizeText(sizeMsg);
        setTotalSelectedItems(totalSelected);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.data.IUpdateListener#update(java.util.List)
     */
    @Override
    public void update(List<SizeJobRequest> dirInfos) {
        final List<DisplayData> displayDatas = new ArrayList<DisplayData>(
                dirInfos.size());
        for (SizeJobRequest request : dirInfos) {
            displayDatas.add(request.getDisplayData());
        }

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                tableComp.updateSize(displayDatas);
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
     * Remove modification listener.
     * 
     * @param iModifyListener
     */
    protected void removeModifiedListener(IModifyListener iModifyListener) {
        tableComp.removeModifiedListener(iModifyListener);
    }
}
