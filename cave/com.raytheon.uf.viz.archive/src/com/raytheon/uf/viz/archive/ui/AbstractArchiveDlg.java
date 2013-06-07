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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.archive.data.ArchiveInfo;
import com.raytheon.uf.viz.archive.data.CategoryInfo;
import com.raytheon.uf.viz.archive.data.DirInfo;
import com.raytheon.uf.viz.archive.data.IArchiveTotals;
import com.raytheon.uf.viz.archive.ui.ArchiveTableComp.TableType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public abstract class AbstractArchiveDlg extends CaveSWTDialog implements
        IArchiveTotals {

    private static final String UNKNOWN_SIZE_TEXT = "????MB";

    /** Table composite that holds the table controls. */
    private ArchiveTableComp tableComp;

    /** Archive config combo box. */
    private Combo archCfgCbo;

    /** Category combo box. */
    private Combo categoryCbo;

    /** Information for populating the various table displays. */
    private final Map<String, ArchiveInfo> archiveInfoMap = new HashMap<String, ArchiveInfo>();

    private Cursor busyCursor;

    /**
     * @param parentShell
     */
    public AbstractArchiveDlg(Shell parentShell) {
        super(parentShell);
        setupCursor();
    }

    /**
     * @param parentShell
     * @param swtStyle
     */
    public AbstractArchiveDlg(Shell parentShell, int swtStyle) {
        super(parentShell, swtStyle);
        setupCursor();
    }

    /**
     * @param parentShell
     * @param style
     * @param caveStyle
     */
    public AbstractArchiveDlg(Shell parentShell, int style, int caveStyle) {
        super(parentShell, style, caveStyle);
        setupCursor();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.archive.data.IArchiveTotals#getTotalSelectedItems()
     */
    // @Override
    public int getTotalSelectedItems() {
        int totalSelected = 0;
        for (ArchiveInfo archiveInfo : archiveInfoMap.values()) {
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayInfo : categoryInfo
                        .getDisplayInfoList()) {
                    if (displayInfo.isSelected()) {
                        ++totalSelected;
                    }
                }
            }
        }
        updateTotalSizeLabel();
        return totalSelected;
    }

    public List<DisplayData> getAllSelected() {
        List<DisplayData> allSelected = new ArrayList<ArchiveConfigManager.DisplayData>();
        for (String archiveName : archiveInfoMap.keySet()) {
            ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayInfoList()) {
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

    protected void updateTotalSizeLabel() {
        long totalSize = 0;
        for (String archiveName : archiveInfoMap.keySet()) {
            ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayInfoList()) {
                    if (displayData.isSelected()) {
                        long size = displayData.getSize();
                        if (size < 0) {
                            // Size still being computed.
                            setTotalSizeText(UNKNOWN_SIZE_TEXT);
                            return;
                        } else {
                            totalSize += size;
                        }
                    }
                }
            }
        }
        setTotalSizeText(SizeUtil.prettyByteSize(totalSize));
    }

    /**
     * This method is called by the AbstractArchiveDlg to set the size text.
     * 
     * @param prettyByteSize
     */
    protected abstract void setTotalSizeText(String sizeStringText);

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
     * This method is called by the AbstractArchiveDlg when the combo boxes are
     * updated.
     */
    protected abstract void selectionsUpdated();

    /**
     * Create the Archive and Category combo controls.
     * 
     * @param comp
     *            Composite to put the controls in.
     */
    protected void createComboControls(Composite comp) {
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
                String archiveName = archCfgCbo.getItem(archCfgCbo
                        .getSelectionIndex());
                populateCategoryCbo(archiveName);
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
                String archiveName = archCfgCbo.getItem(archCfgCbo
                        .getSelectionIndex());
                String categoryName = categoryCbo.getItem(categoryCbo
                        .getSelectionIndex());
                populateTableComp(archiveName, categoryName);
            }
        });

        ArchiveConfigManager.getInstance().reset();

        createTable();
        populateComboBoxes();
        updateTableComp();
    }

    /**
     * Create the table control.
     */
    protected void createTable() {
        tableComp = new ArchiveTableComp(shell, TableType.Case, this);
    }

    /**
     * Update table based on current item selections in archive and category.
     */
    protected void updateTableComp() {
        populateTableComp(getSelectedArchiveName(), getSelectedCategoryName());
    }

    /**
     * Init busyCursor. On close, make sure that cursor reverts back to unbusy
     * state if it isn't already.
     */
    private void setupCursor() {
        busyCursor = getParent().getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        this.setCloseCallback(new ICloseCallback() {
            @Override
            public void dialogClosed(Object returnValue) {
                setCursorBusy(false);
            }
        });
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
            String archiveName = archCfgCbo.getItem(0);
            populateCategoryCbo(archiveName);
        }
    }

    /**
     * Populate the category combo based on the archive name and populate the
     * table.
     * 
     * @param archiveName
     */
    private void populateCategoryCbo(String archiveName) {
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        categoryCbo.removeAll();
        for (String categoryName : manager.getCategoryNames(archiveName)) {
            categoryCbo.add(categoryName);
        }
        categoryCbo.select(0);
        String categoryName = categoryCbo.getItem(0);
        populateTableComp(archiveName, categoryName);
    }

    private void populateTableComp(String archiveName, String categoryName) {

        setCursorBusy(true);
        DirInfo.clearQueue();
        ArchiveInfo archiveInfo = archiveInfoMap.get(archiveName);
        if (archiveInfo == null) {
            archiveInfo = new ArchiveInfo();
            archiveInfoMap.put(archiveName, archiveInfo);
        }

        CategoryInfo categoryInfo = archiveInfo.get(categoryName);
        if (categoryInfo == null) {
            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
            List<DisplayData> displayInfos = manager.getDisplayInfo(
                    archiveName, categoryName);
            categoryInfo = new CategoryInfo(archiveName, categoryName,
                    displayInfos);
            archiveInfo.add(categoryInfo);
        }

        for (DisplayData displayInfo : categoryInfo.getDisplayInfoList()) {
            new DirInfo(displayInfo, getStart(), getEnd());
        }

        tableComp.populateTable(categoryInfo.getDisplayInfoList());
        selectionsUpdated();
        setCursorBusy(false);
    }

    private void setCursorBusy(boolean state) {
        Cursor cursor = null;
        if (state) {
            cursor = busyCursor;
        }
        shell.setCursor(cursor);
    }

}
