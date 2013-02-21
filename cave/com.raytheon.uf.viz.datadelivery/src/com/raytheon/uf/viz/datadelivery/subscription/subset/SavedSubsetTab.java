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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * The Saved Subsets tab.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2012            mpduff     Initial creation.
 * Jun  4, 2012    645     jpiatt     Added tooltips.
 * Nov  1, 2012   1278     mpduff     Formatted to meet coding standard.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class SavedSubsetTab extends SubsetTab {

    /** Parent composite */
    private final Composite comp;

    /** Subset list */
    private List subsetList;

    /** ITabAction callback */
    private final ITabAction callback;

    /** Load button */
    private Button loadBtn;

    /** Delete button */
    private Button deleteBtn;

    /** File extension */
    String extension = ".xml";

    /**
     * Constructor.
     *
     * @param comp
     *            Composite holding these controls
     * @param callback
     *            The class for callbacks
     */
    public SavedSubsetTab(Composite comp, ITabAction callback) {
        this.comp = comp;
        this.callback = callback;

        init();
    }

    /**
     * Initialize components
     */
    private void init() {
        subsetList = new List(comp, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        GridData listData = new GridData(SWT.FILL, SWT.FILL, true, true);
        listData.widthHint = 275;
        subsetList.setLayoutData(listData);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, false);

        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Composite btnComp = new Composite(comp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        loadBtn = new Button(btnComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setLayoutData(btnData);
        loadBtn.setToolTipText("Highlight subset and click to load");
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleLoadSubset();
            }
        });

        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(btnData);
        saveBtn.setToolTipText("Highlight subset and click to save");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleSaveSubset();
            }
        });

        deleteBtn = new Button(btnComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(btnData);
        deleteBtn.setToolTipText("Highlight subset and click to delete");
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDeleteSubset();
            }
        });

        loadList();

        disableButtons();

    }

    private void loadList() {
        subsetList.removeAll();
        // Get the subset data
        for (LocalizationFile locFile : SubsetFileManager.getInstance()
                .getSubsets()) {
            String locFileName = locFile.getFile().getName();
            subsetList.add(SubsetXML.getBaseSubsetName(locFileName));
        }
    }

    private void handleDeleteSubset() {
        if (subsetList.getSelectionCount() > 0) {
            int response = DataDeliveryUtils.showMessage(comp.getShell(),
                    SWT.YES | SWT.NO, "Delete Subset?",
                    "Are you sure you want to delete this subset?");
            String subsetName = subsetList.getItem(subsetList
                    .getSelectionIndex());
            subsetName = subsetName + extension;
            if (response == SWT.YES) {
                SubsetFileManager.getInstance().deleteSubset(subsetName);
                loadList();
            }
        } else {
            DataDeliveryUtils.showMessage(comp.getShell(), SWT.ERROR,
                    "No Subset Selection", "Please select a Subset to Delete.");
        }

        disableButtons();

    }

    private void handleLoadSubset() {
        if (subsetList.getSelectionCount() > 0) {
            String subsetName = subsetList.getItem(subsetList
                    .getSelectionIndex());
            subsetName = subsetName + extension;
            callback.handleLoadSubset(subsetName);
        } else {
            DataDeliveryUtils.showMessage(comp.getShell(), SWT.ERROR,
                    "No Subset Selection", "Please select a Subset to load.");
        }
    }

    private void handleSaveSubset() {
        callback.handleSaveSubset();
        loadList();
    }

    /**
     * Enable buttons.
     *
     * @param name
     *            subset name
     */
    public void enableButtons(Text name) {

        if (name != null) {
            loadBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
        }
    }

    /**
     * Disable buttons.
     */
    public void disableButtons() {

        if (subsetList.getItemCount() == 0) {
            loadBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
        }
    }
}
