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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.viz.datadelivery.common.ui.AreaComp;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Spatial Subset Tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2012            mpduff     Initial creation.
 * Jun  4, 2012    645     jpiatt     Added tooltips.
 * Jun  8, 2012    684     jpiatt     Added clean & dirty checks.
 * Aug 10, 2012   1002     mpduff     Implementing dataset size estimation.
 * Sep 24, 2012   1209     djohnson   isValid() no longer needs @Override.
 * Oct  4, 2012   1245     jpiatt     Modify to reference util class & code clean up.
 * Oct 22, 2012    684     mpduff     Fix for saving pre-defined regions as a user region.
 * Oct 31, 2012   1278     mpduff     Allow dataset to be passed in on a load.
 * Nov 19, 2012   1289     bgonzale   Added delete button and controls.
 * Dec 07, 2012 1278       bgonzale   additional param to AreaComp ctor.
 * Dec 18, 2012 1439       mpduff     Redo subset name validation.
 * Dec 10, 2012   1259     bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SpatialSubsetTab extends SubsetTab implements IDataSize {

    /** Parent composite */
    private final Composite parentComp;

    /** Envelope describing full area where requests are possible */
    private ReferencedEnvelope fullEnvelope;

    /** Data Set Size flag */
    private boolean useDataSetSize = false;

    /** AreaComp object */
    private AreaComp areaComp;

    /** Saved Region Text field */
    private Text savedRegionTxt;

    /** Callback for data size changes */
    private final IDataSize callback;

    /** Flag for spatial dirty. */
    private boolean spatialDirty = false;

    /** Delete button for user defined areas. **/
    private Button deleteBtn = null;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Composite holding these controls
     * 
     * @param dataSet
     *            The DataSetMetaData object
     * @param callback
     *            IDataSize callback
     */
    public SpatialSubsetTab(Composite parentComp, DataSet dataSet,
            IDataSize callback) {
        this.parentComp = parentComp;
        if (dataSet != null && dataSet.getCoverage() != null) {
            this.fullEnvelope = dataSet.getCoverage().getEnvelope();
        }
        this.callback = callback;

        init();
    }

    /**
     * Initialize components
     */
    private void init() {
        final Button dataBoundaryChk = new Button(parentComp, SWT.CHECK);
        dataBoundaryChk.setText("Use Dataset Boundary");
        dataBoundaryChk.setSelection(false);
        dataBoundaryChk.setToolTipText("Use the Dataset coordinates");
        dataBoundaryChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean dataBoundaryChecked = ((Button) e.getSource())
                        .getSelection();

                useDataSetSize = dataBoundaryChecked;
                if (dataBoundaryChk.getSelection() == true) {
                    // Disable controls
                    areaComp.enableAllControls(false);
                } else {
                    // Enable controls
                    areaComp.enableAllControls(true);
                }

                handleBoundaryCheck(dataBoundaryChecked);

                setSpatialDirty(true);
            }
        });

        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        areaComp = new AreaComp(parentComp, "Subset by Area", this,
                fullEnvelope);
        areaComp.setLayout(gl);
        areaComp.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Group saveGrp = new Group(parentComp, SWT.NONE);
        saveGrp.setText(" Save Boundary as Region ");
        saveGrp.setLayout(gl);
        saveGrp.setLayoutData(gd);

        Label saveLbl = new Label(saveGrp, SWT.LEFT);
        saveLbl.setText("Saved Region Name: ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        savedRegionTxt = new Text(saveGrp, SWT.BORDER);
        savedRegionTxt.setLayoutData(gd);
        savedRegionTxt.setToolTipText("Enter Region name");

        Composite buttonComp = new Composite(saveGrp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false, 2, 1));

        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        gd = new GridData(75, SWT.DEFAULT);
        saveBtn.setLayoutData(gd);
        saveBtn.setToolTipText("Click to save boundary as named region");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleSave();
            }
        });
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        gd = new GridData(75, SWT.DEFAULT);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn
                .setToolTipText("Click to delete named region from 'My Regions'");
        deleteBtn.setEnabled(false);
        deleteBtn.setVisible(false);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDelete();
            }
        });
    }

    /**
     * Boundary Check action method.
     * 
     * @param selected
     *            true if the data set boundary checkbox is selected.
     */
    private void handleBoundaryCheck(boolean selected) {
        if (selected) {
            areaComp.updateBounds(fullEnvelope);
        }

        updateDataSize();
    }

    /**
     * Get text in Saved Region Name text box.
     * 
     * @return Region Name
     */
    public String getRegionSaveText() {
        String saveName = savedRegionTxt.getText().trim();
        return saveName;
    }

    /**
     * Reset Saved Region Name text box.
     * 
     */
    public void resetRegionSaveText() {
        savedRegionTxt.setText("");
    }

    /**
     * Handle the save region event
     */
    private void handleSave() {

        if (!DataDeliveryGUIUtils.hasText(savedRegionTxt)) {
            DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR,
                    "Name Required",
                    "Name required. A Region Name must be entered.");
            return;
        }

        String saveName = getRegionSaveText();
        if (DataDeliveryGUIUtils.INVALID_CHAR_PATTERN.matcher(saveName.trim())
                .find()) {
            DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR,
                    DataDeliveryGUIUtils.INVALID_CHARS_TITLE,
                    DataDeliveryGUIUtils.INVALID_CHARS_MESSAGE);
            return;
        }

        if (this.areaComp.isEnvelopeValid()) {
            ReferencedEnvelope envelope = this.areaComp.getEnvelope();

            AreaXML area = new AreaXML();
            area.setRegionName(saveName);

            area.setEnvelope(envelope);

            SubsetFileManager.getInstance().saveArea(area,
                    this.areaComp.getShell());

            // update the regionCombo
            areaComp.showMyRegions(saveName);
        }
    }

    /**
     * Handle delete region.
     */
    private void handleDelete() {
        if (DataDeliveryGUIUtils.hasText(savedRegionTxt)) {
            String regionName = getRegionSaveText();
            int response = DataDeliveryUtils.showMessage(parentComp.getShell(),
                    SWT.YES | SWT.NO, "Delete User Defined Region?",
                    "Are you sure you want to delete the user defined region, "
                            + regionName + "?");
            if (response == SWT.YES) {
                SubsetFileManager.getInstance().deleteArea(regionName);
                areaComp.handleRegionChange();
            }
        } else {
            DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR,
                    "Named Region has not been saved yet.",
                    "A Region cannot be deleted until it has been saved.");
            return;
        }
    }

    /**
     * Using the data set size?
     * 
     * @return true if Use Data Set Size is checked
     */
    public boolean useDataSetSize() {
        return useDataSetSize;
    }

    /**
     * Get the requested envelope.
     * 
     * 
     * @return The selected envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return areaComp.getEnvelope();
    }

    /**
     * Populate the tab.
     * 
     * @param area
     *            The AreaXML object
     */
    public void populate(AreaXML area) {

        areaComp.updateBounds(area.getEnvelope());
        areaComp.setCustom();
        String regionName = area.getRegionName();

        // set the region name in the combo box
        if (regionName != null) {
            int i = 0;
            areaComp.setRegion(regionName);

            for (String region : areaComp.predefinedRegions) {
                if (region.equals(regionName)) {
                    areaComp.selectCombo.select(0);
                    areaComp.regionCombo.setItems(areaComp.predefinedRegions);
                    areaComp.regionCombo.select(i);
                }
                i++;
            }

        } else {
            areaComp.updateRegionControls();
        }

    }

    /**
     * Get the save details from this tab
     * 
     * @return AreaXML object populated with the save details
     */
    public AreaXML getSaveInfo() {
        AreaXML area = new AreaXML();
        ReferencedEnvelope envelope = areaComp.getEnvelope();
        if (envelope != null) {
            area.setEnvelope(envelope);
        }

        String name = areaComp.getRegionName();

        if (areaComp.selectCombo.getEnabled()) {
            area.setRegionName(name);
        }
        return area;
    }

    /**
     * Determine if the tab selections are valid.
     * 
     * @return true if tab is valid
     */
    public boolean isValid() {
        ReferencedEnvelope envelope = areaComp.getEnvelope();
        if (envelope == null) {
            return false;
        }
        return true;
    }

    @Override
    public void updateDataSize() {
        if (callback != null) {
            callback.updateDataSize();
        }
        if (areaComp != null) {
            if (areaComp.isRegionChanged()
                    && !areaComp.doAreaEditBoxesHaveFocus()) {
                if (areaComp.isPredefinedRegion()
                        && areaComp.isUserDefinedRegion()) {
                    savedRegionTxt.setText(areaComp.getRegionName());
                } else {
                    resetRegionSaveText();
                }
            }
            boolean isUserDefinedRegion = areaComp.isUserDefinedRegion();
            deleteBtn.setEnabled(isUserDefinedRegion);
            deleteBtn.setVisible(isUserDefinedRegion);
        }
    }

    /**
     * Get the boolean if any spatial info has changed.
     * 
     * @param spatialDirty
     *            spatial info has changed
     */
    public void setSpatialDirty(boolean spatialDirty) {
        this.spatialDirty = spatialDirty;
    }

    /**
     * Get the boolean if any spatial info has changed.
     * 
     * @return dateCycleDirty true if spatial info has changed
     */
    public boolean isSpatialDirty() {
        return spatialDirty;
    }

    /**
     * Set the DataSet.
     * 
     * @param dataSet
     */
    public void setDataSet(DataSet dataSet) {
        if (dataSet != null && dataSet.getCoverage() != null) {
            this.fullEnvelope = dataSet.getCoverage().getEnvelope();
        } else {
            this.fullEnvelope = null;
        }
        this.areaComp.setFullEnvelope(this.fullEnvelope);
    }
}
