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
package com.raytheon.uf.viz.datadelivery.filter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.datadelivery.browser.FilterComp;
import com.raytheon.uf.viz.datadelivery.browser.FilterConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.ExpandBarControls;
import com.raytheon.uf.viz.datadelivery.common.ui.ExpandBarControlsConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction;
import com.raytheon.uf.viz.datadelivery.filter.FilterImages.ExpandItemState;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterSettingsXML;
import com.raytheon.uf.viz.datadelivery.filter.config.xml.FilterTypeXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.FilterDefinitionManager;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.DataTypeFilterElementXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.DataTypeFilterXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.FilterElementsXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.FilterXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.SettingsXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * 
 * The main class that contains the filter expand bar and the associated
 * controls.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2012            lvenable     Initial creation
 * Jun 21, 2012    736     djohnson     Add setter for coordinates.
 * Dec 12, 2012   1391     bgonzale     Added a job for the dataset query.
 * Dec 10, 2012   1259     bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Dec 18, 2012   1436     bgonzale     When creating the filter dialogs, use the loaded
 *                                      configuration when populating the filters.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FilterExpandBar extends Composite implements IFilterUpdate, IExpandControlAction {

    /**
     * Filter expand bar.
     */
    private ExpandBar expandBar;

    /**
     * Filter Images for the filter items and the expand bar controls.
     */
    private FilterImages filterImgs;

    // private String dataType;

    /**
     * Dialog that will enable or disable a filter.
     */
    private EnableFilterDlg enableFilterDlg = null;

    /**
     * Controls for the expand bar.
     */
    // private ExpandBarControls expandBarControls;

    private DataTypeFilterXML dataTypeFilterXml;

    private FilterXML filterXml;

    private FilterSettingsXML filterSettingsXml;

    private String[] dataTypes;

    /** List of common filters */
    private ArrayList<String> filterList = null;

    /**
     * envelope for filtering.
     */
    private ReferencedEnvelope envelope;

    /**
     * Constructor. 
     * 
     * @param parent 
     *          The parent composite
     */
    public FilterExpandBar(Composite parent) {
        super(parent, SWT.NONE);

        init();
    }

    /**
     * Initialize the filter group.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 2;
        gl.marginHeight = 0;
        gl.verticalSpacing = 2;
        this.setLayout(gl);
        this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        filterImgs = new FilterImages(this);

        createFilterControls();

        expandBar = new ExpandBar(this, SWT.V_SCROLL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 250;
        gd.widthHint = 600;
        expandBar.setLayoutData(gd);

        FilterDefinitionManager filterMan = FilterDefinitionManager.getInstance();
        dataTypeFilterXml = filterMan.getDataTypeXml();
        filterXml = filterMan.getFilterXml();

        createExpandItems();
    }

    /**
     * Create the filter controls.
     */
    private void createFilterControls() {
        Composite composite = new Composite(this, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        ExpandBarControlsConfig expBarConfig = new ExpandBarControlsConfig();
        expBarConfig.setCollapseAll(true);
        expBarConfig.setExpandAll(true);
        expBarConfig.setDisable(true);
        expBarConfig.setClearAll(true);

        new ExpandBarControls(composite, expBarConfig, this, filterImgs);
        addSeparator(composite);
    }

    private void createExpandItems() {
        if (dataTypes != null && dataTypes.length > 0) {
            final Shell jobParent = this.getShell();
            final Job job = new Job("Dataset Discovery...") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    MetaDataManager dataManager = MetaDataManager.getInstance();

                    DataDeliveryGUIUtils.markBusyInUIThread(jobParent);
                    dataManager.rereadMetaData();
                    dataManager.readMetaData(dataTypes[0]);
                    return Status.OK_STATUS;
                }
            };

            job.addJobChangeListener(new JobChangeAdapter() {
                @Override
                public void done(IJobChangeEvent event) {
                    DataTypeFilterElementXML dtfe;
                    HashMap<String, ArrayList<String>> dataFilterMap = new HashMap<String, ArrayList<String>>();

                    // Get filters for each data type
                    for (int i = 0; i < dataTypes.length; i++) {
                        String dataType = dataTypes[i];
                        dtfe = dataTypeFilterXml.getFilterData(dataType);
                        ArrayList<String> filterIDList = dtfe.getFilterIDList();
                        dataFilterMap.put(dataType, filterIDList);
                    }

                    // Now have a list of available filter types
                    // Need to find the common filters
                    FilterDefinitionManager filterMan = FilterDefinitionManager
                            .getInstance();
                    filterList = filterMan.findCommon(dataFilterMap);

                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            // Now we have a list of common filters, lets build
                            // them
                            for (String filter : filterList) {
                                final FilterElementsXML fex = filterXml
                                        .getFilter(filter);
                                String clazz = fex.getClazz();

                                // TODO use reflection here to instantiate the
                                // class
                                if (clazz.equals("FilterComp")) {
                                    createFilter(fex);
                                }
                            }
                            notifyListeners(SWT.SetData, new Event());
                            DataDeliveryGUIUtils
                                    .markNotBusyInUIThread(jobParent);
                        }
                    });
                }
            });

            job.schedule();
        }
    }

    private void createFilter(FilterElementsXML data) {
        // Create a provider filter
        ExpandItem expItem = new ExpandItem(expandBar, SWT.NONE);
        int idx = expandBar.indexOf(expItem);

        String displayName = data.getDisplayName();
        String filterID = data.getId();
//        String clazz = data.getClazz();
        ArrayList<SettingsXML> settingsList = data.getSettingsList();

        DualListConfig dualConfig = new DualListConfig();
        FilterConfig filterConfig = new FilterConfig();

        // Process the settings
        for (SettingsXML setting : settingsList) {
            if (setting.getName().equalsIgnoreCase("availableText")) {
                dualConfig.setAvailableListLabel(setting.getValue());
            }
            else if (setting.getName().equalsIgnoreCase("selectedText")) {
                dualConfig.setSelectedListLabel(setting.getValue());
            }
            else if (setting.getName().equalsIgnoreCase("showUpDownBtns")) {
                dualConfig.setShowUpDownBtns(getBoolean(setting.getValue()));
            }
            else if (setting.getName().equalsIgnoreCase("listWidth")) {
                dualConfig.setShowUpDownBtns(getBoolean(setting.getValue()));
            }
            else if (setting.getName().equalsIgnoreCase("listHeight")) {
                dualConfig.setShowUpDownBtns(getBoolean(setting.getValue()));
            }
            else if (setting.getName().equalsIgnoreCase("showRegex")) {
                filterConfig.setRegExVisible(getBoolean(setting.getValue()));
            }
            else if (setting.getName().equalsIgnoreCase("showMatch")) {
                filterConfig.setMatchControlVisible(getBoolean(setting.getValue()));
            }
            else if (setting.getName().equalsIgnoreCase("showDualList")) {
                filterConfig.setDualListVisible(getBoolean(setting.getValue()));
            }
        }

        MetaDataManager dataManager = MetaDataManager.getInstance();
        dataManager.setArea(envelope);

        /*
         * TODO : this needs to be reworked as this only has 4 display
         * (filters). This should be configurable.
         */
        final String DATA_PROVIDER = "Data Provider";
        final String DATA_SET = "Data Set";
        final String PARAMETER = "Parameter";
        final String LEVEL = "Level";
        if (displayName.equals(DATA_PROVIDER)) {
            dualConfig.setFullList(new ArrayList<String>(dataManager
                    .getAvailableDataProviders()));
            dualConfig.setSelectedList(getFilterSettingsValues(DATA_PROVIDER));
        } else if (displayName.equals(DATA_SET)) {
            dualConfig.setFullList(new ArrayList<String>(dataManager
                    .getAvailableDataSets()));
            dualConfig.setSelectedList(getFilterSettingsValues(DATA_SET));
        } else if (displayName.equals(PARAMETER)) {
            dualConfig.setFullList(new ArrayList<String>(dataManager
                    .getAvailableParameters()));
            dualConfig.setSelectedList(getFilterSettingsValues(PARAMETER));
        } else if (displayName.equals(LEVEL)) {
            dualConfig.setFullList(new ArrayList<String>(dataManager
                    .getAvailableLevels()));
            dualConfig.setSelectedList(getFilterSettingsValues(LEVEL));
        }

        filterConfig.setDualConfig(dualConfig);
        filterConfig.setDualListVisible(true);
        filterConfig.setFilterID(filterID);

        expItem.setText(displayName);
        FilterComp filterComp = new FilterComp(expandBar, SWT.NONE, this, filterConfig, idx);

        expItem.setHeight(filterComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        expItem.setImage(filterImgs.getExpandItemImage(ExpandItemState.NoEntries));
        expItem.setControl(filterComp);
    }

    /**
     * Get the list of defined filterSettings for a filter type defined in
     * filterSettingsXml.
     * 
     * @param filterType
     * @return List of filter values.
     */
    private List<String> getFilterSettingsValues(String filterType) {
        if (filterSettingsXml != null) {
            for (FilterTypeXML filter : filterSettingsXml.getFilterTypeList()) {
                if (filter.getFilterType().equals(filterType)) {
                    return filter.getValues();
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * Add a separator line to the display.
     * 
     * @param parentComp
     *            Parent component.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout)parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Dispose the expand items and the controls in the expand items.
     */
    private void disposeExpandItemsAndControls() {
        for (ExpandItem ei : expandBar.getItems()) {
            if (ei != null) {
                ei.getControl().dispose();
                ei.dispose();
            }
        }
    }

    /**
     * Display the enable filter dialog.
     */
    private void displayEnableFilterDialog() {

        if (expandBar.getItemCount() == 0) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Warning");
            mb.setMessage("No filters are available to enable/disable.");
            mb.open();
            return;
        }

        if (enableFilterDlg == null || enableFilterDlg.isDisposed()) {
            enableFilterDlg = new EnableFilterDlg(this.getShell(), getFilterNames(), getEnabledFilters());
            enableFilterDlg.open();

            if (enableFilterDlg.getReturnValue() != null && (Boolean)enableFilterDlg.getReturnValue() == true) {
                this.enableFilters(enableFilterDlg.getSelectedIndexes());
            }
        }
        else {
            enableFilterDlg.bringToTop();
        }
    }

    /**
     * Called when a filter has been updated.
     */
    @Override
    public void filterUpdate(int index, ExpandItemState state) {
        // Get the correct item
        ExpandItem expItem = expandBar.getItem(index);

        expItem.setImage(filterImgs.getExpandItemImage(state));
        //
        // Control control = expItem.getControl();
        //
        // // Check to see if other filters need to be updated
        // if (control instanceof FilterComp) {
        // FilterComp filterComp = (FilterComp) control;
        // String[] selectedItems = filterComp.getSelectedListItems();
        // FilterConfig config = filterComp.getConfig();
        // String filterID = config.getFilterID();
        //
        // MetaDataManager dataMan = MetaDataManager.getInstance();
        // dataMan.setFilterData
        //
        // }
    }

    /**
     * Get a list of filter names.
     * 
     * @return A list of filter names.
     */
    public ArrayList<String> getFilterNames() {
        ArrayList<String> names = new ArrayList<String>();

        for (ExpandItem ei : expandBar.getItems()) {
            names.add(ei.getText());
        }

        return names;
    }

    /**
     * Any filters in the expandBar?
     * 
     * @return true if one or more filters exist in the expand bar
     */
    public boolean hasFilters() {
        if (expandBar.getItemCount() > 0) {
            return true;
        }

        return false;
    }

    /**
     * Get a list of enabled filters.
     * 
     * @return A list of enabled filters.
     */
    public ArrayList<Integer> getEnabledFilters() {
        ArrayList<Integer> enabledIndexes = new ArrayList<Integer>();

        for (int i = 0; i < expandBar.getItems().length; i++) {
            AbstractFilterComp afc = (AbstractFilterComp)(expandBar.getItem(i).getControl());
            if (afc.isEnabled()) {
                enabledIndexes.add(i);
            }
        }

        return enabledIndexes;
    }

    /**
     * Enable the list of specified filters.
     * 
     * @param indexes
     *            Array of indexes specifying the filters to be enabled.
     */
    public void enableFilters(ArrayList<Integer> indexes) {
        for (ExpandItem ei : expandBar.getItems()) {
            ((AbstractFilterComp)ei.getControl()).setEnabled(false);
        }

        for (int idx : indexes) {
            ((AbstractFilterComp)(expandBar.getItem(idx).getControl())).setEnabled(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#
     * collapseAction()
     */
    @Override
    public void collapseAction() {
        ExpandItem[] items = expandBar.getItems();

        for (ExpandItem ei : items) {
            ei.setExpanded(false);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#expandAction
     * ()
     */
    @Override
    public void expandAction() {
        ExpandItem[] items = expandBar.getItems();

        for (ExpandItem ei : items) {
            ei.setExpanded(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#
     * expandSelectedAction()
     */
    @Override
    public void expandSelectedAction() {
        // NOT USED...
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#disableAction
     * ()
     */
    @Override
    public void disableAction() {
        displayEnableFilterDialog();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#
     * clearAllAction()
     */
    @Override
    public void clearAllAction() {

        MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_QUESTION | SWT.YES | SWT.NO);
        mb.setText("Clear All Filters");
        mb.setMessage("You are about to clear all of your filter settings.  This\n"
                + "cannot be undone.\n\nDo you wish to continue?");
        int result = mb.open();

        if (result == SWT.NO) {
            return;
        }

        for (ExpandItem ei : expandBar.getItems()) {
            if (ei.getControl() instanceof FilterComp) {
                ((FilterComp)ei.getControl()).resetControls();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IExpandControlAction#previewAction
     * ()
     */
    @Override
    public void previewAction() {
        // NOT USED...
    }

    private boolean getBoolean(String bool) {
        return Boolean.parseBoolean(bool);
    }

    /**
     * Update the filters.
     * 
     * @param dataTypes
     *            Array of data types
     * @param envelope
     *            envelope
     */
    public void updateFilters(String[] dataTypes, ReferencedEnvelope envelope) {
        this.dataTypes = dataTypes;
        setEnvelope(envelope);
        disposeExpandItemsAndControls();
        createExpandItems();
    }

    private void updateFilterSettings() {
        ArrayList<FilterTypeXML> filterTypeList = filterSettingsXml.getFilterTypeList();
        if (filterTypeList != null && filterTypeList.size() > 0) {
            for (FilterTypeXML ftx : filterTypeList) {
                if (ftx.getFilterType().equals("Data Type")) {
                    dataTypes = new String[ftx.getValues().size()];
                    int i = 0;
                    for (String dataType : ftx.getValues()) {
                        dataTypes[i] = dataType;
                        i++;
                    }
                    break;
                }
            }
            
            if (filterList != null) {
                // createExpandItems();
                for (FilterTypeXML filterTypeXml : filterTypeList) {
                    String filterType = filterTypeXml.getFilterType();
    
                    if (filterList.contains(filterType)) {
                        for (String filter : filterList) {
                            if (filter.equals(filterType)) {
                                ExpandItem[] expandItems = expandBar.getItems();
                                for (ExpandItem ei : expandItems) {
                                    if (ei.getText().equals(filter)) {
                                        Control control = ei.getControl();
                                        if (control instanceof FilterComp) {
                                            FilterComp fc = (FilterComp)control;
                                            String[] items = filterTypeXml.getValues().toArray(
                                                    new String[filterTypeXml.getValues().size()]);
                                            if (items != null && items.length > 0) {
                                                fc.selectItems(items);
                                                ei.setImage(filterImgs.getExpandItemImage(ExpandItemState.Entries));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * @param filterSettingsXml 
     */
    public void populateFilterSettingsXml(FilterSettingsXML filterSettingsXml) {
        ExpandItem[] items = expandBar.getItems();
        for (ExpandItem item : items) {
            Control control = item.getControl();
            if (control instanceof FilterComp) {
                FilterComp fc = (FilterComp)control;
                String[] selectedItems = fc.getSelectedListItems();
                ArrayList<String> values = new ArrayList<String>();
                for (String selectedItem : selectedItems) {
                    values.add(selectedItem);
                }
                String type = item.getText();
                FilterTypeXML ftx = new FilterTypeXML();
                ftx.setFilterType(type);
                ftx.setValues(values);

                filterSettingsXml.addFilterType(ftx);
            }
        }
        // return filterSettingsXml;
    }

    /**
     * @param filterSettingsXml
     *            the filterSettingsXml to set
     */
    public void setFilterSettingsXml(FilterSettingsXML filterSettingsXml) {
        this.filterSettingsXml = filterSettingsXml;
        updateFilterSettings();
    }

    /**
     * Check for changes in the filter.
     * 
     * @return true if changes have been made;
     */
    public boolean isDirty() {
        ExpandItem[] items = expandBar.getItems();
        for (ExpandItem item : items) {
            FilterComp comp = (FilterComp)item.getControl();
            if (comp != null) {
                if (comp.isDirty()) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Set the filters clean.
     */
    public void setClean() {
        ExpandItem[] items = expandBar.getItems();
        for (ExpandItem item : items) {
            FilterComp comp = (FilterComp)item.getControl();
            if (comp != null) {
                comp.setDirty(false);
            }
        }

    }

    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }

}
