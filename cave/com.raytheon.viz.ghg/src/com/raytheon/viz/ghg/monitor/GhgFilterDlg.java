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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsFilterEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ghg.monitor.filter.GhgFilterEngine;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the GHG Filter Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 17Jun2008    1157       MW Fegan    Hooked in configuration.
 * 28 Nov 2012  1353       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgFilterDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgFilterDlg.class);

    /**
     * Composite containing the list controls.
     */
    private Composite listComp;

    /**
     * Show current hazards check box.
     */
    private Button showCurrentHazardsChk;

    /**
     * Show advanced columns check box.
     */
    private Button showAdvancedColumnsChk;

    /**
     * Array of filters.
     */
    private GhgConfigData.AlertsFilterEnum[] filterArray = {
            GhgConfigData.AlertsFilterEnum.Action,
            GhgConfigData.AlertsFilterEnum.PhenSig,
            GhgConfigData.AlertsFilterEnum.Pil,
            GhgConfigData.AlertsFilterEnum.WFO,
            GhgConfigData.AlertsFilterEnum.GeoId,
            GhgConfigData.AlertsFilterEnum.ETN,
            GhgConfigData.AlertsFilterEnum.Seg };

    public static Map<GhgConfigData.AlertsFilterEnum, String[]> filterToEnumMap = new HashMap<GhgConfigData.AlertsFilterEnum, String[]>() {

        private static final long serialVersionUID = 6183513849706287870L;
        {
            put(GhgConfigData.AlertsFilterEnum.Action,
                    GhgConfigData.vtecActionNames);
            put(GhgConfigData.AlertsFilterEnum.PhenSig, GhgConfigData
                    .getInstance().getPhenSigCodes());
            put(GhgConfigData.AlertsFilterEnum.Pil, GhgConfigData.vtecPILNames);
        }
    };

    /**
     * Array of filter group containers that contain the list controls.
     */
    private List<GhgFilterListGroup> listGroupArray;

    /**
     * Combine Geo ID check box.
     */
    private Button combineGeoIdChk;

    /**
     * Combine Segments check box.
     */
    private Button combineSegmentsChk;

    /**
     * Combine Purge Times check box.
     */
    private Button combinePurgeTimesChk;

    /**
     * Combine Actions check box.
     */
    private Button combineActionsChk;

    /**
     * Include Alerts check box.
     */
    private Button incAlertsChk;

    /**
     * Include Map Selections check box.
     */
    private Button incMapSelectionsChk;

    /**
     * Include Past Events check box.
     */
    private Button incPastEventsChk;

    private Button incOrgPilEvents;

    private GhgDataFilter filter = null;

    /**
     * List of WFOs in the area.
     */
    private List<String> wfoList = new ArrayList<String>();

    /**
     * List of geo ids.
     */
    private List<String> geoIdList = new ArrayList<String>();

    /**
     * Array of event tracking numbers.
     */
    private String[] etnArr = null;

    /**
     * Array of segment values.
     */
    private String[] segArr = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public GhgFilterDlg(Shell parent, GhgDataFilter filter) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GHG Monitor Filter Dialog");

        this.filter = filter;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createMainFilterControls();

        createGroupControls();

        createDismissButton();

        setInitialDialogState();
    }

    /**
     * Updates the configuration object prior to dialog close.
     */
    private void updateFilterConfig() {
        /* update the state of the current hazards only check-box */
        filter.currentHazards = showCurrentHazardsChk.getSelection();

        /* update the state of the filter boxes */
        for (int i = 0; i < filterArray.length; i++) {
            GhgConfigData.AlertsFilterEnum type = filterArray[i];
            GhgFilterListGroup group = listGroupArray.get(i);
            filter.setFilterByType(type, group.getSelections());
        }

        /* update the state of the Record Consolidation check-boxes */
        filter.combineGeoId = combineGeoIdChk.getSelection();
        filter.combineSegments = combineSegmentsChk.getSelection();
        filter.combinePurgeTimes = combinePurgeTimesChk.getSelection();
        filter.combineActions = combineActionsChk.getSelection();

        /* update the state of the Filter Override check-boxes */
        filter.includeAlerts = incAlertsChk.getSelection();
        filter.includeMapSelections = incMapSelectionsChk.getSelection();
        filter.includePastEvents = incPastEventsChk.getSelection();
        filter.includeOrgPilEvents = incOrgPilEvents.getSelection();

        filter.name = "<Custom>";

        GhgConfigData.getInstance().setCurrentFilter(filter);
        setReturnValue(filter.name);
    }

    /**
     * Sets the initial state of the dialog based on current configuration.
     */
    private void setInitialDialogState() {
        /* set the current hazards only check-box */
        showCurrentHazardsChk.setSelection(filter.currentHazards);

        /* set the initial state of the selection boxes */
        for (int i = 0; i < filterArray.length; i++) {
            GhgConfigData.AlertsFilterEnum type = filterArray[i];
            GhgFilterListGroup group = listGroupArray.get(i);
            group.setSelValues(filter.getFilterByType(type));
        }
        /* set the initial state of the Record Consolidation check-boxes */
        combineGeoIdChk.setSelection(filter.combineGeoId);
        combineSegmentsChk.setSelection(filter.combineSegments);
        combinePurgeTimesChk.setSelection(filter.combinePurgeTimes);
        combineActionsChk.setSelection(filter.combineActions);

        /* set the initial state of the Filter Override check-boxes */
        incAlertsChk.setSelection(filter.includeAlerts);
        incMapSelectionsChk.setSelection(filter.includeMapSelections);
        incPastEventsChk.setSelection(filter.includePastEvents);
        incOrgPilEvents.setSelection(filter.includeOrgPilEvents);
    }

    /**
     * Create the main filter controls.
     */
    private void createMainFilterControls() {
        Composite mainControlComp = new Composite(shell, SWT.NONE);
        mainControlComp.setLayout(new GridLayout(1, false));

        showCurrentHazardsChk = new Button(mainControlComp, SWT.CHECK);
        showCurrentHazardsChk.setText("Show Current Hazards Only");
        showCurrentHazardsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.currentHazards = showCurrentHazardsChk.getSelection();
                updateListBoxes();
            }

        });

        createFilterLists(mainControlComp);

        showAdvancedColumnsChk = new Button(mainControlComp, SWT.CHECK);
        showAdvancedColumnsChk.setText("Show Advanced Columns");
        showAdvancedColumnsChk.setSelection(false);
        showAdvancedColumnsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (showAdvancedColumnsChk.getSelection() == true) {
                    GhgFilterListGroup listGroup;
                    for (int i = 4; i < filterArray.length; i++) {
                        listGroup = listGroupArray.get(i);
                        listGroup.setVisible(true);
                        ((GridData) listGroup.getLayoutData()).exclude = false;
                    }
                } else {
                    GhgFilterListGroup listGroup;
                    for (int i = 4; i < filterArray.length; i++) {
                        listGroup = listGroupArray.get(i);
                        listGroup.setVisible(false);
                        ((GridData) listGroup.getLayoutData()).exclude = true;
                    }
                }
                listComp.layout();
                shell.pack();
                shell.layout();
            }
        });
    }

    /**
     * Create the list of filter controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createFilterLists(Composite parentComp) {
        listComp = new Composite(parentComp, SWT.NONE);
        listComp.setLayout(new GridLayout(filterArray.length, false));

        listGroupArray = new ArrayList<GhgFilterListGroup>();

        initListData();

        for (GhgConfigData.AlertsFilterEnum filter : filterArray) {
            switch (filter) {
            case Action:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        GhgConfigData.vtecActionNames));
                break;
            case PhenSig:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        GhgConfigData.getInstance().getPhenSigCodes()));
                break;
            case Pil:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        GhgConfigData.vtecPILNames));
                break;
            case WFO:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        wfoList.toArray(new String[wfoList.size()])));
                break;
            case GeoId:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        geoIdList.toArray(new String[geoIdList.size()])));
                break;
            case ETN:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        etnArr));
                break;
            case Seg:
                listGroupArray.add(new GhgFilterListGroup(listComp, filter,
                        segArr));
                break;
            }
        }

        GhgFilterListGroup listGroup;
        for (int i = 4; i < filterArray.length; i++) {
            listGroup = listGroupArray.get(i);
            listGroup.setVisible(false);
            ((GridData) listGroup.getLayoutData()).exclude = true;
        }

        listComp.layout();
    }

    /**
     * Create the container for the record consolidation and filter override
     * controls.
     */
    private void createGroupControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainGroupComp = new Composite(shell, SWT.NONE);
        mainGroupComp.setLayout(new GridLayout(2, true));
        mainGroupComp.setLayoutData(gd);

        // Create the Record Consolidation group
        createRecordConsolidationGroup(mainGroupComp);

        // Create the Filter Overrides group
        createFilterOverrideGroup(mainGroupComp);
    }

    /**
     * Create the record consolidation group controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createRecordConsolidationGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group recordConGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        recordConGroup.setLayout(gl);
        recordConGroup.setLayoutData(gd);
        recordConGroup.setText(" Record Consolidation ");

        combineGeoIdChk = new Button(recordConGroup, SWT.CHECK);
        combineGeoIdChk.setText("Combine GeoIDs");
        combineGeoIdChk.setSelection(filter.combineGeoId);
        combineGeoIdChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.combineGeoId = combineGeoIdChk.getSelection();
                updateDisplay();
            }
        });

        combineSegmentsChk = new Button(recordConGroup, SWT.CHECK);
        combineSegmentsChk.setText("Combine Segments");
        combineSegmentsChk.setSelection(filter.combineSegments);
        combineSegmentsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.combineSegments = combineSegmentsChk.getSelection();
                updateDisplay();
            }
        });

        combinePurgeTimesChk = new Button(recordConGroup, SWT.CHECK);
        combinePurgeTimesChk.setText("Combine Purge Times");
        combinePurgeTimesChk.setSelection(filter.combinePurgeTimes);
        combinePurgeTimesChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.combinePurgeTimes = combinePurgeTimesChk.getSelection();
                updateDisplay();
            }
        });

        combineActionsChk = new Button(recordConGroup, SWT.CHECK);
        combineActionsChk.setText("Combine Actions");
        combineActionsChk.setSelection(filter.combineActions);
        combineActionsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.combineActions = combineActionsChk.getSelection();
                updateDisplay();
            }
        });
    }

    /**
     * Create the filter override group controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createFilterOverrideGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group filterOverrideGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        filterOverrideGroup.setLayout(gl);
        filterOverrideGroup.setLayoutData(gd);
        filterOverrideGroup.setText(" Filter Overrides ");

        incAlertsChk = new Button(filterOverrideGroup, SWT.CHECK);
        incAlertsChk.setText("Include Alerts");
        incAlertsChk.setSelection(filter.includeAlerts);
        incAlertsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.includeAlerts = incAlertsChk.getSelection();
                updateDisplay();
            }
        });

        incMapSelectionsChk = new Button(filterOverrideGroup, SWT.CHECK);
        incMapSelectionsChk.setText("Include Map Selections");
        incMapSelectionsChk.setSelection(filter.includeMapSelections);
        incMapSelectionsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.includeMapSelections = incMapSelectionsChk
                        .getSelection();
                updateDisplay();
            }
        });

        incPastEventsChk = new Button(filterOverrideGroup, SWT.CHECK);
        incPastEventsChk.setText("Include Past Events");
        incPastEventsChk.setSelection(filter.includePastEvents);
        incPastEventsChk.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.includePastEvents = incPastEventsChk.getSelection();
                updateDisplay();
            }
        });

        incOrgPilEvents = new Button(filterOverrideGroup, SWT.CHECK);
        incOrgPilEvents.setText("Include OrgPil Events");
        incOrgPilEvents.setSelection(filter.includeOrgPilEvents);
        incOrgPilEvents.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                filter.includeOrgPilEvents = incOrgPilEvents.getSelection();
                updateDisplay();
            }
        });
    }

    /**
     * Create the bottom Dismiss button.
     */
    private void createDismissButton() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, true));
        buttonComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Button dismissBtn = new Button(buttonComp, SWT.PUSH);
        dismissBtn.setText("Dismiss");
        dismissBtn.setLayoutData(gd);
        dismissBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFilterConfig();
                close();
            }
        });
    }

    /**
     * Get the data that populates the list boxes.
     */
    private void initListData() {
        // wfos (based on ISC edit areas)
        DataManager dm = DataManager.getCurrentInstance();
        IReferenceSetManager refSetManager = dm.getRefManager();
        List<ReferenceID> rIdList = refSetManager.getAvailableSets();

        try {
            // get list of known sites (WFOs) and geoIds
            List<String> knownSites = dm.knownSites();

            for (int i = 0; i < rIdList.size(); i++) {
                ReferenceID refId = rIdList.get(i);
                String name = refId.getName();
                if ((name.length() == 7) && name.startsWith("ISC_")
                        && knownSites.contains(name.substring(4))) {
                    String wfo4 = SiteMap.getInstance().getSite4LetterId(
                            name.substring(4));
                    wfoList.add(wfo4);
                } else if ((name.length() == 6) && !name.contains("_")) {
                    geoIdList.add(name);
                }
            }

            if (wfoList.contains("KWNS") == false) {
                wfoList.add("KWNS");
            }
            if (wfoList.contains("KNHC") == false) {
                wfoList.add("KNHC");
            }

            // Get the VTEC active table
            List<ActiveTableRecord> activeTableList = DataManager
                    .getCurrentInstance().getActiveTable();

            // Get the etn and seg values, these are based on actual data
            Set<String> etnSet = new TreeSet<String>();
            Set<String> segSet = new TreeSet<String>();
            for (ActiveTableRecord rec : activeTableList) {
                etnSet.add(rec.getEtn());
                segSet.add(String.valueOf(rec.getSeg()));

                etnArr = etnSet.toArray(new String[etnSet.size()]);
                segArr = segSet.toArray(new String[segSet.size()]);
            }

            // Sort the lists
            Collections.sort(wfoList);
            Collections.sort(geoIdList);
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Update the data in the list boxes when the show current hazards check box
     * is selected/deselected
     */
    private void updateListBoxes() {
        /* update the state of the filter boxes */
        if (showCurrentHazardsChk.getSelection()) {
            // Get data from the active table and update the lists accordingly
            try {
                List<ActiveTableRecord> tableDataList = DataManager
                        .getCurrentInstance().getActiveTable();

                // compile the data for each list
                Set<String> actionSet = new HashSet<String>();
                Set<String> phenSigSet = new HashSet<String>();
                Set<String> pilSet = new HashSet<String>();
                Set<String> wfoSet = new HashSet<String>();
                Set<String> geoIdSet = new HashSet<String>();
                Set<String> etnSet = new HashSet<String>();
                Set<String> segSet = new HashSet<String>();

                // Get the data from the activeTable
                for (ActiveTableRecord rec : tableDataList) {
                    GhgData data = new GhgData(rec);

                    if (GhgFilterEngine.filterCheck(data)) {
                        actionSet.add(data.getAction());
                        phenSigSet.add(data.getPhenSig());
                        pilSet.add(data.getPil());
                        wfoSet.add(data.getWfo());
                        geoIdSet.add(data.getGeoId());
                        etnSet.add(data.getEtn());
                        segSet.add(data.getSegNum());
                    }
                }

                for (int i = 0; i < filterArray.length - 2; i++) {
                    GhgConfigData.AlertsFilterEnum type = filterArray[i];
                    GhgFilterListGroup group = listGroupArray.get(i);

                    if (type == AlertsFilterEnum.Action) {
                        group.setListValues(actionSet
                                .toArray(new String[actionSet.size()]));
                    } else if (type == AlertsFilterEnum.PhenSig) {
                        group.setListValues(phenSigSet
                                .toArray(new String[phenSigSet.size()]));
                    } else if (type == AlertsFilterEnum.Pil) {
                        group.setListValues(pilSet.toArray(new String[pilSet
                                .size()]));
                    } else if (type == AlertsFilterEnum.WFO) {
                        group.setListValues(wfoSet.toArray(new String[wfoSet
                                .size()]));
                    } else if (type == AlertsFilterEnum.GeoId) {
                        group.setListValues(geoIdSet
                                .toArray(new String[geoIdSet.size()]));
                    } else if (type == AlertsFilterEnum.ETN) {
                        group.setListValues(etnSet.toArray(new String[etnSet
                                .size()]));
                    } else if (type == AlertsFilterEnum.Seg) {
                        group.setListValues(segSet.toArray(new String[segSet
                                .size()]));
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            // reset the list data
            for (int i = 0; i < filterArray.length - 2; i++) {
                GhgConfigData.AlertsFilterEnum type = filterArray[i];
                GhgFilterListGroup group = listGroupArray.get(i);
                switch (type) {
                case Action:
                    group.setListValues(GhgConfigData.vtecActionNames);
                    break;
                case PhenSig:
                    group.setListValues(GhgConfigData.getInstance()
                            .getPhenSigCodes());
                    break;
                case Pil:
                    group.setListValues(GhgConfigData.vtecPILNames);
                    break;
                case WFO:
                    group.setListValues(wfoList.toArray(new String[wfoList
                            .size()]));
                    break;
                case GeoId:
                    group.setListValues(geoIdList.toArray(new String[geoIdList
                            .size()]));
                    break;
                }
            }
        }
    }

    private void updateDisplay() {
        filter.name = GhgConfigData.CUSTOM_FILTER_NAME;
        GhgConfigData.getInstance().setCurrentFilter(filter);
    }
}
