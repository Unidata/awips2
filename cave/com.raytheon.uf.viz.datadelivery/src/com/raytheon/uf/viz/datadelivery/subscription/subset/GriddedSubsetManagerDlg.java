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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Ordering;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.GriddedTimingSelectionPresenter;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.GriddedTimingSubsetPresenter;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SpecificDateTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.VerticalXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * {@link SubsetManagerDlg} for gridded data sets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Removed call to add cycle times to subscription.
 * Sep 27, 2012 1202       bgonzale     Fixed dateStringtoDateMap key creation.
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Oct 11, 2012 1263       jpiatt       Modified for cancel flag.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 04, 2013 1299       djohnson     Add logging of invalid forecast hour information if it occurs again.
 * Jan 04, 2013 1420       mpduff       Pass cycles in for rules.
 * Jan 18, 2013 1414       bsteffen     Add ensemble tab.
 * Jan 28, 2013 1533       djohnson     Update the calculated dataset size after loading subset xml.
 * Mar 21, 2013 1794       djohnson     Add option to create a shared subscription, if phase3 code is available.
 * Mar 29, 2013 1841       djohnson     Subscription is now UserSubscription.
 * May 21, 2013 2020       mpduff       Rename UserSubscription to SiteSubscription.
 * Jun 04, 2013  223       mpduff       Added grid specific items to this class.
 * 
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedSubsetManagerDlg
        extends
        SubsetManagerDlg<GriddedDataSet, GriddedTimingSubsetPresenter, SpecificDateTimeXML> {
    private static final String TIMING_TAB_GRID = "Forecast Hours";

    @VisibleForTesting
    final String POPUP_TITLE = "Notice";

    @VisibleForTesting
    final String NO_DATA_FOR_DATE_AND_CYCLE = "No data is available for the specified date and cycle combination.";

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GriddedSubsetManagerDlg.class);

    private final ThreadLocal<DateFormat> dateFormat = new ThreadLocal<DateFormat>() {
        @Override
        protected DateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy - H 'Z'");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };

    private final List<String> asString = new ArrayList<String>();

    @VisibleForTesting
    final Map<String, ImmutableDate> dateStringToDateMap = new HashMap<String, ImmutableDate>();

    private boolean useLatestDate = true;

    private DataSetMetaData metaData;

    private GriddedEnsembleSubsetTab ensembleTab;

    private TabItem timingTab;

    /**
     * Constructor.
     * 
     * @param shell
     * @param loadDataSet
     * @param subscription
     */
    public GriddedSubsetManagerDlg(Shell shell, boolean loadDataSet,
            Subscription subscription) {
        super(shell, loadDataSet, subscription);
    }

    /**
     * Constructor.
     * 
     * @param shell
     * @param dataSet
     * @param loadDataSet
     * @param subsetXml
     */
    public GriddedSubsetManagerDlg(Shell shell, GriddedDataSet dataSet,
            boolean loadDataSet, SubsetXML<SpecificDateTimeXML> subsetXml) {
        super(shell, dataSet, loadDataSet, subsetXml);
    }

    /**
     * Constructor.
     * 
     * @param shell
     * @param dataSet
     */
    public GriddedSubsetManagerDlg(Shell shell, GriddedDataSet dataSet) {
        super(shell, dataSet);
    }

    @Override
    protected void createTabs(TabFolder tabFolder) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        TabItem verticalTab = new TabItem(tabFolder, SWT.NONE);
        verticalTab.setText(VERTICAL_TAB);
        verticalTab.setData("valid", false);
        Composite vertComp = new Composite(tabFolder, SWT.NONE);
        vertComp.setLayout(gl);
        vertComp.setLayoutData(gd);
        verticalTab.setControl(vertComp);
        vTab = new VerticalSubsetTab(vertComp, dataSet, this);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);

        timingTab = new TabItem(tabFolder, SWT.NONE);
        timingTab.setText(TIMING_TAB_GRID);
        timingTab.setData("valid", false);
        Composite timingComp = new Composite(tabFolder, SWT.NONE);
        timingComp.setLayout(gl);
        timingComp.setLayoutData(gd);
        timingTab.setControl(timingComp);
        timingTabControls = getDataTimingSubsetPresenter(timingComp, dataSet,
                this, shell);
        timingTabControls.init();

        Ensemble e = dataSet.getEnsemble();
        if (e != null && e.getMembers() != null) {
            TabItem ensembleTabItem = new TabItem(tabFolder, SWT.NONE, 2);
            Composite ensembleComp = new Composite(tabFolder, SWT.NONE);
            ensembleComp.setLayout(new GridLayout(1, false));
            ensembleComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT,
                    true, false));
            ensembleTabItem.setControl(ensembleComp);
            ensembleTab = new GriddedEnsembleSubsetTab(ensembleComp,
                    dataSet.getEnsemble());
            ensembleTab.addListener(this);
            ensembleTabItem.setText(ensembleTab.getName());
        }
    }

    @Override
    protected Collection<String> getInvalidTabs() {
        Collection<String> invalidTabs = super.getInvalidTabs();
        if (ensembleTab != null && !ensembleTab.isValid()) {
            invalidTabs.add(ensembleTab.getName());
        }

        if (!vTab.isValid()) {
            invalidTabs.add(VERTICAL_TAB);
        }

        if (!timingTabControls.isValid()) {
            invalidTabs.add(timingTab.getText());
        }

        return invalidTabs;
    }

    @Override
    protected void populateSubsetXML(SubsetXML<SpecificDateTimeXML> subset) {
        super.populateSubsetXML(subset);
        if (ensembleTab != null) {
            ensembleTab.populateSubsetXML(subset);
        }
    }

    @Override
    protected void loadFromSubsetXML(SubsetXML<SpecificDateTimeXML> subsetXml) {
        super.loadFromSubsetXML(subsetXml);
        if (ensembleTab != null) {
            ensembleTab.loadFromSubsetXML(subsetXml);
        }

        ArrayList<VerticalXML> vertList = subsetXml.getVerticalList();
        vTab.populate(vertList, dataSet);

        SpecificDateTimeXML time = subsetXml.getTime();
        this.timingTabControls.populate(time, dataSet);

        updateDataSize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg
     * #loadFromSubscription
     * (com.raytheon.uf.common.datadelivery.registry.Subscription)
     */
    @Override
    protected void loadFromSubscription(Subscription subscription) {
        super.loadFromSubscription(subscription);
        if (ensembleTab != null) {
            ensembleTab.loadFromSubscription(subscription);
        }

        // Cycle time
        SpecificDateTimeXML timeXml = getTimeXmlFromSubscription();

        timeXml.setLatestData(true);

        this.timingTabControls.populate(timeXml, dataSet);

        // Vertical/Parameters
        Map<String, VerticalXML> levelMap = new HashMap<String, VerticalXML>();
        List<Parameter> paramaterList = this.subscription.getParameter();

        for (Parameter p : paramaterList) {
            for (DataLevelType levelType : p.getLevelType()) {
                if (!levelMap.containsKey(levelType.getKey())) {
                    VerticalXML v = new VerticalXML();
                    if (levelType.getUnit() == null) {
                        v.setLayerType(String.valueOf(levelType
                                .getDescription()));
                    } else {
                        v.setLayerType(levelType.getDescription() + " ("
                                + levelType.getUnit() + "" + ")");
                    }
                    levelMap.put(levelType.getKey(), v);
                }
                VerticalXML v = levelMap.get(levelType.getKey());
                v.addParameter(p.getProviderName());

                // TODO - This is set up to only have one level type with
                // Multiple parameters. This will need to change if other
                // Data providers have parameters with multiple level types
                // containing multiple levels
                if (levelType.getId() == 100) {
                    final Levels levels = p.getLevels();
                    final List<Integer> selectedLevelIndices = levels
                            .getSelectedLevelIndices();
                    for (int index : selectedLevelIndices) {
                        v.addLevel(String.valueOf(levels.getLevel().get(index)));
                    }
                }
            }
        }

        ArrayList<VerticalXML> vertList = new ArrayList<VerticalXML>(
                levelMap.values());
        vTab.populate(vertList, dataSet);
    }

    @Override
    protected boolean isDirty() {
        boolean modified = super.isDirty();
        if (!modified && ensembleTab != null) {
            modified = ensembleTab.isModified();
        }
        return modified;
    }

    @Override
    protected void setClean() {
        super.setClean();
        if (ensembleTab != null) {
            ensembleTab.setModified(false);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected GriddedTimingSubsetPresenter getDataTimingSubsetPresenter(
            Composite parentComp, GriddedDataSet dataSet, IDataSize callback,
            Shell shell) {
        GriddedTimingSubsetTab view = new GriddedTimingSubsetTab(parentComp,
                callback, shell);
        return new GriddedTimingSubsetPresenter(dataSet, view);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T extends SiteSubscription> T createSubscription(T sub,
            Network defaultRoute) {
        T subscription = super.createSubscription(sub, defaultRoute);
        if (subscription == null) {
            return null;
        }

        subscription.setUrl(getSubscriptionUrl());

        Time time = sub.getTime();
        List<String> fcstHours = time.getFcstHours();

        // Set the gridded specific data on the time object
        String[] selectedItems = this.timingTabControls.getSelectedFcstHours();
        List<Integer> fcstIndices = new ArrayList<Integer>();
        for (String hr : selectedItems) {
            fcstIndices.add(fcstHours.indexOf(hr));
        }

        time.setSelectedTimeIndices(fcstIndices);
        subscription.setTime(time);

        if (ensembleTab != null) {
            ensembleTab.populateSubscription(subscription);
        }

        return subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected SpecificDateTimeXML getTimeXmlFromSubscription() {
        SpecificDateTimeXML timeXml = new SpecificDateTimeXML();
        Time time = this.subscription.getTime();
        List<Integer> cycleTimes = time.getCycleTimes();
        if (!CollectionUtil.isNullOrEmpty(cycleTimes)) {
            for (int cycle : cycleTimes) {
                timeXml.addCycle(cycle);
            }
        }

        // All Forecast hours
        List<String> fcstHours = time.getFcstHours();
        final int numberOfFcstHours = fcstHours.size();

        // Selected Forecast hour indices
        List<Integer> selectedTimeIndices = time.getSelectedTimeIndices();
        if (!CollectionUtil.isNullOrEmpty(selectedTimeIndices)) {
            for (int idx : selectedTimeIndices) {
                if (idx < 0 || idx >= numberOfFcstHours) {
                    warnOfInvalidForecastHourIndex(this.subscription,
                            numberOfFcstHours, idx);
                } else {
                    timeXml.addHour(fcstHours.get(idx));
                }
            }
        }
        return timeXml;
    }

    /**
     * Warns of an invalid forecast hour index, with debugging information.
     * 
     * @param subscription
     *            the time object
     * @param numberOfFcstHours
     *            the number of forecast hours in the time object
     * @param idx
     *            the requested index, which was invalid
     */
    private void warnOfInvalidForecastHourIndex(Subscription subscription,
            final int numberOfFcstHours, int idx) {
        String subscriptionAsXml;
        try {
            subscriptionAsXml = new JAXBManager(Subscription.class)
                    .marshalToXml(subscription);
        } catch (JAXBException e) {
            StringWriter writer = new StringWriter();
            writer.append("Unable to convert the subscription object to xml:");
            e.printStackTrace(new PrintWriter(writer));
            subscriptionAsXml = writer.toString();
        }

        statusHandler
                .handle(Priority.WARN,
                        String.format(
                                "Invalid value for selected forecast hour.  Expected less than [%s] but was [%s].\nSubscription represented as XML:\n%s",
                                numberOfFcstHours, idx, subscriptionAsXml),
                        new IllegalStateException("Debugging stacktrace"));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateDataSize() {
        if (!initialized) {
            return;
        }

        // Update the data set size label text.

        // Get the number of requested grids
        dataSize.determineNumberRequestedGrids(vTab.getParameters());

        // Get the temporal data
        int numFcstHours = this.timingTabControls.getSelectedFcstHours().length;
        dataSize.setNumFcstHours(numFcstHours);
        if (ensembleTab != null) {
            dataSize.setNumEnsembleMembers(ensembleTab
                    .getEnsembleWithSelection());
        } else {
            dataSize.setNumEnsembleMembers(dataSet.getEnsemble());
        }
        // Get the Areal data
        ReferencedEnvelope envelope = this.spatialTabControls.getEnvelope();

        dataSize.setEnvelope(envelope);

        this.sizeLbl.setText(SizeUtil.prettyByteSize(dataSize
                .getDataSetSizeInBytes())
                + " of "
                + SizeUtil.prettyByteSize(dataSize.getFullSizeInBytes()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Time setupDataSpecificTime(Time newTime, Subscription sub) {
        if (asString.isEmpty()) {
            SortedSet<ImmutableDate> newestToOldest = new TreeSet<ImmutableDate>(
                    Ordering.natural().reverse());
            try {
                newestToOldest.addAll(DataDeliveryHandlers
                        .getDataSetMetaDataHandler().getDatesForDataSet(
                                dataSet.getDataSetName(),
                                dataSet.getProviderName()));
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve dates for the dataset!", e);
            }

            if (newestToOldest.isEmpty()) {
                asString.add("No Data Available");
            } else {
                for (ImmutableDate date : newestToOldest) {
                    this.dataSet.getTime().getCycleTimes();
                    String displayString = dateFormat.get().format(date);

                    if (!asString.contains(displayString)) {
                        asString.add(displayString);
                        dateStringToDateMap.put(displayString, date);
                    }
                }
            }
        }

        GriddedTimingSelectionPresenter presenter = new GriddedTimingSelectionPresenter(
                new GriddedTimingSelectionDlg(getShell(), dataSet.getCycles(),
                        sub), dataSet, asString);
        Integer cycle = presenter.open();

        if (presenter.isCancel()) {
            return null;
        }

        if (cycle != null) {
            Time time;
            this.useLatestDate = (cycle == -999 ? true : false);
            if (!useLatestDate) {
                newTime.addCycleTime(cycle);
                String selectedDate = presenter.getDate();
                metaData = retrieveFilteredDataSetMetaData(selectedDate, cycle);
                if (metaData == null) {
                    return null;
                } else {
                    time = metaData.getTime();
                    time.addCycleTime(cycle);
                    return time;
                }
            } else {
                // If ulse latest data is selected then add all cycle times, the
                // retrieval generator will determine which one to use.
                time = dataSet.getTime();
                for (Integer c : new TreeSet<Integer>(dataSet.getCycles())) {
                    time.addCycleTime(c);
                }
            }

            return time;
        }

        return null;
    }

    /**
     * Retrieve the filtered {@link DATASETMETADATA}.
     * 
     * @return the DataSetMetaData that applies, or null if none
     */
    protected DataSetMetaData retrieveFilteredDataSetMetaData(
            String selectedDate, int cycle) {
        try {
            GriddedDataSetMetaData dsmd = DataDeliveryHandlers
                    .getGriddedDataSetMetaDataHandler()
                    .getByDataSetDateAndCycle(dataSet.getDataSetName(),
                            dataSet.getProviderName(), cycle,
                            dateStringToDateMap.get(selectedDate));
            if (dsmd == null) {
                DataDeliveryUtils.showMessage(getShell(), SWT.OK, POPUP_TITLE,
                        NO_DATA_FOR_DATE_AND_CYCLE);
            }
            return dsmd;
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving applicable DataSetMetaData.", e);
            return null;
        }
    }

    /**
     * Return the {@link Time} object that should be associated with this
     * subscription. It will either be null for a reoccurring subscription, or
     * the {@link DataSetMetaData} url if an adhoc query is being performed for
     * a non-latest date.
     * 
     * @return the url to use
     */
    public String getSubscriptionUrl() {
        return (this.useLatestDate || this.metaData == null) ? null
                : this.metaData.getUrl();
    }

    @Override
    protected <T extends Subscription> T populateSubscription(T sub,
            boolean create) {
        ArrayList<Parameter> selectedParameterObjs = vTab.getParameters();
        sub.setParameter(selectedParameterObjs);

        Time dataSetTime = dataSet.getTime();

        Time newTime = new Time();
        newTime.setEnd(dataSetTime.getEnd());
        newTime.setFormat(dataSetTime.getFormat());
        newTime.setNumTimes(dataSetTime.getNumTimes());
        newTime.setRequestEnd(dataSetTime.getRequestEnd());
        newTime.setRequestStart(dataSetTime.getRequestStart());
        newTime.setStart(dataSetTime.getStart());
        newTime.setStep(dataSetTime.getStep());
        newTime.setStepUnit(dataSetTime.getStepUnit());

        if (sub instanceof AdhocSubscription) {
            newTime = setupDataSpecificTime(newTime, sub);
        } else if (!create) {
            newTime.setCycleTimes(this.subscription.getTime().getCycleTimes());
        }

        sub.setTime(newTime);

        // TODO Phase 1 is only gridded coverage
        GriddedCoverage cov = (GriddedCoverage) dataSet.getCoverage();
        cov.setModelName(dataSet.getDataSetName());
        cov.setGridName(getNameText());
        GridCoverage coverage = cov.getGridCoverage();
        coverage.setName(getNameText());

        if (spatialTabControls.useDataSetSize()) {
            cov.setRequestEnvelope(cov.getEnvelope());
            sub.setFullDataSet(true);
        } else {
            cov.setRequestEnvelope(spatialTabControls.getEnvelope());
            sub.setFullDataSet(false);
        }

        sub.setCoverage(cov);

        return sub;
    }
}
