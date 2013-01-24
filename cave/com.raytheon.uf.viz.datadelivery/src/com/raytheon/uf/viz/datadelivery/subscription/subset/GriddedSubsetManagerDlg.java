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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Ordering;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedSubsetManagerDlg
        extends
        SubsetManagerDlg<GriddedDataSet, GriddedTimingSubsetPresenter, SpecificDateTimeXML> {
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
    protected <T extends Subscription> T createSubscription(T sub) {
        T subscription = super.createSubscription(sub);
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
        return subscription;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected SpecificDateTimeXML getTimeXml() {
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
}
