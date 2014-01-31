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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ResponseProcessingUtilities;
import com.raytheon.uf.edex.datadelivery.retrieval.util.RetrievalGeneratorUtilities;

/**
 * 
 * {@link RetrievalGenerator} implementation for OpenDAP.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011 218        dhladky      Initial creation
 * Jul 24, 2012 955        djohnson     Check multiple datasets for cycles provided on subscription.
 * Aug 02, 2012 955        djohnson     Use DataSetQuery to get all metadata objects.
 * Aug 10, 2012 1022       djohnson     Retrieve latest available url from {@link OpenDapGriddedDataSet}.
 * Aug 20, 2012 0743       djohnson     Cycle will no longer be null.
 * Sep 24, 2012 1209       djohnson     NO_CYCLE metadatas can now fulfill subscriptions.
 * Oct 05, 2012 1241       djohnson     Replace RegistryManager calls with registry handler calls.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Nov 25, 2012 1340       dhladky      Added type for subscriptions to retrieval
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Sep 18, 2013 2383       bgonzale     Added subscription name to log output.
 * Sept 25, 2013 1797      dhladky      separated time from gridded time
 * 
 * @author djohnson
 * @version 1.0
 */
class OpenDAPRetrievalGenerator extends RetrievalGenerator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDAPRetrievalGenerator.class);

    private static <T extends Subscription<GriddedTime, GriddedCoverage>> List<T> addMostRecentDataSetMetaDataUrlToSubscriptions(
            List<T> subscriptions) {
        // Find out what the most recent url this subscription should use would
        // be
        Iterator<T> iter = subscriptions.iterator();
        while (iter.hasNext()) {
            Subscription<GriddedTime, GriddedCoverage> subscription = iter.next();
            DataSet result = null;
            try {
                result = DataDeliveryHandlers.getDataSetHandler()
                        .getByNameAndProvider(subscription.getDataSetName(),
                                subscription.getProvider());
                if (result == null
                        || !(result instanceof OpenDapGriddedDataSet)) {
                    // TODO: Send notification?
                    statusHandler
                            .warn("Unable to satisfy the criteria for a new subscription!");
                    iter.remove();
                    continue;

                }
            } catch (RegistryHandlerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve dataset.", e);
            }

            OpenDapGriddedDataSet openDapGriddedDataSet = (OpenDapGriddedDataSet) result;
            Iterator<Integer> cycleIter = openDapGriddedDataSet
                    .newestToOldestIterator();

            List<Integer> subCycleTimes = subscription.getTime()
                    .getCycleTimes();
            if (subCycleTimes.isEmpty()) {
                subCycleTimes.add(GriddedDataSetMetaData.NO_CYCLE);
            }

            while (cycleIter.hasNext()) {
                Integer cycle = cycleIter.next();
                if (subCycleTimes.contains(cycle)) {
                    subscription.setUrl(openDapGriddedDataSet.getCyclesToUrls()
                            .get(cycle));
                    break;
                }
            }

            if (subscription.getUrl() == null) {
                iter.remove();
                continue;
            }
        }

        return subscriptions;
    }

    /**
     * Determines the retrieval URL for a subscription that has multiple cycle
     * times. Each dataset by name will be considered, and the one providing a
     * cycle time subscribed to with the newest start date will be used.
     * 
     * @param subscription
     *            the subscription
     * @return the url for retrieval or null if no retrieval should take place
     */
    private static String getRetrievalUrl(Subscription<GriddedTime, GriddedCoverage> subscription) {
        String url = subscription.getUrl();

        DataSetMetaData result = null;
        try {
            result = DataDeliveryHandlers.getDataSetMetaDataHandler().getById(
                    url);
            if (result == null) {
                throw new RegistryHandlerException(
                        "Unable to find the dataset by id from its unique url!",
                        new NullPointerException("DataSetMetaData"));
            }

            if (satisfiesSubscriptionCriteria(subscription, result)) {
                return url;
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to find the dataset by id from its unique url!", e);
        }

        return null;
    }

    /**
     * Determines whether a subscription can be satisified by the dataset
     * metadata.
     * 
     * @param subscription
     *            the subscription
     * @param dsmd
     *            the dataset metadata
     * @return true if the datasetmetadata will satisfy the subscription
     */
    @VisibleForTesting
    static boolean satisfiesSubscriptionCriteria(Subscription<GriddedTime, GriddedCoverage> subscription,
            DataSetMetaData dsmd) {
        List<Integer> cycleTimes = subscription.getTime().getCycleTimes();
        // If the subscription doesn't have cycle times subscribed to, then add
        // the NO_CYCLE marker cycle
        if (cycleTimes.isEmpty()) {
            cycleTimes.add(GriddedDataSetMetaData.NO_CYCLE);
        }

        if (dsmd instanceof GriddedDataSetMetaData) {
            GriddedDataSetMetaData data = (GriddedDataSetMetaData) dsmd;
            int cycle = data.getCycle();

            if (!cycleTimes.contains(cycle)) {
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.debug(subscription.getName()
                            + " is not subscribed to cycle [" + cycle + "]");
                }
            } else {
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.debug(subscription.getName()
                            + " is subscribed to cycle [" + cycle + "]");
                }
                return true;
            }
        }

        return false;
    }

    public OpenDAPRetrievalGenerator() {
        super(ServiceType.OPENDAP);
    }

    /***
     * Build the necessary retrieval objects
     * 
     * @param bundle
     * @return
     */
    @Override
    public List<Retrieval> buildRetrieval(SubscriptionBundle bundle) {
        List<Retrieval> retrievals = Collections.emptyList();
        switch (bundle.getDataType()) {
        case GRID:
            retrievals = getGridRetrievals(bundle);
            break;
        default:
            statusHandler.error("Point DATA OPENDAP NOT YET IMPLEMENTED");
        }

        return retrievals;
    }

    /**
     * Gets the size of the data set
     * 
     * @param cov
     * @return
     */
    private int getDimensionalSize(GriddedCoverage cov) {

        return cov.getGridCoverage().getNx()
                * cov.getGridCoverage().getNy();
    }

    /**
     * Process the RetrievalAttribute
     * 
     * @param parm
     * @return
     */
    protected Map<DataTime, List<Level>> getGridDuplicates(
            String name,
            Parameter parm, List<DataTime> times, List<Level> levels,
            List<String> ensembleMembers,
            GriddedCoverage cov) {

        return RetrievalGeneratorUtilities.findGridDuplicates(name, times,
                levels, ensembleMembers,
                parm, cov.getRequestGridCoverage());
    }

    /**
     * create the grid type retrievals
     * 
     * @param bundle
     * @return
     */
    @SuppressWarnings("unchecked")
    private List<Retrieval> getGridRetrievals(SubscriptionBundle bundle) {

        List<Retrieval> retrievals = new ArrayList<Retrieval>();
        Subscription<GriddedTime, GriddedCoverage> sub = bundle.getSubscription();

        int sfactor = getSizingFactor(getDimensionalSize(sub.getCoverage()));
        sub = removeDuplicates(sub);

        if (sub != null) {

            if (sub.getUrl() == null) {

                List<Subscription<GriddedTime, GriddedCoverage>> subs = new ArrayList<Subscription<GriddedTime, GriddedCoverage>>(1);
                subs.add(sub);

                List<Subscription<GriddedTime, GriddedCoverage>> fillableSubs = addMostRecentDataSetMetaDataUrlToSubscriptions(subs);

                if (!CollectionUtil.isNullOrEmpty(fillableSubs)) {
                    sub = fillableSubs.get(0);
                } else if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler
                            .debug("Subscription with null url is not retrievable at this time.  There are no relevant DataSetMetaData objects with which to fill it.");
                    sub = null;
                    return Collections.emptyList();
                }
            }

            GriddedTime subTime = sub.getTime();

            String retrievalUrl = getRetrievalUrl(sub);
            sub.setUrl(retrievalUrl);

            if (sub.getUrl() == null) {
                statusHandler
                        .info("Skipping subscription "
                                + sub.getName()
                                + " that is unfulfillable with the current metadata (null URL.)");
                return Collections.emptyList();
            }

            List<Ensemble> ensembles = null;
            if (sub.getEnsemble() == null) {
                ensembles = Arrays.asList((Ensemble) null);
            } else {
                ensembles = sub.getEnsemble().split(1);
            }

            for (List<Integer> timeSequence : subTime.getTimeSequences(sfactor)) {

                for (Parameter param : (List<Parameter>)sub.getParameter()) {
                    final Levels paramLevels = param.getLevels();
                    if (CollectionUtil.isNullOrEmpty(paramLevels
                            .getSelectedLevelIndices())) {
                        // handle single level
                        paramLevels.setRequestLevelEnd(0);
                        paramLevels.setRequestLevelStart(0);
                        List<GriddedTime> times = processTime(timeSequence,
                                (GriddedTime)sub.getTime());

                        for (GriddedTime time : times) {
                            for (Ensemble ensemble : ensembles) {
                                Retrieval retrieval = getRetrieval(sub, bundle,
                                        param, paramLevels, time, ensemble);
                                retrievals.add(retrieval);
                            }
                        }

                    } else {
                        for (List<Integer> levelSequence : paramLevels
                                .getLevelSequences(sfactor)) {

                            List<GriddedTime> times = processTime(timeSequence,
                                    sub.getTime());
                            List<Levels> levels = processLevels(levelSequence,
                                    paramLevels);

                            // temporarily make all requests single level
                            // and time
                            for (Time time : times) {
                                for (Levels level : levels) {
                                    for (Ensemble ensemble : ensembles) {
                                        Retrieval retrieval = getRetrieval(sub,
                                                bundle, param, level, time,
                                                ensemble);
                                        retrievals.add(retrieval);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return retrievals;
    }

    /**
     * Get the retrieval
     * 
     * @param sub
     * @param bundle
     * @param param
     * @param level
     * @param Time
     * @return
     */
    private Retrieval getRetrieval(Subscription<GriddedTime, GriddedCoverage> sub, SubscriptionBundle bundle,
            Parameter param, Levels level, Time time, Ensemble ensemble) {

        Retrieval retrieval = new Retrieval();
        retrieval.setSubscriptionName(sub.getName());
        retrieval.setServiceType(getServiceType());
        retrieval.setConnection(bundle.getConnection());
        retrieval.getConnection().setUrl(sub.getUrl());
        retrieval.setOwner(sub.getOwner());
        retrieval.setSubscriptionType(getSubscriptionType(sub));
        retrieval.setNetwork(sub.getRoute());

        // Coverage and type processing
        Coverage cov = sub.getCoverage();
        if (!(cov instanceof GriddedCoverage)) {
            throw new UnsupportedOperationException(
                    "OPENDAP retrieval does not yet support coverages other than Gridded. ");
        }

        // Attribute processing
        RetrievalAttribute att = new RetrievalAttribute();
        Parameter lparam = processParameter(param);
        att.setCoverage(cov);
        lparam.setLevels(level);
        att.setTime(time);
        att.setParameter(lparam);
        att.setEnsemble(ensemble);
        att.setSubName(retrieval.getSubscriptionName());
        Provider provider;
        try {
            provider = DataDeliveryHandlers.getProviderHandler().getByName(
                    sub.getProvider());
        } catch (RegistryHandlerException e) {
            throw new IllegalArgumentException(
                    "Error looking up the provider!", e);
        }
        // Look up the provider's configured plugin for this data type
        ProviderType providerType = provider.getProviderType(sub
                .getDataSetType());
        att.setPlugin(providerType.getPlugin());
        att.setProvider(sub.getProvider());
        retrieval.addAttribute(att);

        return retrieval;
    }

    /**
     * {@inheritDoc}
     */

    @Override
    protected RetrievalAdapter getServiceRetrievalAdapter() {
        return new OpenDAPRetrievalAdapter();
    }

    /**
     * Sizing factor so we don't run box out of Heap
     * 
     * @param size
     * @return
     */
    public int getSizingFactor(int size) {

        int sfactor = 0;

        if (size > 1000000) {
            sfactor = 1;
        } else if (size < 1000000 && size > 750000) {
            sfactor = 2;
        } else if (size < 750000 && size > 500000) {
            sfactor = 3;
        } else if (size < 500000 && size > 250000) {
            sfactor = 4;
        } else {
            sfactor = 5;
        }

        return sfactor;
    }

    /**
     * Get the level sequence
     * 
     * @param levelSequence
     * @param parmLevels
     * @return
     */
    private List<Levels> processLevels(List<Integer> levelSequence,
            Levels parmLevels) {

        List<Levels> levels = new ArrayList<Levels>();
        for (int i = 0; i < levelSequence.size(); i++) {
            Levels level = new Levels();
            level.setLevel(parmLevels.getLevel());
            level.setDz(parmLevels.getDz());
            level.setLevelType(parmLevels.getLevelType());
            Integer currentLevelSequence = levelSequence.get(i);
            level.setRequestLevelStart(currentLevelSequence);
            level.setRequestLevelEnd(currentLevelSequence);
            level.setName(parmLevels.getName());
            level.setSelectedLevelIndices(Arrays.asList(currentLevelSequence));
            levels.add(level);
        }

        return levels;

    }

    /**
     * Process sequences of hours for separate retrieval
     * 
     * @param timeSequence
     * @param subTime
     * @return
     */
    private ArrayList<GriddedTime> processTime(List<Integer> timeSequence, GriddedTime subTime) {

        ArrayList<GriddedTime> times = new ArrayList<GriddedTime>();
        for (int i = 0; i < timeSequence.size(); i++) {
            GriddedTime time = new GriddedTime();
            time.setEnd(subTime.getEnd());
            time.setStart(subTime.getStart());
            time.setNumTimes(subTime.getNumTimes());
            time.setFormat(subTime.getFormat());
            time.setStep(subTime.getStep());
            time.setStepUnit(subTime.getStepUnit());
            ArrayList<Integer> indicies = new ArrayList<Integer>(1);
            indicies.add(timeSequence.get(i));
            time.setSelectedTimeIndices(indicies);
            time.setRequestStartTimeAsInt(timeSequence.get(i));
            time.setRequestEndTimeAsInt(timeSequence.get(i));
            times.add(time);
        }

        return times;
    }

    /**
     * Remove duplicate levels, times, subscriptions
     */
    @SuppressWarnings("unchecked")
    @Override
    protected Subscription<GriddedTime, GriddedCoverage> removeDuplicates(Subscription<?, ?> subin) {

        Subscription<GriddedTime, GriddedCoverage> sub = (Subscription<GriddedTime, GriddedCoverage>)subin;
        GriddedCoverage cov = sub.getCoverage();
        GriddedTime time = sub.getTime();
        
        int sfactor = getSizingFactor(getDimensionalSize(cov));

        List<String> ensembles = null;
        if (sub.getEnsemble() != null && sub.getEnsemble().hasSelection()) {
            ensembles = sub.getEnsemble().getSelectedMembers();
        } else {
            ensembles = Arrays.asList((String) null);
        }

        for (List<Integer> timeSequence : time.getTimeSequences(
                sfactor)) {

            for (Parameter param : (List<Parameter>)sub.getParameter()) {

                if (param.getLevels().getSelectedLevelIndices() == null
                        || param.getLevels().getSelectedLevelIndices().size() == 0) {
                    // levels don't matter so much here it's just one
                    param.getLevels().setRequestLevelEnd(0);
                    param.getLevels().setRequestLevelStart(0);

                    ArrayList<DataTime> times = null;

                    for (GriddedTime gtime : processTime(timeSequence, time)) {
                        times = ResponseProcessingUtilities
                                .getOpenDAPGridDataTimes(gtime);
                    }

                    ArrayList<Level> levels = ResponseProcessingUtilities
                            .getOpenDAPGridLevels(param.getLevels());

                    Map<DataTime, List<Level>> dups = getGridDuplicates(
                            sub.getName(),
 param, times, levels, ensembles,
                            cov);

                    for (int i = 0; i < times.size(); i++) {
                        DataTime dtime = times.get(i);
                        List<Level> levDups = dups.get(dtime);

                        if (levDups != null) {
                            // single level, remove the time
                            time.getSelectedTimeIndices()
                                    .remove(timeSequence.get(i));
                            statusHandler.info("Removing duplicate time: "
                                    + dtime.toString());
                        }
                    }

                } else {

                    for (List<Integer> levelSequence : param.getLevels()
                            .getLevelSequences(sfactor)) {

                        ArrayList<DataTime> times = null;
                        ArrayList<Level> plevels = null;

                        for (GriddedTime gtime : processTime(timeSequence,
                                time)) {
                            times = ResponseProcessingUtilities
                                    .getOpenDAPGridDataTimes(gtime);
                        }
                        for (Levels level : processLevels(levelSequence,
                                param.getLevels())) {
                            plevels = ResponseProcessingUtilities
                                    .getOpenDAPGridLevels(level);
                        }

                        Map<DataTime, List<Level>> dups = getGridDuplicates(
                                sub.getName(), param, times, plevels,
                                ensembles,
                                cov);

                        for (int i = 0; i < times.size(); i++) {
                            DataTime dtime = times.get(i);
                            List<Level> levDups = dups.get(dtime);

                            if (levDups != null) {

                                if (levDups.size() == plevels.size()) {
                                    // just remove the entire time
                                    time.getSelectedTimeIndices()
                                            .remove(timeSequence.get(i));
                                    statusHandler
                                            .info("Removing duplicate time: "
                                                    + dtime.toString());
                                } else {

                                    for (int j = 0; j < plevels.size(); j++) {
                                        Level lev = plevels.get(j);
                                        for (Level plev : levDups) {
                                            if (plev.equals(lev)) {
                                                param.getLevels()
                                                        .getSelectedLevelIndices()
                                                        .remove(levelSequence
                                                                .get(j));
                                                statusHandler
                                                        .info("Removing duplicate level: "
                                                                + lev.getMasterLevel()
                                                                        .getDescription());
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
        // remove entire subscription, it's a duplicate
        if (time.getSelectedTimeIndices().size() == 0) {
            statusHandler.info("Removing duplicate subscription: "
                    + sub.getName());
            return null;
        }

        return sub;

    }

}
