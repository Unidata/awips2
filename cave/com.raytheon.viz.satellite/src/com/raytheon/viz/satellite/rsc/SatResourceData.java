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
package com.raytheon.viz.satellite.rsc;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.satellite.inventory.SatelliteDataCubeAdapter;
import com.raytheon.viz.satellite.inventory.SatelliteInventory;

/**
 * Resource data for satellite data
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 17, 2009           njensen     Initial creation
 * Feb 20, 2000  2032     jsanchez    Added @XmlAccessorType(XmlAccessType.NONE).
 * Apr 23, 2013  2947     bsteffen    Fix updates for derived products with
 *                                    multiple records per frame.
 * Jul 13, 2016  20487    jburks      Added variable to toggle the showing of incomplete
 *                                    frames
 * Dec 11, 2017  DCS19856 jburks      Add boolean to track if product support incomplete frames toggling and fixed
 *                                    incomplete frames issue.
 * Feb 10, 2021  20421 mgamazaychikov Add support for centalWaveLength handling
 * May 27, 2021  22589    jkelmer     Added caching to getAvailableTimes()
 * Mar 21, 2022  23043    smoorthy    Utilize latest DataTime when deciding to use the cache.
 * Oct 13, 2022  23218 mgamazaychikov Fixed the comparison to DateTime's Level default value
 * Oct 29, 2022  8959     mapeters    Update how data time levels are set
 * Dec 09, 2022  23218 mgamazaychikov Fixed the PDO time field level value and imlpemented update for AlertMessage
 * Mar 07, 2023  23414    dfriedman   Update DR 21057 logic to support derived products
 * Mar 09, 2023  23414 mgamazaychikov Fix the handling of centalWaveLength functionality
 * Jul 13, 2023 2035927 mgamazaychikov Fix setting the time level value of pdos in update, and
 *                                     provide level type in setLevel method
 * Jul 15, 2024  2037624  mapeters    Make invalidateAvailableTimesCache public
 *
 * </pre>
 *
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SatResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatResourceData.class);

    public SatelliteRecord[] records;

    @XmlAttribute
    private boolean showIncompleteFrames = true;

    private boolean isIncompleteFrameSelectableProduct = false;

    private Set<DataTime> previousTimes = new HashSet<>();

    private static Map<String, Pair<Long, DataTime[]>> satelliteTimeCache = new HashMap<>();

    private static long DEFAULT_CACHE_EXPIRATION = 60000;

    private DataTime latestDataTime;

    /**
     * Listening SatResource instances that need to know about raw data updates
     * to manage derived products.
     *
     * Reason for doing it this way - adding a resourceChanged handler that
     * unconditionally removes a DataTime will also be invoked by
     * D2DTimeMatcher.pruneUnusedData(), reverting the fix for DR 21057.
     */
    protected Set<SatResource> satResources = new CopyOnWriteArraySet<>();

    /*
     * (non-Javadoc)
     *
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.comm.LoadProperties,
     * com.raytheon.edex.db.objects.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new SatelliteRecord[objects.length];
        boolean cwf = metadataMap
                .containsKey(SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);
        // only augment record's DataTime field for this special case
        for (int i = 0; i < objects.length; i++) {
            records[i] = (SatelliteRecord) objects[i];
            if (cwf) {
                if (records[i].getCentralWavelength() != null) {
                    records[i].setDataTime(levelAugmentedDataTime(records[i]));
                }
            }
        }
        return new SatResource(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
        boolean issueAnUpdate = true;
        if (updateData instanceof PluginDataObject[]) {
            /*
             * This is here because derived updates will send us records that we
             * don't want, so filter them.
             */
            PluginDataObject[] pdos = (PluginDataObject[]) updateData;
            Set<PluginDataObject> pdosToFilterOut = new LinkedHashSet<>();

            Set<DataTime> invalidTimes = new HashSet<>();

            updateLatestDataTime(pdos);

            for (PluginDataObject pdo : (PluginDataObject[]) updateData) {
                try {
                    Map<String, Object> pdoMap = RecordFactory.getInstance()
                            .loadMapFromUri(pdo.getDataURI());
                    for (Entry<String, RequestConstraint> entry : metadataMap
                            .entrySet()) {
                        if (entry.getKey()
                                .equals(SatelliteDataCubeAdapter.DERIVED)) {
                            continue;
                        } else if (entry.getKey().equals(
                                SatelliteInventory.CENTRAL_WAVELENGTH_FIELD)) {
                            RequestConstraint rc = entry.getValue();
                            SatelliteRecord sr = (SatelliteRecord) pdo;
                            Number cwfValue = sr.getCentralWavelength();
                            if (!rc.evaluate(cwfValue)) {
                                pdosToFilterOut.add(pdo);
                                continue;
                            } else {
                                DataTime dtAugmented = levelAugmentedDataTime(
                                        sr);
                                pdo.setDataTime(dtAugmented);
                                continue;
                            }
                        }
                        Object pdoItem = pdoMap.get(entry.getKey());
                        RequestConstraint rc = entry.getValue();
                        /*
                         * Record Factory automatically replaces space with
                         * underscore, but some derived parameters have
                         * underscore in them
                         */
                        String pdoItemStr = pdoItem.toString().replace(" ",
                                "_");
                        if ((!rc.evaluate(pdoItem)
                                && !rc.evaluate(pdoItemStr))) {
                            DataTime time = pdo.getDataTime();
                            if (binOffset != null) {
                                time = binOffset.getNormalizedTime(time);
                            }
                            invalidTimes.add(time);
                            break;
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            if (!pdosToFilterOut.isEmpty()) {
                updateData = filterOutPdos((PluginDataObject[]) updateData,
                        pdosToFilterOut);
                PluginDataObject[] pdosCopy = (PluginDataObject[]) updateData;
                if (pdosCopy.length == 0) {
                    issueAnUpdate = false;
                }
            }

            if (!invalidTimes.isEmpty()) {

                /* Next time query should requery */
                super.invalidateAvailableTimesCache();
                /*
                 * Remove times from resources where there is new derived data.
                 */
                for (DataTime time : invalidTimes) {
                    fireChangeListeners(ChangeType.DATA_REMOVE, time);
                    for (SatResource res : satResources) {
                        res.dataTimeInvalidated(time);
                    }
                }
                /*
                 * Don't send updates for PDO's with invalidTimes, the time
                 * matcher will pull in all the records including derived
                 * records.
                 */
                List<PluginDataObject> pdoList = new ArrayList<>(
                        Arrays.asList(pdos));
                Iterator<PluginDataObject> it = pdoList.iterator();
                while (it.hasNext()) {
                    DataTime t = it.next().getDataTime();
                    if (binOffset != null) {
                        t = binOffset.getNormalizedTime(t);
                    }
                    if (invalidTimes.contains(t)) {
                        it.remove();
                    }
                }
                if (pdoList.isEmpty()) {
                    return;
                } else {

                    updateData = pdoList.toArray(new PluginDataObject[0]);

                }
            }
        }
        if (issueAnUpdate) {
            if (!isShowIncompleteFrames()
                    && (updateData instanceof PluginDataObject
                            || updateData instanceof PluginDataObject[])) {

                if (previousTimes != null) {
                    List<DataTime> oldTimes = Arrays
                            .asList(previousTimes.toArray(new DataTime[0]));
                    try {
                        super.invalidateAvailableTimesCache();
                        List<DataTime> newTimes = new ArrayList<>(Arrays.asList(
                                getAvailableTimes(metadataMap, binOffset)));

                        newTimes.removeAll(oldTimes);
                        if (newTimes.size() > 0) {
                            PluginDataObject[] objs = requestPluginDataObjects(
                                    newTimes);
                            List<PluginDataObject> newArray = new ArrayList<>();
                            if (objs != null && objs.length > 0) {
                                newArray.addAll(Arrays.asList(objs));
                            }

                            if (updateData instanceof PluginDataObject) {
                                newArray.add((PluginDataObject) updateData);
                                updateData = newArray
                                        .toArray(new PluginDataObject[0]);
                            } else if (updateData instanceof PluginDataObject[]) {
                                newArray.addAll(Arrays.asList(
                                        (PluginDataObject[]) updateData));
                                updateData = newArray
                                        .toArray(new PluginDataObject[0]);
                            }
                            invalidateAvailableTimesCache();
                        }

                    } catch (VizException e) {
                        statusHandler.error("Problem recalculating new frames",
                                e);
                    }
                }
            }
            Set<Integer> gids = new HashSet<>();
            for (SatelliteRecord record : records) {
                gids.add(record.getCoverage().getGid());
            }
            if (updateData instanceof SatelliteRecord) {
                gids.add(((SatelliteRecord) updateData).getCoverage().getGid());
            } else if (updateData instanceof PluginDataObject[]) {
                for (PluginDataObject obj : (PluginDataObject[]) updateData) {
                    gids.add(((SatelliteRecord) obj).getCoverage().getGid());
                }
            }
            if (gids.size() > 1) {
                isIncompleteFrameSelectableProduct = true;
            }
            super.update(updateData);
        }

    }

    @Override
    protected void update(AlertMessage... messages) {
        List<Object> objectsToSend = new ArrayList<>(messages.length);
        for (AlertMessage message : messages) {
            try {
                AbstractAlertMessageParser parser = getAlertParser();
                if (parser == null) {
                    parser = defaultParser;
                }
                Object objectToSend = parser.parseAlertMessage(message, this);
                if (objectToSend != null) {
                    objectsToSend.add(objectToSend);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error performing update: " + message.dataURI, e);
            }
        }

        if (!objectsToSend.isEmpty()) {
            Class<?> componentType = objectsToSend.get(0).getClass();
            update(objectsToSend.toArray((Object[]) Array
                    .newInstance(componentType, objectsToSend.size())));
        }
    }

    private Object filterOutPdos(PluginDataObject[] updateData,
            Set<PluginDataObject> pdosToFilterOutSet) {
        Set<PluginDataObject> updateDataSet = new LinkedHashSet<>(
                Arrays.asList(updateData));
        updateDataSet.removeAll(pdosToFilterOutSet);
        return updateDataSet.toArray(new PluginDataObject[0]);
    }

    /**
     * @return the records
     */
    public SatelliteRecord[] getRecords() {
        return records;
    }

    /**
     * @param records
     *            the records to set
     */
    public void setRecords(SatelliteRecord[] records) {
        this.records = records;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        /*
         * under some circumstances obj might consider itself equal to this, so
         * just let it decide.
         */
        if (obj instanceof SatBestResResourceData) {
            return obj.equals(this);
        }
        return super.equals(obj);
    }

    @Override
    protected DataTime[] getAvailableTimes(
            Map<String, RequestConstraint> constraintMap, BinOffset binOffset)
            throws VizException {

        /*
         * Builds cache key based on query
         *
         */
        String cacheKey = new HashSet<>(constraintMap.values()).toString();

        Pair<Long, DataTime[]> timeCache = satelliteTimeCache.get(cacheKey);
        if (timeCache == null) {
            timeCache = new Pair<>(0l, new DataTime[0]);
        }

        // extra flag to determine whether or not to use the cache
        boolean useCache = false;

        /*
         * Check if the latest DataTime that came in is more recent than the
         * latest Cache time. If it is, cancel use the cache by setting useCache
         * to 'false'.
         */
        DataTime latestCacheTime = getLatestDataTime(timeCache.getSecond());
        if (latestCacheTime == null || latestDataTime == null) {
            // anything null, don't cancel use of the cache
            useCache = true;
        } else {
            // true if latestCacheTime is >= latest time. Cached values are ok
            // in this respect. Don't cancel.
            useCache = !(latestDataTime.getRefTime()
                    .after(latestCacheTime.getRefTime()));
        }

        if (timeCache.getFirst() != 0l && System.currentTimeMillis()
                - timeCache.getFirst() > DEFAULT_CACHE_EXPIRATION) {
            invalidateAvailableTimesCache();
        } else if ((timeCache.getFirst() != 0l) && (timeCache != null)
                && (timeCache.getSecond() != null) && useCache) {
            return timeCache.getSecond();
        }

        synchronized (satelliteTimeCache) {

            DbQueryRequest request = new DbQueryRequest(constraintMap);
            request.addRequestField(PluginDataObject.DATATIME_ID);
            /*
             * Don't actually need gid, just need to add it along with distinct
             * so that each time is repeated based off how many distinct gids it
             * has.
             */
            request.addRequestField("coverage.gid");
            request.setDistinct(true);
            // augment datatime request for central wavelength requests
            if (constraintMap
                    .containsKey(SatelliteInventory.CENTRAL_WAVELENGTH_FIELD)) {
                request.addRequestField(
                        SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);
            }
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            Map<DataTime, Integer> gidCounts = new HashMap<>();
            DataTime[] times = null;
            // augment datatime response for central wavelength requests
            if (constraintMap
                    .containsKey(SatelliteInventory.CENTRAL_WAVELENGTH_FIELD)) {
                List<DataTime> dtal = new ArrayList<>();
                for (Map<String, Object> map : response.getResults()) {
                    DataTime dtaug;
                    dtaug = (DataTime) map.get(PluginDataObject.DATATIME_ID);
                    Number cwl = (Number) map
                            .get(SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);
                    if (cwl != null) {
                        dtaug.setLevel(cwl.doubleValue(),
                                SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);
                    }
                    dtal.add(dtaug);
                }
                times = dtal.toArray(new DataTime[0]);
            } else {
                times = response.getFieldObjects(PluginDataObject.DATATIME_ID,
                        DataTime.class);
            }

            int max = 1;
            for (DataTime time : times) {
                Integer gidCount = gidCounts.get(time);
                if (gidCount == null) {
                    gidCounts.put(time, 1);
                } else {
                    gidCounts.put(time, gidCount + 1);
                    if (gidCount.intValue() == max) {
                        max += 1;
                    }
                }
            }
            if (max > 1) {
                isIncompleteFrameSelectableProduct = true;
            } else {
                // Hide the incomplete frames menu item
                isIncompleteFrameSelectableProduct = false;
            }
            if (showIncompleteFrames) {
                timeCache.setSecond(
                        super.getAvailableTimes(constraintMap, binOffset));
            } else {
                invalidateAvailableTimesCache(timeCache);
                Set<DataTime> uniqueTimes = new HashSet<>(gidCounts.size(),
                        1.0f);
                for (Entry<DataTime, Integer> countEntry : gidCounts
                        .entrySet()) {
                    if (countEntry.getValue().intValue() == max) {
                        DataTime time = countEntry.getKey();
                        if (binOffset != null) {
                            time = binOffset.getNormalizedTime(time);
                        }
                        uniqueTimes.add(time);
                    }
                }
                previousTimes = uniqueTimes;
                timeCache.setSecond(uniqueTimes.toArray(new DataTime[0]));
            }
            timeCache.setFirst(System.currentTimeMillis());
            satelliteTimeCache.put(cacheKey, timeCache);

            return timeCache.getSecond();
        }
    }

    public boolean isShowIncompleteFrames() {
        return showIncompleteFrames;
    }

    public void setShowIncompleteFrames(boolean showIncompleteFrames) {
        boolean previousValue = this.showIncompleteFrames;

        this.showIncompleteFrames = showIncompleteFrames;
        if (previousValue != this.showIncompleteFrames) {
            invalidateAvailableTimesCache();
            try {
                this.getAvailableTimes(metadataMap, binOffset);
            } catch (VizException e) {
                // Ignore error
            }
        }

    }

    public boolean isIncompleteFrameSelectableProduct() {

        return isIncompleteFrameSelectableProduct;
    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        PluginDataObject[] pdos = super.getLatestPluginDataObjects(desired,
                current);

        boolean cwf = metadataMap
                .containsKey(SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);

        // augment PDO's DataTime field for this special case
        if (cwf) {
            for (PluginDataObject pdo : pdos) {
                SatelliteRecord sr = (SatelliteRecord) pdo;
                if (sr.getCentralWavelength() != null) {
                    sr.setDataTime(levelAugmentedDataTime(sr));
                }
                pdo.setDataTime(sr.getDataTime());
            }
            List<PluginDataObject> pdoList = Arrays.asList(pdos);
            Collections.sort(pdoList,
                    (o1, o2) -> o2.getDataTime().getLevelValue()
                            .compareTo(o1.getDataTime().getLevelValue()));
            return pdoList.toArray(new PluginDataObject[0]);
        } else {
            return pdos;
        }
    }

    private DataTime levelAugmentedDataTime(SatelliteRecord record) {
        DataTime dt = record.getDataTime();
        Float cwl = record.getCentralWavelength();
        if (cwl == null) {
            return dt;
        }
        Double level = record.getCentralWavelength().doubleValue();
        if (level != null) {
            // Using Level.INVALID_VALUE constant here since DataTime default
            // value
            // is modeled after it. In the future will need to do it explicitly.
            if (level != Level.INVALID_VALUE) {
                dt = dt.clone();
                dt.setLevel(level, SatelliteInventory.CENTRAL_WAVELENGTH_FIELD);
            }
        }
        return dt;
    }

    @Override
    public void invalidateAvailableTimesCache() {
        synchronized (satelliteTimeCache) {
            satelliteTimeCache.clear();
        }
    }

    protected void invalidateAvailableTimesCache(
            Pair<Long, DataTime[]> timeCache) {
        synchronized (timeCache) {
            timeCache = new Pair<>(0l, new DataTime[0]);
        }

    }

    private void updateLatestDataTime(PluginDataObject[] times) {
        /* find the latest datatime and store it */

        if (times == null || times.length == 0) {
            return;
        }

        DataTime latest = times[0].getDataTime();
        for (PluginDataObject pdo : times) {
            DataTime curr = pdo.getDataTime();
            if (curr.getRefTime().after(latest.getRefTime())) {
                latest = curr;
            }
        }

        if (latestDataTime == null) {
            latestDataTime = latest;
        } else {
            if (this.latestDataTime.getRefTime().before(latest.getRefTime())) {
                this.latestDataTime = latest;
            }
        }
    }

    private DataTime getLatestDataTime(DataTime[] times) {

        if (times == null || times.length == 0) {
            return null;
        }
        DataTime latest = times[0];
        for (DataTime dt : times) {
            if (dt.getRefTime().after(latest.getRefTime())) {
                latest = dt;
            }
        }

        return latest;
    }

    /**
     * Add a SatResource to the list of resources to be notified of invalidated
     * derived parameter data times.
     */
    public void addSatResource(SatResource res) {
        satResources.add(res);
    }

    /**
     * Remove a SatResource from the list of resources to be notified of
     * invalidated derived product data times.
     */
    public void removeSatResource(SatResource res) {
        satResources.remove(res);
    }

}
