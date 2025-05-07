package com.raytheon.uf.viz.grid.radar;

import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridCacheUpdater;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * Listens for updates to radar products and transforms them into grid updates
 * so that radar data being used in grid derived parameters will update.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Sep 20, 2012           bsteffen   Initial creation
 * Aug 30, 2013  2298     rjpeter    Make getPluginName abstract
 * Feb 21, 2014  16744    dfriedman  Support thin client updates
 * Sep 09, 2014  3356     njensen    Remove CommunicationException
 * Aug 15, 2017  6332     bsteffen   Move to viz.grid.radar plugin
 * Jul 07, 2021  8576     randerso   Changed RadarAdapter to support multiple
 *                                   local radars as defined in radarsInUse.txt
 * Jul 26, 2021  8600     randerso   Send converted DataURIs to GridCacheUpdater.
 * Jan 09, 2024  2036695  mapeters   Support multiple params using the same
 *                                   product code (for SRM/V)
 * May 22, 2024  2037092  mapeters   Add icao and virtual volume to cache key, clear cache
 *                                   when virtual volumes are enabled/disabled, move
 *                                   instance into LazyHolder
 * Sep 16, 2024  2037941  mapeters   Virtual availability is no longer cached
 * Oct 14, 2024  2037939  mapeters   Replace globalTimes with icao->times cache to make it
 *                                   work for the radar->radar-kxxx changes done under #8576
 *                                   and for the new TiltTemporalGridDataLevelNode
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarUpdater implements IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarUpdater.class);

    protected static final int CACHE_SIZE = 100;

    protected static final int CACHE_TIME = 60_000;

    /**
     * Initialization-on-demand holder to prevent instantiation until
     * getInstance() is actually called. Specifically needed for unit tests to
     * be able to mock getInstance().
     */
    private static class LazyHolder {
        private static final RadarUpdater instance = new RadarUpdater();
    }

    public static RadarUpdater getInstance() {
        return LazyHolder.instance;
    }

    protected static class CacheKey {

        public final String icao;

        public final Integer productCode;

        public final Double elevationAngle;

        public final boolean normalScansOnly;

        public CacheKey(String icao, Integer productCode, Double elevationAngle,
                boolean normalScansOnly) {
            this.icao = icao;
            this.elevationAngle = elevationAngle;
            this.productCode = productCode;
            this.normalScansOnly = normalScansOnly;
        }

        @Override
        public int hashCode() {
            return Objects.hash(elevationAngle, icao, normalScansOnly,
                    productCode);
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            CacheKey other = (CacheKey) obj;
            return Objects.equals(elevationAngle, other.elevationAngle)
                    && Objects.equals(icao, other.icao)
                    && normalScansOnly == other.normalScansOnly
                    && Objects.equals(productCode, other.productCode);
        }
    }

    protected static class TimeAndSpaceCacheEntry {

        public final long insertTime;

        public final Set<TimeAndSpace> times;

        public TimeAndSpaceCacheEntry(Set<TimeAndSpace> times) {
            this.insertTime = Instant.now().toEpochMilli();
            this.times = Collections.unmodifiableSet(times);
        }
    }

    protected static class TimeCacheEntry {

        public final long insertTime;

        public final Set<DataTime> times;

        public TimeCacheEntry(Collection<DataTime> times) {
            this.insertTime = Instant.now().toEpochMilli();
            this.times = new CopyOnWriteArraySet<>(times);
        }
    }

    protected final Map<CacheKey, TimeAndSpaceCacheEntry> cache = new LinkedHashMap<CacheKey, TimeAndSpaceCacheEntry>(
            100, .75f, true) {

        private static final long serialVersionUID = 2022670836957170184L;

        @Override
        protected boolean removeEldestEntry(
                Entry<CacheKey, TimeAndSpaceCacheEntry> eldest) {
            return this.size() > CACHE_SIZE;
        }
    };

    protected final Map<String, TimeCacheEntry> stationTimesCache = new HashMap<>();

    private RadarUpdater() {
        ProductAlertObserver.addObserver(RadarAdapter.RADAR_SOURCE, this);
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        Set<String> dataURIs = convertRadarAlertsToGridDatauris(alertMessages);
        ProductAlertObserver.processDataURIAlerts(dataURIs);

        GridCacheUpdater gridCacheUpdater = GridCacheUpdater.getInstance();
        DataURINotificationMessage message = new DataURINotificationMessage();
        message.setDataURIs(dataURIs.toArray(new String[0]));
        try {
            gridCacheUpdater.handleDataURINotificationMessage(message);
        } catch (NotificationException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    public Set<String> convertRadarAlertsToGridDatauris(
            Collection<AlertMessage> alertMessages) {
        Set<String> configuredRadars = RadarAdapter.getInstance()
                .getConfiguredRadars();
        if (configuredRadars.isEmpty()) {
            return new HashSet<>();
        }
        Set<String> dataUris = new HashSet<>();
        for (AlertMessage alertMessage : alertMessages) {
            String icao = alertMessage.decodedAlert.get(RadarAdapter.ICAO_QUERY)
                    .toString();
            if ((icao == null) || !configuredRadars.contains(icao)) {
                continue;
            }
            Object obj = alertMessage.decodedAlert
                    .get(PluginDataObject.DATATIME_ID);
            if (!(obj instanceof DataTime)) {
                continue;
            }
            DataTime time = (DataTime) obj;

            TimeCacheEntry icaoTimes = stationTimesCache.get(icao);
            if (icaoTimes != null) {
                icaoTimes.times.add(time);
            }

            obj = alertMessage.decodedAlert
                    .get(RadarAdapter.PRODUCT_CODE_QUERY);
            if (!(obj instanceof Integer)) {
                continue;
            }
            Integer productCode = (Integer) obj;
            Set<String> paramAbbrevs = RadarProductCodeMapping.getInstance()
                    .getParameterAbbrevsForProductCode(productCode);
            if (CollectionUtils.isEmpty(paramAbbrevs)) {
                continue;
            }
            paramAbbrevs = RadarAsGridUtil
                    .addVirtualVolumeParamAbbrevs(paramAbbrevs);
            obj = alertMessage.decodedAlert.get(RadarAdapter.TILT_QUERY);
            if (!(obj instanceof Double)) {
                continue;
            }
            Double elevationAngle = (Double) obj;
            cache.remove(new CacheKey(icao, productCode, elevationAngle, true));
            cache.remove(
                    new CacheKey(icao, productCode, elevationAngle, false));
            Level level = LevelFactory.getInstance().getLevel(
                    RadarAdapter.CUBE_MASTER_LEVEL_NAME, elevationAngle);

            for (String paramAbbrev : paramAbbrevs) {
                GridRecord fakeRec = new GridRecord();

                fakeRec.setDataTime(time);
                fakeRec.setDatasetId(RadarAsGridUtil.getModelNameForIcao(icao));
                Parameter param = new Parameter(paramAbbrev);
                fakeRec.setParameter(param);
                fakeRec.setLevel(level);
                dataUris.add(fakeRec.getDataURI());
            }
        }
        return dataUris;
    }

    protected CacheKey getCacheKey(RadarRequestableLevelNode rNode) {
        /*
         * Virtual volume nodes don't cache virtual availability but still need
         * separate cache keys because they only use normal scan types (not
         * SAILS/MRLE).
         */
        boolean normalScansOnly = rNode.isVirtualVolume();
        return new CacheKey(rNode.getIcao(), rNode.getProductCode(),
                rNode.getTilt(), normalScansOnly);
    }

    public void setTimes(RadarRequestableLevelNode rNode,
            Set<TimeAndSpace> times) {
        cache.put(getCacheKey(rNode), new TimeAndSpaceCacheEntry(times));
    }

    public Set<TimeAndSpace> getTimes(RadarRequestableLevelNode rNode) {
        CacheKey cacheKey = getCacheKey(rNode);
        TimeAndSpaceCacheEntry entry = cache.get(cacheKey);
        if (entry == null) {
            return null;
        }
        if ((entry.insertTime + CACHE_TIME) < Instant.now().toEpochMilli()) {
            cache.remove(cacheKey);
            return null;
        }
        return entry.times;
    }

    /**
     * Get all volume scan times for the radar station with the given icao.
     *
     * @param icao
     *            radar station icao
     * @return all radar station scan times
     */
    public Set<DataTime> getStationTimes(String icao) {
        TimeCacheEntry entry = stationTimesCache.get(icao);
        if (entry != null) {
            if (entry.insertTime + CACHE_TIME >= Instant.now().toEpochMilli()) {
                return Collections.unmodifiableSet(entry.times);
            }
        }

        Map<String, RequestConstraint> newQuery = new HashMap<>();
        newQuery.put(RadarAdapter.PLUGIN_NAME_QUERY,
                new RequestConstraint(RadarAdapter.RADAR_SOURCE));
        newQuery.put(RadarAdapter.ICAO_QUERY, new RequestConstraint(icao));

        DataTime[] times = null;
        try {
            times = CatalogQuery.performTimeQuery(newQuery, false, null);
        } catch (VizException e) {
            statusHandler.error("Error querying volume scan times for " + icao,
                    e);
        }

        Set<DataTime> rval = Set.of();
        if (times != null) {
            TimeCacheEntry cacheEntry = new TimeCacheEntry(
                    Arrays.asList(times));
            stationTimesCache.put(icao, cacheEntry);
            rval = cacheEntry.times;
        }

        return Collections.unmodifiableSet(rval);
    }

    public void clearCache() {
        cache.clear();
        stationTimesCache.clear();
    }
}
