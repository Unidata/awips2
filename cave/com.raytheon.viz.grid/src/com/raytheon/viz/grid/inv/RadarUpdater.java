package com.raytheon.viz.grid.inv;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.grid.util.RadarAdapter;
import com.raytheon.viz.grid.util.RadarProductCodeMapping;

/**
 * 
 * Listens for updates to radatr products and transforms them into grid updates
 * so that radar data being used in grid derived parameters will update.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarUpdater implements IAlertObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarUpdater.class);

    protected static final int CACHE_SIZE = 100;

    protected static final int CACHE_TIME = 60000;

    private static RadarUpdater instance;

    public static RadarUpdater getInstance() {
        if (instance == null) {
            instance = new RadarUpdater();
        }
        return instance;
    }

    private class CacheKey {

        public CacheKey(Integer productCode, Double elevationAngle) {
            this.elevationAngle = elevationAngle;
            this.productCode = productCode;
        }

        public Integer productCode;

        public Double elevationAngle;

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime
                    * result
                    + ((elevationAngle == null) ? 0 : elevationAngle.hashCode());
            result = prime * result
                    + ((productCode == null) ? 0 : productCode.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (elevationAngle == null) {
                if (other.elevationAngle != null)
                    return false;
            } else if (!elevationAngle.equals(other.elevationAngle))
                return false;
            if (productCode == null) {
                if (other.productCode != null)
                    return false;
            } else if (!productCode.equals(other.productCode))
                return false;
            return true;
        }

    }

    private class CacheEntry {

        public CacheEntry(Set<TimeAndSpace> times) {
            this.insertTime = System.currentTimeMillis();
            this.times = times;
        }

        public long insertTime;

        public Set<TimeAndSpace> times;

    }

    private Map<CacheKey, CacheEntry> cache = new LinkedHashMap<CacheKey, CacheEntry>(
            100, .75f, true) {

        private static final long serialVersionUID = 2022670836957170184L;

        @Override
        protected boolean removeEldestEntry(Entry<CacheKey, CacheEntry> eldest) {
            return this.size() > CACHE_SIZE;
        }
    };

    private Set<DataTime> globalTimes;

    private long globalInsertTime;

    private RadarUpdater() {
        ProductAlertObserver.addObserver("radar", this);
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        RadarStation configuredRadar = RadarAdapter.getInstance()
                .getConfiguredRadar();
        if (configuredRadar == null) {
            return;
        }
        Set<String> datauris = new HashSet<String>();
        for (AlertMessage alertMessage : alertMessages) {
            String icao = alertMessage.decodedAlert
                    .get(RadarAdapter.ICAO_QUERY).toString();
            if (icao == null
                    || !icao.equalsIgnoreCase(configuredRadar.getRdaId())) {
                continue;
            }
            globalTimes = null;
            Object obj = alertMessage.decodedAlert
                    .get(RadarAdapter.PRODUCT_CODE_QUERY);
            if (obj == null || !(obj instanceof Integer)) {
                continue;
            }
            Integer productCode = (Integer) obj;
            String paramAbbrev = RadarProductCodeMapping.getInstance()
                    .getParameterAbbrev(productCode);
            if (paramAbbrev == null) {
                continue;
            }
            obj = alertMessage.decodedAlert.get("dataTime");
            if (obj == null || !(obj instanceof DataTime)) {
                continue;
            }
            DataTime time = (DataTime) obj;
            obj = alertMessage.decodedAlert.get(RadarAdapter.TILT_QUERY);
            if (obj == null || !(obj instanceof Double)) {
                continue;
            }
            Double elevationAngle = (Double) obj;
            cache.remove(new CacheKey(productCode, elevationAngle));
            Level level = null;
            try {
                level = LevelFactory.getInstance().getLevel(
                        RadarAdapter.CUBE_MASTER_LEVEL_NAME, elevationAngle);
            } catch (CommunicationException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        e1.getLocalizedMessage(), e1);
            }
            GridRecord fakeRec = new GridRecord();
            fakeRec.setPluginName(GridInventory.PLUGIN_NAME);

            fakeRec.setDataTime(time);
            fakeRec.setDatasetId(RadarAdapter.RADAR_SOURCE);
            Parameter param = new Parameter(paramAbbrev);
            fakeRec.setParameter(param);
            fakeRec.setLevel(level);
            try {
                fakeRec.constructDataURI();
                datauris.add(fakeRec.getDataURI());
            } catch (PluginException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to generate updates for derived product", e);
            }
        }
        ProductAlertObserver.processDerivedAlerts(datauris);
    }

    private CacheKey getCacheKey(RadarRequestableLevelNode rNode) {
        Map<String, RequestConstraint> rcMap = rNode.getRequestConstraintMap();
        RequestConstraint rc = rcMap.get(RadarAdapter.PRODUCT_CODE_QUERY);
        if (rc == null || rc.getConstraintType() != ConstraintType.EQUALS) {
            return null;
        }
        Integer productCode = Integer.parseInt(rc.getConstraintValue());
        rc = rcMap.get(RadarAdapter.TILT_QUERY);
        if (rc == null || rc.getConstraintType() != ConstraintType.EQUALS) {
            return null;
        }
        Double elevationAngle = Double.parseDouble(rc.getConstraintValue());
        return new CacheKey(productCode, elevationAngle);
    }

    public void setTimes(RadarRequestableLevelNode rNode,
            Set<TimeAndSpace> times) {
        cache.put(getCacheKey(rNode), new CacheEntry(times));
    }

    public Set<TimeAndSpace> getTimes(RadarRequestableLevelNode rNode) {
        CacheKey cacheKey = getCacheKey(rNode);
        CacheEntry entry = cache.get(cacheKey);
        if (entry == null) {
            return null;
        }
        if (entry.insertTime + CACHE_TIME < System.currentTimeMillis()) {
            cache.remove(cacheKey);
            return null;
        }
        return entry.times;
    }

    public void setGlobalTimes(Set<DataTime> times) {
        globalTimes = times;
        globalInsertTime = System.currentTimeMillis();
    }

    public Set<DataTime> getGlobalTimes() {
        if (globalTimes == null) {
            return null;
        }
        if (globalInsertTime + CACHE_TIME < System.currentTimeMillis()) {
            globalTimes = null;
            return null;
        }
        return globalTimes;
    }

    public void clearCache() {
        cache.clear();
        globalTimes = null;
    }

}
