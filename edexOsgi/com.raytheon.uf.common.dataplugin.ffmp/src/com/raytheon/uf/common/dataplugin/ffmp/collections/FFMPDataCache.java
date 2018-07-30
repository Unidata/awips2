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
 * */

package com.raytheon.uf.common.dataplugin.ffmp.collections;

import java.io.File;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentNavigableMap;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPAggregateRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates.MODE;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.GUIDANCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * FFMP Common Data Cache
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Oct 26, 2015  5056       D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */


public class FFMPDataCache {
    
    
    /** Soft reference wrapper for data cache **/
    private static SoftReference<FFMPDataCache> softCache = new SoftReference<FFMPDataCache>(null);
    
    /** FFMP Source configuration manager **/
    private FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager.getInstance();
    
    /** FFMP Running(Comparison) configuration manager **/
    private FFMPRunConfigurationManager frcm = FFMPRunConfigurationManager.getInstance();
    
    /** The FFMP templates **/
    private volatile FFMPTemplates templates;
    
    /** FFMP Cache Data Container **/
    private final FFMPSiteDataContainer siteDataMap = new FFMPSiteDataContainer();
    
    /** Local lock object for URI requests **/
    private final Object uriRequestLock = new Object();
    
    /** WFO this cache is active for **/
    private final String wfo;
    
    /** Status handler **/
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataCache.class);
    
   /**
     * Singleton instance
     * 
     * @param wfo
     * 
     * @return
     */
    public static synchronized FFMPDataCache getInstance(String wfo) {

        FFMPDataCache cache = softCache.get();
        
        if (cache == null ||!cache.wfo.equals(wfo)) {
            cache = new FFMPDataCache(wfo);
            softCache = new SoftReference<FFMPDataCache>(cache);
        }

        return cache;
    }

    /**
     * private instance constructor
     * 
     * @param wfo
     */
    private FFMPDataCache(String wfo) {
        this.wfo = wfo;
    }
    
    /**
     * Gets the HUC templates
     * 
     * @param siteKey
     * @return
     */
    public FFMPTemplates getTemplates(String siteKey) {
        
        if (templates == null) {
            long t0 = System.currentTimeMillis();
            synchronized (this) {
                if (templates == null) {
                    FFMPRunXML runner = frcm.getRunner(wfo);
                    templates = FFMPTemplates.getInstance(
                            runner.getPrimaryDomain(), siteKey, MODE.CAVE);

                    // backup domains
                    if (runner.getBackupDomains() != null) {
                        for (DomainXML backup : runner.getBackupDomains()) {
                            templates.addDomain(siteKey, backup);
                        }
                    } else {
                        templates.done = true;
                    }
                }
            }
            statusHandler.info("Time spent initializing templates: "
                    + (System.currentTimeMillis() - t0));
        }

        if (!templates.isSiteLoaded(siteKey)) {
            FFMPRunXML runner = frcm.getRunner(wfo);

            for (DomainXML domain : runner.getDomains()) {
                templates.addDomain(siteKey, domain);
            }
        }

        return templates;
    }
    
    /**
     * populate a new FFMPRecord
     * 
     * @param uri
     * @param siteKey
     * @param source
     * @return
     * @throws Exception
     */
    public FFMPRecord populateFFMPRecord(String uri, String siteKey,
            String source) throws Exception {

        populateFFMPRecord(siteKey, new FFMPRecord(uri), source);
        SourceXML sourceXml = fscm.getSource(source);
        
        if (sourceXml.getSourceType().equals(
                SOURCE_TYPE.GUIDANCE.getSourceType())) {
            source = sourceXml.getDisplayName();
        }
        
        return siteDataMap.get(siteKey).getSourceData(source).getRecord(uri);
    }
    
    /**
     * populate a new FFMPRecord
     * 
     * @param siteKey
     * @param dataKey
     * @param source
     * @param ptime
     * @param retrieveNew
     * @throws Exception
     */
    public void populateFFMPRecord(String siteKey, String dataKey,
            String source, Date ptime, boolean retrieveNew) throws Exception {
        if (source != null) {

            boolean dupOverride = false;
            if (fscm.getSource(source).getSourceType()
                    .equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
                dupOverride = true;
            }

            SortedMap<Date, List<String>> urisByDate = getAvailableUris(
                    siteKey, dataKey, source, ptime, retrieveNew);

            if (urisByDate != null) {
                for (List<String> uris : urisByDate.values()) {
                    for (String uri : uris) {
                        if (uri != null) {
                            if (dupOverride
                                    || !getLoadedUris(siteKey, source)
                                            .contains(uri)) {
                                populateFFMPRecord(siteKey,
                                        new FFMPRecord(uri), source);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Inserts the loader records directly into the cache
     * 
     * 
     * @param data
     * @param uris
     * @param siteKey
     * @param source
     */
    public void insertFFMPData(FFMPAggregateRecord data,
            NavigableMap<Date, List<String>> uris, String siteKey, String source) {

        // get record from cache
        FFMPSourceData sourceData = siteDataMap.get(siteKey).getSourceData(
                source);

        // add all of the uris
        for (Entry<Date, List<String>> duris : uris.entrySet()) {
            if (data.getTimes().contains(duris.getKey().getTime())) {
                for (String uri : duris.getValue()) {
                    sourceData.getRecord(uri);
                    if (!sourceData.getLoadedUris().contains(uri)) {
                        sourceData.addLoadedUri(uri);
                    }
                }
            }
        }

        if (sourceData.getRecord() != null) {
            FFMPBasinData basinData = data.getBasins();
            basinData.populate(data.getTimes());
            sourceData.getRecord().populate(basinData);
        }
    }

    /**
     * Try and get the loading off of the GUI thread
     * 
     * @param siteKey
     * @param ffmpRec
     * @param source
     * 
     * @throws Exception
     */
    public void populateFFMPRecord(String siteKey, FFMPRecord ffmpRec,
            String source) throws Exception {

        if (ffmpRec != null) {

            List<String> uris = getLoadedUris(siteKey, source);
            String dataUri = ffmpRec.getDataURI();
            if (!uris.contains(dataUri)) {
                Date refTime = ffmpRec.getDataTime().getRefTime();
                File loc = HDF5Util.findHDF5Location(ffmpRec);

                FFMPSiteData siteData = siteDataMap.get(siteKey);
                String mySource = source;
                boolean isGageSource = false;
                SourceXML sourceXML = fscm.getSource(source);

                if (sourceXML.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    mySource = sourceXML.getDisplayName();
                } else if (sourceXML.getSourceType().equals(
                        SOURCE_TYPE.GAGE.getSourceType())) {
                    isGageSource = true;
                }

                FFMPSourceData sourceData = siteData
                        .getSourceData(mySource);
                FFMPRecord curRecord = sourceData.getRecord(dataUri);

                if (isGageSource) {
                    curRecord.retrieveVirtualMapFromDataStore(loc, dataUri,
                            getTemplates(siteKey), refTime,
                            ffmpRec.getSourceName());
                } else {
                    curRecord.retrieveMapFromDataStore(loc, dataUri,
                            getTemplates(ffmpRec.getSiteKey()), refTime,
                            ffmpRec.getSourceName());
                }

                sourceData.addLoadedUri(dataUri);
            }
        }
    }
       

    /**
     * Load data for a particular basin
     * 
     * @param dataUri
     * @param siteKey
     * @param source
     * @param phuc
     * @param basin
     * @throws Exception
     */
    public void populateFFMPBasin(String dataUri, String siteKey,
            String source, String phuc, FFMPBasin basin) throws Exception {

        if (dataUri != null) {
            List<String> uris = getLoadedUris(siteKey, source);
            if (!uris.contains(dataUri)) {

                SourceXML sourceXML = fscm.getSource(source);
                FFMPRecord ffmpRec = populateFFMPRecord(dataUri, siteKey,
                        source);
                File loc = HDF5Util.findHDF5Location(ffmpRec);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);

                if (sourceXML.getSourceType().equals(
                        SOURCE_TYPE.GAGE.getSourceType())
                        && phuc.equals(FFMPRecord.ALL)) {
                    ffmpRec.retrieveVirtualBasinFromDataStore(loc, dataUri,
                            getTemplates(siteKey), ffmpRec.getDataTime()
                                    .getRefTime(), basin);
                } else {
                    ffmpRec.retrieveBasinFromDataStore(dataStore, dataUri,
                            getTemplates(siteKey), phuc, ffmpRec
                                    .getDataTime().getRefTime(), ffmpRec
                                    .getSourceName(), basin);
                }
            }
        }
    }

    /**
     * gets the URI's by field type and site
     * 
     * @param siteKey
     * @param pfield
     * @return
     */

    public List<String> getLoadedUris(String siteKey, String source) {
        FFMPSiteData siteData = siteDataMap.get(siteKey);
        FFMPSourceData sourceData = siteData.getSourceData(source);
        return sourceData.getLoadedUris();
    }
    
    /**
     * Gets the available uris back to a given time
     * 
     * @param siteKey
     * @param dataKey
     * @param sourceName
     * @param time
     * @param retrieveNew
     * @return
     * @throws Exception 
     */
    public ConcurrentNavigableMap<Date, List<String>> getAvailableUris(
            String siteKey, String dataKey, String sourceName, Date time,
            boolean retrieveNew) throws Exception {
        synchronized (uriRequestLock) {
            ConcurrentNavigableMap<Date, List<String>> sortedUris = siteDataMap
                    .get(siteKey).getSourceData(sourceName).getAvailableUris();
            Date previousQueryTime = siteDataMap.get(siteKey)
                    .getSourceData(sourceName).getPreviousUriQueryDate();
            Date earliestTime = time;

            SourceXML source = fscm.getSource(sourceName);
            boolean isTimeConstraint = true;

            if (source.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                /**
                 * Always look back for guidance types because of long
                 * expiration times, prevents mosaic brittleness from occurring.
                 */
                retrieveNew = true;

                if (source.getGuidanceType() != null
                        && source.getGuidanceType().equals(
                                GUIDANCE_TYPE.ARCHIVE.getGuidanceType())) {
                    isTimeConstraint = false;
                } else {
                    long timeOffset = source.getExpirationMinutes(siteKey)
                            * TimeUtil.MILLIS_PER_MINUTE;
                    earliestTime = new Date(time.getTime() - timeOffset);
                }
            }

            if (retrieveNew
                    || (time != null && (previousQueryTime == null || time
                            .before(previousQueryTime)))) {
                DbQueryRequest request = new DbQueryRequest();
                request.setEntityClass(FFMPRecord.class);
                request.addRequestField("dataURI");
                request.setOrderByField("dataTime.refTime", OrderMode.DESC);

                request.addConstraint("wfo", new RequestConstraint(wfo));
                request.addConstraint("sourceName", new RequestConstraint(
                        sourceName));
                request.addConstraint("siteKey", new RequestConstraint(siteKey));
                if (!source.isMosaic()) {
                    request.addConstraint("dataKey", new RequestConstraint(
                            dataKey));

                }

                // ARCHIVE sources don't have a time constraint
                if (isTimeConstraint) {
                    String earliestTimeString = TimeUtil
                            .formatToSqlTimestamp(earliestTime);

                    if (!retrieveNew && (previousQueryTime != null)) {
                        String latestTimeString = TimeUtil
                                .formatToSqlTimestamp(previousQueryTime);
                        RequestConstraint timeRC = new RequestConstraint(null,
                                ConstraintType.BETWEEN);
                        timeRC.setBetweenValueList(new String[] {
                                earliestTimeString, latestTimeString });
                        request.addConstraint("dataTime.refTime", timeRC);
                    } else {
                        request.addConstraint("dataTime.refTime",
                                new RequestConstraint(earliestTimeString,
                                        ConstraintType.GREATER_THAN_EQUALS));
                    }
                }

                handleURIRequest(request, siteKey, dataKey, time);

            }

            if (time != null) {
                if (source.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    return sortedUris;
                } else {
                    return sortedUris.tailMap(time, true);
                }
            }
        }

        return null;
    }

    /**
     * Perform a single database request to populate the availableUris for
     * multiple sources. After preloading the uris the uris for each source can
     * be retrieved with getAvailableUris
     * 
     * @param siteKey
     * @param dataKey
     * @param sourceNames
     * @param time
     * @throws Exception 
     */
    public void preloadAvailableUris(String siteKey, String dataKey,
            Set<String> sourceNames, Date time) throws Exception {
        synchronized (uriRequestLock) {
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(FFMPRecord.class);
            request.addRequestField("dataURI");
            request.setOrderByField("dataTime.refTime", OrderMode.DESC);

            request.addConstraint("wfo", new RequestConstraint(wfo));
            request.addConstraint("siteKey", new RequestConstraint(siteKey));
            request.addConstraint("dataTime.refTime", new RequestConstraint(
                    TimeUtil.formatToSqlTimestamp(time),
                    ConstraintType.GREATER_THAN_EQUALS));

            RequestConstraint sourceRC = new RequestConstraint(null,
                    ConstraintType.IN);
            sourceRC.setConstraintValueList(sourceNames);
            request.addConstraint("sourceName", sourceRC);

            handleURIRequest(request, siteKey, dataKey, time);
            FFMPSiteData siteData = siteDataMap.get(siteKey);
            for (String sourceName : sourceNames) {
                // This is done to ensure that the previous query time is
                // updated, even for sources with no data.
                FFMPSourceData sourceData = siteData.getSourceData(sourceName);
                Date oldPrevTime = sourceData.getPreviousUriQueryDate();
                if (oldPrevTime == null || time.before(oldPrevTime)) {
                    sourceData.setPreviousUriQueryDate(time);
                }
            }
        }
    }

    /**
     * Handle a pre assembled database query for uris. The request is sent to
     * edex and the response is parsed to populate the uris in the siteDataMap.
     * 
     * @param request
     * @param siteKey
     * @param dataKey
     * @param time
     * @throws Exception
     */
    private void handleURIRequest(DbQueryRequest request, String siteKey,
            String dataKey, Date time) throws Exception {
        
        FFMPSiteData siteData = siteDataMap.get(siteKey);
        DbQueryResponse dbResponse = (DbQueryResponse) RequestRouter
                .route(request);
        Map<String, List<FFMPRecord>> recordsBySource = new HashMap<String, List<FFMPRecord>>();

        for (String datauri : dbResponse.getFieldObjects("dataURI",
                String.class)) {
            FFMPRecord record = new FFMPRecord(datauri);
            List<FFMPRecord> records = recordsBySource.get(record
                    .getSourceName());
            if (records == null) {
                records = new ArrayList<FFMPRecord>();
                recordsBySource.put(record.getSourceName(), records);
            }
            records.add(record);
        }
        for (Entry<String, List<FFMPRecord>> entry : recordsBySource.entrySet()) {
            String sourceName = entry.getKey();
            SourceXML sourceXml = fscm.getSource(sourceName);
            boolean isMosiac = sourceXml.isMosaic();
            FFMPSourceData sourceData = siteData.getSourceData(sourceName);
            Map<Date, List<String>> sortedUris = sourceData.getAvailableUris();

            List<String> list = new LinkedList<String>();
            Date prevRefTime = null;
            for (FFMPRecord rec : entry.getValue()) {
                if (!isMosiac && !rec.getDataKey().equals(dataKey)) {
                    continue;
                }
                Date curRefTime = rec.getDataTime().getRefTime();
                if ((prevRefTime != null) && !prevRefTime.equals(curRefTime)) {
                    sortedUris.put(prevRefTime, list);
                    list = new LinkedList<String>();
                }
                prevRefTime = curRefTime;
                list.add(rec.getDataURI());
            }

            if (prevRefTime != null) {
                sortedUris.put(prevRefTime, list);
            }

            Date prevTime = time;
            if (sourceXml.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                long timeOffset = sourceXml.getExpirationMinutes(siteKey)
                        * TimeUtil.MILLIS_PER_MINUTE;
                prevTime = new Date(time.getTime() - timeOffset);
            }
            Date oldPrevTime = sourceData.getPreviousUriQueryDate();
            if (oldPrevTime == null || prevTime.before(oldPrevTime)) {
                sourceData.setPreviousUriQueryDate(prevTime);
            }
        }
    }
    
    /**
     * Process an individual URI
     * 
     * @param isProductLoad
     * @param uri
     * @param siteKey
     * @param sourceName
     * @param barrierTime
     * @throws Exception
     */
    public void processUri(String uri, String siteKey, String sourceName,
            Date barrierTime) throws Exception {
        if (uri != null) {

            FFMPRecord record = populateFFMPRecord(uri, siteKey, sourceName);
            if (record != null) {
                record.getBasinData().loadNow();
                SourceXML source = fscm.getSource(sourceName);
                if (source != null) {
                    record.setExpiration(source.getExpirationMinutes(siteKey));
                    record.setRate(source.isRate());
                }
            }
        }
    }

    /**
     * fire off a cleaner
     * 
     * @param product
     * @param source
     * @param siteKey
     * @param date
     */
    public void purgeFFMPData(ProductXML product, String source,
            String siteKey, Date date) {
        
        ArrayList<String> purgeSources = new ArrayList<String>();

        if (product != null) {

            ProductRunXML productRun = frcm.getProduct(siteKey);

            // guidance
            for (String type : productRun.getGuidanceTypes(product)) {
                for (SourceXML guidSource : productRun.getGuidanceSources(
                        product, type)) {
                    // Don't purge archive guidance!
                    if (guidSource != null
                            && guidSource.getGuidanceType() != null
                            && !guidSource.getGuidanceType()
                                    .equals(GUIDANCE_TYPE.ARCHIVE
                                            .getGuidanceType())) {
                        purgeSources.add(guidSource.getSourceName());
                    }
                }
            }

            // qpf
            for (String type : productRun.getQpfTypes(product)) {
                for (SourceXML qpfSource : productRun.getQpfSources(
                        product, type)) {
                    if (qpfSource != null) {
                        purgeSources.add(qpfSource.getSourceName());
                    }
                }
            }
            // qpe, etc
            for (String sourceName : product.getSources()) {
                if (!purgeSources.contains(sourceName)) {
                    purgeSources.add(sourceName);
                }
            }

            for (String sourceName : purgeSources) {

                SourceXML sourceXML = fscm.getSource(sourceName);

                if (sourceXML != null) {
                    if (sourceXML.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                        sourceName = SOURCE_TYPE.GUIDANCE.getSourceType();
                    }

                    if (siteDataMap != null) {
                        if (siteDataMap.containsSite(siteKey)) {
                            FFMPRecord record = siteDataMap.get(siteKey)
                                    .getSourceData(sourceName).getRecord();
                            if (record != null) {
                                record.purgeData(date);
                            }
                        }
                    }
                }
            }

        } else {
            if (siteDataMap != null) {
                if (siteDataMap.containsSite(siteKey)) {
                    FFMPRecord record = siteDataMap.get(siteKey)
                            .getSourceData(source).getRecord();
                    if (record != null) {
                        record.purgeData(date);
                    }
                }
            }
        }

        FFMPSiteData siteData = siteDataMap.get(siteKey);
        for (String sourceEntry : siteData.getSources()) {
            ConcurrentNavigableMap<Date, List<String>> oldUris = siteData
                    .getSourceData(sourceEntry).getAvailableUris()
                    .headMap(date);
            for (List<String> uris : oldUris.headMap(date).values()) {
                for (String uri : uris) {

                    if (product != null) {

                        for (String sourceName : purgeSources) {

                            if (siteData.getSourceData(sourceName)
                                    .hasLoadedAnyUris()) {
                                FFMPSourceData sourceData = siteData
                                        .getSourceData(sourceName);

                                sourceData.removeLoadedUri(uri);
                            }
                        }
                    } else {
                        FFMPSourceData sourceData = siteData
                                .getSourceData(source);

                        sourceData.removeLoadedUri(uri);
                    }
                }
            }

            oldUris.clear();
        }
    }
    
    /**
     * Clear site Data
     */
    public void clear() {
        siteDataMap.clear();
    }
    
    /**
     * Remove a site.
     * @param siteKey
     */
    public void removeSite(String siteKey) {
        siteDataMap.removeSite(siteKey);
    }
    
    /**
     * Get the FFMP Data for this site.
     * 
     * @param siteKey
     * @return
     */
    public FFMPSiteData getFFMPSiteData(String siteKey) {
        return siteDataMap.get(siteKey);
    }
    
   /**
    * Gets the source data for this source.
    * 
    * @param siteKey
    * @param sourceName
    * @return
    */
    public FFMPSourceData getSourceData(String siteKey, String sourceName) {
        return siteDataMap.get(siteKey).getSourceData(sourceName);
    }  
    
}
